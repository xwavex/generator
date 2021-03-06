;;;; archive.lisp --- Access project that are distributed as archives.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.analysis)

(defun open-download-stream (url &key username password)
  (let+ (((&values stream code headers &ign &ign close-stream? reason)
          (apply #'drakma:http-request url
                 :want-stream  t
                 :force-binary t
                 :verify       nil
                 (when (and username password)
                   (list :basic-authorization (list username password))))))
    (unless (<= 200 code 299)
      (error "~@<Download from ~A failed with code ~D: ~A.~@:>"
             url code reason))
    (let ((length (when-let ((length (assoc-value headers :content-length)))
                    (parse-integer length))))
      (values stream close-stream? length))))

(defun call-with-download-stream (thunk url &rest args &key username password)
  (declare (ignore username password))
  (let+ (((&values stream close-stream? content-length)
          (apply #'open-download-stream url args)))
    (unwind-protect
         (funcall thunk stream content-length)
      (when close-stream?
        (close stream)))))

(defconstant +archive-hash-length-limit+ 65536)

(defun digest-stream-head (url input-stream &key content-length output-stream)

  (log:debug "~@<Content length for ~A is ~:[not known~;~:*~:D~]~@:>"
             url content-length)
  (let* ((digest     (ironclad:make-digesting-stream :sha512))
         (end        (if content-length
                         (min content-length +archive-hash-length-limit+)
                         +archive-hash-length-limit+))
         (end-length (integer-length end)))
    ;; Feed length of content length and content length to digest.
    (write-byte end-length digest)
    (loop :for i :below end-length :by 8
          :for octet = (ldb (byte 8 i) end)
          :do (write-byte octet digest))
    ;; Feed beginning of content to digest.
    (let ((stream (if output-stream
                      (make-broadcast-stream output-stream digest)
                      digest)))
      (copy-stream input-stream stream :end end))
    (ironclad:produce-digest digest)))

(macrolet ((define-download-function
               (name (stream-var &optional content-length-var) &body body)
             (let+ (((&values content-length-var content-length-used?)
                     (if content-length-var
                         (values content-length-var t)
                         (values (gensym)           nil))))
               `(defun ,name (url output-file &rest args &key username password)
                  (declare (ignore username password))
                  (util:with-retries (usocket:ns-try-again-condition :limit 3)
                    (util:with-retry-restart ("Retry downloading ~A" url)
                      (apply #'call-with-download-stream
                             (lambda (,stream-var ,content-length-var)
                               ,@(when (not content-length-used?)
                                   `((declare (ignore ,content-length-var))))
                               ,@body)
                             url args)))))))

  (define-download-function download-file/hash-head (stream content-length)
    ;; In order to download the file and obtain the hash in a single
    ;; pass, copy the first `+archive-hash-length-limit+' bytes from
    ;; STREAM into both, OUTPUT and DIGEST. Then copy the remainder
    ;; into OUTPUT only.
    (with-output-to-file (output output-file :element-type '(unsigned-byte 8))
      (prog1
          (digest-stream-head url stream :content-length content-length
                                         :output-stream  output)
        (copy-stream stream output))))

  (define-download-function download-file (stream)
    (with-output-to-file (output output-file :element-type '(unsigned-byte 8))
      (copy-stream stream output))))

(defun archive-remote-hash (url &rest args &key username password)
  (declare (ignore username password))
  (with-condition-translation (((error repository-access-error)
                                :specification url))
    (util:with-retries (usocket:ns-try-again-condition :limit 3)
      (util:with-retry-restart ("Retry obtaining hash for ~A" url)
        (apply #'call-with-download-stream
               (lambda (stream content-length)
                 (digest-stream-head
                  url stream :content-length content-length))
               url args)))))

(defun download-and-extract (source temp-directory
                             &key username password sub-directory)
  (let* ((archive-name (lastcar (puri:uri-parsed-path source)))
         (temp-file    (merge-pathnames archive-name temp-directory))
         (content-hash ;; Download into temporary archive file inside
                       ;; temporary directory.
                       (with-trivial-progress (:download "~A" source)
                         (download-file/hash-head source temp-file
                                                  :username username
                                                  :password password))))
    (log:info "~@<Downloaded ~A into ~A, content hash ~
               ~(~{~2,'0X~}~).~@:>"
              source temp-file (coerce content-hash 'list))
    ;; Extract temporary file, producing a single directory if all
    ;; goes well. Delete temporary archive file afterwards.
    (with-trivial-progress (:extract "~A" temp-file)
      (inferior-shell:run/nil `("unp" "-U" ,temp-file)
                              :directory temp-directory)
      (delete-file temp-file))
    ;; Locate the expected singleton directory and run analysis on it.
    (let* ((directory (or (first (directory (merge-pathnames
                                             "*.*" temp-directory)))
                          (error "~@<Cannot locate directory ~
                                  extracted from ~A in ~A.~@:>"
                                 archive-name temp-directory)))
           (directory     (if sub-directory
                              (merge-pathnames sub-directory directory)
                              directory)))
      (values directory content-hash))))

(defun call-with-extracted-archive (thunk source temp-directory
                                    &rest args
                                    &key username password sub-directory)
  (declare (ignore username password sub-directory))
  (unwind-protect
       (let+ (((&values directory content-hash)
               (with-condition-translation (((error repository-access-error)
                                             :specification source))
                 (apply #'download-and-extract source temp-directory args))))
         (funcall thunk directory content-hash))
    ;; Delete everything when done or if something goes wrong.
    (uiop:delete-directory-tree
     temp-directory :if-does-not-exist :ignore :validate (constantly t))))

(defmacro with-extracted-archive (((directory hash)
                                   (source temp-directory
                                    &key
                                    username
                                    password
                                    sub-directory))
                                  &body body)
  `(call-with-extracted-archive
    (lambda (,directory ,hash)
      ,@body)
    ,source ,temp-directory
    :username ,username :password ,password :sub-directory ,sub-directory))

(defun %archive-key (hash)
  (octets->hex-string hash "archive"))

(defun archive-analyze-version/maybe-cached (version content-key
                                             &key
                                             extract-directory
                                             cache-directory
                                             sub-directory
                                             natures)
  (let* ((version-args  version)
         (sub-directory (getf version-args :sub-directory sub-directory))
         (natures       (getf version-args :natures       natures))
         (key           (natures->key
                         natures (sub-directory->key
                                  sub-directory (%archive-key content-key)))))
    (cache-or-compute cache-directory key
                      (lambda ()
                        (unless extract-directory
                          (log:info "~@<Need extracted archive, ~
                                     restarting.~@:>")
                          (throw 'not-extracted nil))
                        (list* :scm              :archive
                               :branch-directory nil
                               (apply #'analyze extract-directory :auto
                                      version-args))))))

(defmethod analyze ((source puri:uri) (kind (eql :archive))
                    &key
                    username
                    password
                    (versions       (missing-required-argument :versions))
                    sub-directory
                    cache-directory
                    (temp-directory (util:default-temporary-directory)))
  (flet ((analyze-versions (content-key &key extract-directory)
           (with-sequence-progress (:analyze/version versions)
             (mapcan
              (progressing
               (lambda (version)
                 (with-simple-restart
                     (continue "~@<Ignore ~A and continue with the next ~
                                version.~@:>"
                               version)
                   (list (archive-analyze-version/maybe-cached
                          version content-key
                          :extract-directory extract-directory
                          :cache-directory   cache-directory
                          :sub-directory     sub-directory))))
               :analyze/version)
              versions))))
    ;; First, try to download and hash a small prefix of the remote
    ;; file. Perform a cache lookup using the resulting hash.
    (when cache-directory
      (with-simple-restart (continue "~@<Do not try to use cached ~
                                      analysis results.~@:>")
        (log:info "~@<Determining remote content hash in ~A~@:>" source)
        (let ((content-key (archive-remote-hash source :username username
                                                       :password password)))
          (log:info "~@<Got remote content hash ~(~{~2,'0X~}~)~@:>"
                    (coerce content-key 'list))
          ;; This is thrown when there are no cached results for at
          ;; least one version.
          (catch 'not-extracted
            (return-from analyze (analyze-versions content-key))))))
    ;; If CACHE-DIRECTORY is NIL and/or results for at least one
    ;; version could not be restored from the cache, download and
    ;; extract the archive and re-run the analysis. Already cached
    ;; results can be re-used.
    (with-simple-restart (continue "~@<Give up analyzing ~A.~@:>" source)
      (with-extracted-archive ((extract-directory content-key)
                               (source temp-directory
                                       :username      username
                                       :password      password
                                       :sub-directory sub-directory))
        ;; Note that SUB-DIRECTORY has been taken care of in
        ;; EXTRACT-DIRECTORY.
        (analyze-versions content-key :extract-directory extract-directory)))))
