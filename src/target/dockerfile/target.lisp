(cl:in-package #:jenkins.target.dockerfile)

(defclass dockerfile-target ()
  (;; Configuration
   (output-directory :initarg  :output-directory
                     :reader   output-directory
                     :initform "/tmp/distribution-docker-build/")
   (base-image       :initarg  :base-image
                     :reader   base-image
                     :initform "debian:latest")
   (platform         :initarg  :platform
                     :reader   platform
                     :initform '("ubuntu" "xenial"))))

(defun write-header-comment (stream target)
  (declare (ignore target))
  (heading stream "TODO steal from distribution release command"))

(defun write-package-installation-commands (stream distribution target)
  (let* ((platform     (platform target))
         (requirements (project:platform-requires distribution platform))
         (apt-command  "DEBIAN_FRONTEND=noninteractive apt-get -qq"))
    (heading stream (format nil "Dependencies for platform ~{~A~^ ~}"
                            platform))
    (format stream "RUN ~A update \\~@
                    ~4T&& ~:*~A --assume-yes upgrade \\~@
                    ~4T&& ~:*~A --assume-yes install \\~@
                     ~6@T~{~<~T\\~%~6@T~1,:;~A~>~^ ~}~
                     ~2%"
            apt-command requirements)))

(defun write-cleanup-commands (stream)
  (heading stream "Cleanup")
  #+later (format stream "RUN ~%"))

(defmethod project::finish-deploy ((distributions   t)
                                   (deployed-things t)
                                   (target          dockerfile-target))
  (let ((deployed-things (model::sort-with-partial-order
                          deployed-things (lambda (left right)
                                            (find (model:specification left)
                                                  (model:dependencies
                                                   (model:specification right))))))
        (dockerfile      (merge-pathnames "Dockerfile" (output-directory target))))
    (ensure-directories-exist dockerfile)
    (with-output-to-file (stream dockerfile :if-exists :supersede)
      ;; Header comment and base image.
      (write-header-comment stream target)
      (format stream "FROM ~A~2%" (base-image target))

      ;; Install build and runtime dependencies.
      (write-package-installation-commands
       stream (first distributions) target)

      ;; Write scripts and RUN directives for prepare hook(s).
      (map nil (lambda (distribution) ; TODO could we get this into DEPLOYED-THINGS?
                 (with-simple-restart
                     (continue "~@<Skip writing RUN commands for ~A ~
                                prepare hook~@:>"
                               distribution)
                   (when-let* ((name         (model:name distribution))
                               (prepare-hook (var:value distribution :prepare-hook/unix nil)))
                     (heading stream (format nil "~A Prepare Hook" name))
                     (write-scripts-and-run-commands*
                      stream target "distribution-prepare"
                      `((,(format nil "~A-prepare-hook" name)
                         "Prepare Hook"
                         ,prepare-hook)))
                     (format stream "~2%"))))
           distributions)

      ;; Write scripts and RUN directives for project builders.
      (map nil (lambda (thing)
                 (with-simple-restart
                     (continue "~@<Skip writing RUN commands for ~A~@:>" thing)
                   (heading stream (model:name thing))
                   (write-scripts-and-run-commands
                    target stream thing :one-file-for-all-builders)
                   (format stream "~2%")))
           deployed-things)

      ;; TODO Optionally delete workspaces
      ;; TODO Uninstall build dependencies
      (write-cleanup-commands stream))))

(defclass dockerfile-job (model:named-mixin
                          model:implementation-mixin
                          aspects::aspect-builder-defining-mixin)
  ((builders :accessor builders
             :initform '())))

(defclass shell-command (print-items:print-items-mixin)
  ((aspect  :initarg :aspect
            :reader  aspect)
   (command :initarg :command
            :reader  command)))

(defun shell-command (aspect format-control &rest format-arguments)
  (make-instance 'shell-command
                 :aspect  aspect
                 :command (apply #'format nil format-control format-arguments)))

(defmethod print-items:print-items append ((object shell-command))
  (let+ ((command (command object))
         (length  (length command))
         (end     (min (or (position #\Newline command) length) 30))
         ((&values command shortened?)
          (if (> length end)
              (values (subseq command 0 end) t)
              (values command               nil))))
    `((:command ,(list command shortened?) "~{\"~A~:[~;…~]\"~}"))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   shell-command))
  (let* ((variable        :aspect.builder-constraints.shell)
         (constraints/raw (var:value aspect variable nil))
         (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
    (log:trace "~@<Constraints for ~A in ~A~:@_~
                ~/aspects::format-constraints/~@:>"
               step variable constraints)
    constraints))

(defun write-copy-and-run-commands (stream script-directory scripts)
  (format stream "COPY ~{~A~^ ~} /tmp/~A~@
                    ~@
                    ~{~{~
                      # ~A~@
                      RUN mkdir -p \"~A\" \\~@
                      ~4@T&& cd \"~:*~A\" \\~@
                      ~4@T&& sh \"/tmp/~A\"~@
                    ~}~^~2%~}"
          (map 'list #'third scripts) script-directory scripts))

(defun make-script-directory (sub-directory)
  (make-pathname :directory `(:relative "scripts" ,sub-directory)))

(defun make-script-name (name script-directory output-directory)
  (let* ((script/relative (make-pathname :name      (safe-name name)
                                         :type      "sh"
                                         :defaults  script-directory))
         (script/absolute (merge-pathnames script/relative output-directory)))
    (values script/relative script/absolute)))

(defmacro with-output-to-script ((stream-var name script-directory output-directory)
                                 &body body)
  (with-gensyms (relative-var absolute-var)
    `(let+ (((&values ,relative-var ,absolute-var)
             (make-script-name ,name ,script-directory ,output-directory)))
       (ensure-directories-exist ,absolute-var)
       (with-output-to-file (,stream-var ,absolute-var :if-exists :supersede)
         (format ,stream-var "set -e~2%")
         ,@body)
       (values ,relative-var ,absolute-var))))

(defun write-scripts-and-run-commands* (stream target sub-directory steps)
  (let ((output-directory (output-directory target))
        (script-directory (make-script-directory sub-directory))
        (runs             '()))
    (map nil (lambda+ ((name title command))
               (let ((script/relative
                       (with-output-to-script
                           (stream name script-directory output-directory)
                         (write-string command stream))))
                 (appendf runs (list (list title name script/relative)))))
         steps)

    (write-copy-and-run-commands stream script-directory runs)))

(defmethod write-scripts-and-run-commands ((target   t)
                                           (stream   t)
                                           (job      t)
                                           (strategy (eql :one-file-per-builder)))
  (let* ((output-directory  (output-directory target))
         (project-directory (job->full-name (model:specification job)))
         (script-directory  (make-script-directory project-directory))
         (runs              '()))
    (map nil (lambda (builder)
               (let* ((name    (model:name (aspect builder)))
                      (command (trim-command (command builder)))
                      (script/relative
                        (with-output-to-script
                            (stream name script-directory output-directory)
                          (write-string command stream))))
                 (appendf runs (list (list name
                                           project-directory
                                           script/relative)))))
         (builders job))

    (write-copy-and-run-commands stream script-directory runs)))

(defmethod write-scripts-and-run-commands ((target   t)
                                           (stream   t)
                                           (job      t)
                                           (strategy (eql :one-file-for-all-builders)))
  (let* ((output-directory  (output-directory target))
         (project-directory (job->full-name (model:specification job)))
         (script-directory  (make-script-directory project-directory))
         (script/relative
           (with-output-to-script (stream "builders" script-directory output-directory)
             (map nil (lambda (builder)
                        (let ((name    (model:name (aspect builder)))
                              (command (trim-command (command builder))))
                          (heading stream (format nil "Aspect ~A" name)) ; TODO should take format-control &rest format-arguments
                          ;; Execute COMMAND in a sub-shell so that
                          ;; e.g. changing the current directory or
                          ;; the environment does not affect the next
                          ;; step.
                          ;;
                          ;; Note that we must not indent the
                          ;; sub-shell command string as that could
                          ;; break HERE documents and maybe other
                          ;; things.
                          (format stream "(~@
                                           ~@<~@;~A~:>~@
                                          )~2%" command)))
                  (builders job)))))

    (write-copy-and-run-commands
     stream script-directory
     `(("Builders" ,project-directory ,script/relative)))))

(defmethod model:deploy ((thing project::job) (target (eql :dockerfile)))
  (let ((output (make-instance 'dockerfile-job
                               :name          (job->full-name thing)
                               :specification thing)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (project:aspects thing) thing output target)

    output))
