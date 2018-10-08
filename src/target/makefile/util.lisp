(cl:in-package #:jenkins.target.makefile)

(defun safe-name (name) ; TODO duplicated in jenkins.report and dockerfile stuff
  (substitute #\_ #\/ name))

(defun job->full-name (thing)    ; TODO duplicated in dockerfile stuff
  (let* ((version (model:parent thing))
         (project (model:parent (model:specification version))))
    (format nil "~A@~A"
            (model:name project)
            (model:name version))))

(defun heading (stream title &key (width 80)) ; TODO likewise
  (format stream "##~V,,,'#<~>##~@
                  # ~:*~V<~A~;~> #~@
                  ~2:*##~V,,,'#<~>##~2%"
          width title))

(defun escape-dollars (string)
  (with-output-to-string (stream)
    (loop :for char :across string
          :when (char= char #\$)
          :do (write-char #\$ stream)
          :do (write-char char stream))))
