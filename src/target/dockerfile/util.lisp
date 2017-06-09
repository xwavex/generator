(cl:in-package #:jenkins.target.dockerfile)

;;; Names

(defun safe-name (name)            ; TODO duplicated in jenkins.report
  (substitute #\_ #\/ name))

(defun job->full-name (thing)
  (let* ((version  (model:parent thing))
         (project  (model:parent (model:specification version))))
   (format nil "~A@~A"
           (model:name project)
           (model:name version))))

;;; Commands

(defun trim-command (command)
  (string-trim '(#\Space #\Tab #\Newline) command))

;;; Comments

(defun heading (stream title &key (width 80))
  (format stream "##~V,,,'#<~>##~@
                  # ~:*~V<~A~;~> #~@
                  ~2:*##~V,,,'#<~>##~2%"
          width title))
