;;;; context.lisp --- Context contributors for different recipe types.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.language-server)

;;; Utilities

(defun structure-path (position document)
  (when-let* ((locations (lookup:lookup position (index document))))
    (let ((path (mappend
                 (lambda (node)
                   (when (typep node '(cons keyword))
                     (list (car node))))
                 (map 'list (lambda (location)
                              (project::object-at
                               location (locations document)))
                      locations))))
      ;; TODO removing duplicates should not be necessary
      (values (remove-duplicates path :test #'equal) locations))))

;;; `context'

(defclass context ()
  ((%location :initarg :location
              :reader  location)
   (%word     :initarg :word
              :accessor %word
              :initform nil))
  (:default-initargs
   :location (more-conditions:missing-required-initarg 'context :location)))

(defmethod word ((object context))
  (or (%word object)
      (setf (%word object) (sloc:content (location object)))))

;; TODO print-items for context

;;; `structure-context'

(defclass structure-context (context
                             print-items:print-items-mixin)
  ((%path :initarg :path
          :reader  path)))

(defmethod print-items:print-items append ((object structure-context))
  `((:path ,(reverse (path object)) "~{~A~^ » ~}")))

(defclass structure-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor structure-context-contributor))
  (when-let* ((path (structure-path position document)))
    (list (make-instance 'structure-context
                         :location (first (lookup:lookup position (index document))) ; TODO repeated work in `structure-path'
                         :path     path))))

;;; Template name context

(defclass template-name-context (context)
  ())

(defclass template-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    project-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document)) ; TODO produces location
              (depth (position :templates path)))
    (when (eql depth 0)
      (let ((location (first (lookup:lookup position (index document)))))
        (list (make-instance 'template-name-context :location location))))))

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    template-document)
     (position    t)
     (contributor template-name-context-contributor))
  (when-let* ((path  (structure-path position document))
              (depth (position :inherit path)))
    (when (eql depth 0)
      (list (make-instance 'template-name-context
                           :location (first (lookup:lookup position (index document))))))))

;;; Variable name context

(defclass variable-name-context (context
                                 print-items:print-items-mixin)
  ((%prefix       :initarg :prefix
                  :reader  prefix)
   (%prefix-range :initarg :prefix-range
                  :reader  prefix-range)))

(defmethod print-items:print-items append ((object variable-name-context))
  `((:prefix ,(prefix object) "~S")))

(defclass variable-reference-context (variable-name-context)
  ((%kind :initarg :kind
          :reader  kind)))

(defclass variable-name-context-contributor ()
  ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-name-context-contributor))
  (when-let* ((locations (lookup:lookup position (index document)))
              (location  (first locations))
              (leaf      (project::object-at location (locations document)))
              (leaf      (when (stringp leaf)
                           leaf))
              (path      (structure-path position document))
              (position* (position :variables path)))
    (cond ;; NAME: VALUE
          ;;   ^
          ((and (= position* 1)
                (string-equal leaf (first path))) ;; TODO could be deeper within dictionary value
           (list (make-instance 'variable-name-context
                                :location     location
                                :prefix       (string leaf)
                                :prefix-range (sloc:range location))))

          ;; NAME: … ${NAME} …
          ;;           ^
          ((stringp leaf)
           (when-let* ((base     (sloc:index (sloc:start location)))
                       (relative (- (sloc:index position) base))
                       (start    (search "${" leaf :end2 (1+ relative) :from-end t))
                       (end      (if-let ((end (search "}" leaf :start2 start)))
                                   (1+ end)
                                   (length leaf))))
             (list (make-instance 'variable-reference-context
                                  :location     location
                                  :prefix       leaf
                                  :prefix-range (sloc:make-range
                                                 (+ base start) (+ base end)
                                                 (lsp:text document))
                                  :kind         :scalar)))))))

;;;

(defclass variable-value-context (context)
  ((%variable-location :initarg :variable-location
                       :reader  variable-location)

   (%prefix-range      :initarg :prefix-range
                       :reader  prefix-range)))

(defclass unknown-variable-value-context (variable-value-context)
  ((%variable-name :initarg :variable-name
                   :reader  variable-name)))

(defclass known-variable-value-context (variable-value-context)
  ((%variable-node :initarg :variable-node
                   :reader  variable-node)))

(defmethod variable-name ((context known-variable-value-context))
  (var:variable-info-name (variable-node context)))

(defclass variable-value-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace    t)
     (document     t)
     (position     t)
     (contriubutor variable-value-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document)))
    (when-let* ((position (position :variables path))
                (name     (when (plusp position)
                            (nth (1- position) path))))
      (list (if-let ((variable (var:find-variable name :if-does-not-exist nil)))
              (make-instance 'known-variable-value-context
                             :location          location
                             :variable-location location ; TODO
                             :variable-node     variable
                                        ; :prefix-range
                             )
              (make-instance 'unknown-variable-value-context
                             :location          location
                             :variable-location location
                             :variable-name     name))))))

;;; Project version reference context

(defclass project-name-context (context
                                print-items:print-items-mixin)
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defmethod print-items:print-items append ((object project-name-context))
  `((:prefix ,(prefix object) "~S")))

(defclass project-version-context () ())

(defclass project-version-reference-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor project-version-reference-context-contributor))
  (let+ (((&values path locations)
          (structure-path position document))
         (thing (loop :for location :in locations
                      :for thing = (project::object-at location (locations document))

                      :when (stringp thing)
                      :do (log:error location thing)
                      :and :return thing)))
    (when-let ((position (position :versions path)))
      (cond ;; versions:
            ;; - PROJECT-NAME
            ;;     ^
            ((and (= position 0) (stringp thing) (not (find #\@ thing)))
             (list (make-instance 'project-name-context
                                  :location (first locations)
                                  :prefix   thing)))

            ;; versions:
            ;; - PROJECT-NAME@…
            ;;     ^
            ((and (= position 0) (stringp thing) (find #\@ thing))
             (list (make-instance 'project-name-context
                                  :location (first locations)
                                  :prefix   (string-trim " " (subseq thing 0 (position #\@ thing))))))))))

;;; Aspect class context

(defclass aspect-class-context (context)
  ((%prefix :initarg :prefix
            :reader  prefix)))

(defclass aspect-class-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor aspect-class-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (ends-with-subseq '(:aspect :aspects) path)
      (list (make-instance 'aspect-class-context
                           :location location
                           :prefix   thing)))))

;;; System package name context

(defclass system-package-name-context (context)
  (#+no (%prefix :initarg :prefix
            :reader  prefix)))

(defclass system-package-name-context-contributor () ())

(defmethod contrib:context-contributions
    ((workspace   t)
     (document    t)
     (position    t)
     (contributor system-package-name-context-contributor))
  (let+ (((&values path (&optional location &rest &ign))
          (structure-path position document))
         (thing (project::object-at location (locations document))))
    (when (and (search '(:platform-requires :variables) path)
               (starts-with :packages path))
      (list (make-instance 'system-package-name-context
                           :location location
                           ; :prefix   thing
                           )))))
