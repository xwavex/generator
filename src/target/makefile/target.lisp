(cl:in-package #:jenkins.target.makefile)

;;; `makefile-target'

(defclass makefile-target ()
  ((%output-directory :initarg :output-directory
                      :reader  output-directory)))

;;; `project-rules'

(defclass project-rules (model:implementation-mixin
                         aspects::aspect-builder-defining-mixin)
  ((%directory :initarg  :directory
               :reader   directory)
   (%rules     :initarg  :rules
               :accessor rules
               :initform '()))
  (:documentation
   "A collection of `rule' instances for one project."))

(defun make-project-rules (specification directory)
  (make-instance 'project-rules
                 :directory     directory
                 :specification specification))

;;; `rule'

(defclass rule (print-items:print-items-mixin)
  ((%name         :initarg  :name
                  :reader   name)
   (%dependencies :initarg  :dependencies
                  :accessor dependencies
                  :initform '())
   (%command      :initarg  :command
                  :reader   command)
   (%early?       :initarg  :early?
                  :type     boolean
                  :reader   early?
                  :initform nil
                  :documentation
                  "Controls whether the rule can be executed
                   \"early\", that is disregarding inter-project
                   dependencies.")
   ;; HACK
   (%builder-class :initarg :builder-class
                   :reader builder-class
                   :initform nil))
  (:default-initargs
   :name    (error "name")
   :command (error "command")))

(defun make-rule (name command &key (dependencies '()) early? builder-class)
  (make-instance 'rule :name          name
                       :command       command
                       :dependencies  dependencies
                       :early?        early?
                       :builder-class builder-class))

(defmethod print-items:print-items append ((object rule)) ; TODO duplicated in deploy-dockerfile.lisp
  (let+ ((command (string-left-trim '(#\Space #\Tab #\Newline)
                                    (command object)))
         (length  (length command))
         (end     (min (or (position #\Newline command) length) 30))
         ((&values command shortened?)
          (if (> length end)
              (values (subseq command 0 end) t)
              (values command               nil))))
    `((:name    ,(name object)             "~A")
      (:command ,(list command shortened?) " ~{\"~A~:[~;…~]\"~}" ((:after :name))))))

(defmethod aspects::step-constraints ((aspect aspects::aspect-builder-defining-mixin)
                                      (phase  (eql 'aspects::build))
                                      (step   rule))
  (when-let ((builder-class (builder-class step)))
    (let* ((variable        (let ((*package* (find-package '#:keyword)))
                              (symbolicate  '#:aspect.builder-constraints.
                                            builder-class)))
           (constraints/raw (var:value aspect variable nil))
           (constraints     (mapcar #'aspects::parse-constraint constraints/raw)))
      (log:debug "~@<Constraints for ~A in ~A~:@_~
                  ~/aspects::format-constraints/~@:>"
                 step variable constraints)
      constraints)))

(defun make-ensure-directory-rule (directory)
  (make-rule "ensure-directory" (format nil "mkdir -p '~A'" directory)))
;;;

(defun write-rule (stream name &key dependencies directory command comment)
  (let ((rule-name (safe-name name))
        (log-file  (format nil "~A.log" (safe-name name)))
        (prefix    (string #\Tab)))
    ;; Write command and rule head.
    (format stream "~@[# ~A~%~]~
                    ~A:~{ ~A~}~@
                    "
            comment rule-name (map 'list #'safe-name dependencies))
    ;; Write rule body (called "recipe" in the make documentation).
    (when command
      (pprint-logical-block (stream (list command) :per-line-prefix prefix)
        (format stream "@~
                        echo 'Executing ~A'~@
                        (~@
                          set -e~@
                          ~@[cd '~A'~@:_~]~
                          ~@
                          ~A~@:_~
                        ) > '~A' 2>&1~@
                        if [ $$? -ne 0 ] ; then~@
                        ~2@Tcat '~:*~A'~@
                        ~2@Texit 1~@
                        fi~@
                        touch '~4:*~A'"
                rule-name directory (escape-dollars command) log-file))
      (terpri stream))
    (terpri stream)))

(defmethod project::deploy ((thing project::job) (target (eql :makefile)))
  (let* ((directory (job->full-name thing))
         (output    (make-project-rules thing directory)))
    (push output (model:implementations thing))

    ;; Apply aspects, respecting declared ordering, and sort generated
    ;; builders according to declared ordering.
    (aspects:extend! (project:aspects thing) thing output target)

    output))

(defun write-project-rules (stream thing)
  (let+ ((specification        (model:specification thing))
         (name                 (job->full-name specification))
         (directory            (directory thing))
         (rules                (rules thing))
         (project-dependencies (map 'list #'job->full-name
                                    (project::direct-dependencies
                                     specification)))
         (ensure-directory     (make-ensure-directory-rule
                                directory))
         ((&flet rule-name (rule)
            (format nil "~A-~A" name (name rule)))))
    ;; Header/separator
    (heading stream name)

    ;; Preparation rule
    (write-rule stream (rule-name ensure-directory)
                :command (command ensure-directory))

    ;; Actual rules
    (map nil (lambda (rule)
               (with-simple-restart
                   (continue "~@<Skip ~A~@:>" rule)
                 (let ((dependencies (append
                                      (map 'list #'rule-name
                                           (list* ensure-directory
                                                  (dependencies rule)))
                                      (unless (early? rule)
                                        project-dependencies))))
                   (write-rule stream (rule-name rule)
                               :dependencies dependencies
                               :directory    directory
                               :command      (command rule)))))
         rules)

    ;; Interface rule
    (write-rule stream name :dependencies (map 'list #'rule-name rules))
    name))

(defmethod project::finish-deploy ((distributions   sequence)
                                   (deployed-things sequence)
                                   (target          makefile-target))
  (let ((makefile        (merge-pathnames "Makefile" (output-directory target)))
        (project-rules   '())
        (interface-rules '()))
    ;; Generate rule text for all projects and collect the names of
    ;; interface rules.
    (map nil (lambda (thing)
               (with-simple-restart (continue "~@<Skip ~A~@:>" thing)
                 (let* ((stream         (make-string-output-stream))
                        (interface-rule (write-project-rules stream thing)))
                   (push (get-output-stream-string stream) project-rules)
                   (push interface-rule interface-rules))))
         deployed-things)

    ;; Write the Makefile.
    (ensure-directories-exist makefile)
    (with-output-to-file (stream makefile :if-exists :supersede)

      ;; Execute the recipe lines of each rule as a single shell chunk
      ;; of shell instead of one shell invocation per line (or
      ;; multiple continuation lines using "\"). Note that we have to
      ;; pass -c to the shell so it doesn't attempt to execute the
      ;; shell code chunk as a command.
      (format stream ".ONESHELL:~@
                      SHELL = /bin/bash~@
                      .SHELLFLAGS = -c~@
                      ~2%")

      ;; Add an "all" rule for convenience.
      (write-rule stream "all" :dependencies interface-rules)

      ;; Write project rules.
      (format stream "~{~A~^~2%~}" project-rules))))
