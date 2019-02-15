(cl:in-package #:jenkins.target.makefile)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  output)

(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let+ ((aspects::*step-constraints* '())
         (aspects (aspects::sort-with-partial-order (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (builders job).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (rules       (rules output)))
      (when rules
        (log:debug "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/jenkins.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build (hash-table-count constraints)
                   (hash-table-alist constraints))

        (map nil (lambda (step)
                   (setf (dependencies step)
                         (remove-if-not (rcurry #'aspects::step< step constraints)
                                        rules)))
             rules))))

  output)

;;;

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (let* ((command (aspects:extend! aspect spec 'string :command))
         (step    (make-rule (model:name aspect) command :early? t)))
    (aspects::register-constraints aspect 'aspects::build step 'aspects::archive '((:before t)))
    (push step (rules output)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (catch 'aspects::%bail
    (apply
     (lambda (url username password credentials branches local-branch
              clone-timeout wipe-out-workspace? clean-before-checkout?
              checkout-submodules? shallow? sub-directory)
       (let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (when sub-directory
                           (aspects:extend! aspect spec stream :sub-directory-command))))
              (step    (make-rule (model:name aspect) command :early? t
                                  :builder-class 'aspects::git)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::git '())
         (push step (rules output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-shell)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (catch 'aspects::%bail
    (apply
     (lambda (command)
       (let ((step (make-instance 'rule
                                  :name    (model:name aspect)
                                        ; :aspect  aspect
                                  :command command
                                  :builder-class 'aspects::shell)))
         (aspects::register-constraints aspect 'aspects::build step 'aspects::shell '())
         (push step (rules output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-cmake/unix)
                            (spec   t)
                            (output project-rules)
                            (target (eql :makefile)))
  (catch 'aspects::%bail
    (let ((step (make-instance 'rule
                               :name    (model:name aspect)
                                        ; :aspect  aspect
                               :command (aspects:extend! aspect spec 'string :command)
                               :builder-class 'aspects::cmake/unix)))
      (aspects::register-constraints aspect 'aspects::build step 'aspects::cmake/unix '())
      (push step (rules output))))
  output)
