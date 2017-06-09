(cl:in-package #:jenkins.target.dockerfile)

(defmethod aspects:extend! ((aspect t)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  output)

;;; TODO duplicated in model/aspects/protocol.lisp
(defmethod aspects:extend! ((aspect list)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  ;; Apply aspects, respecting declared ordering, and sort generated
  ;; steps (i.e. builders and publishers) according to declared
  ;; ordering.
  (let ((aspects::*step-constraints* '())
        (aspects (model::sort-with-partial-order
                  (copy-list aspect) #'aspects:aspect<)))

    ;; Methods on `extend!' add entries to `*step-constraints*' and
    ;; push builders onto (builders job).
    (reduce (lambda (output aspect)
              (aspects:extend! aspect spec output target))
            aspects :initial-value output)

    (let ((constraints (aspects::constraints-table 'aspects::build))
          (unsorted    (builders output)))
      (when unsorted
        (log:trace "~@<~@(~A~)er constraint~P:~@:_~
                      ~@<~{• ~{~
                        ~A ~A:~A ~@:_~
                        ~2@T~@<~/jenkins.model.aspects:format-constraints/~@:>~
                      ~}~^~@:_~}~@:>~
                    ~@:>"
                   'aspects::build
                   (hash-table-count constraints)
                   (hash-table-alist constraints))

        ;; Try to sort steps according to CONSTRAINTS.
        (let ((sorted (model::sort-with-partial-order
                       unsorted (rcurry #'aspects::step< constraints))))
          (log:debug "~@<Sorted builder~P:~@:_~
                      ~@<~{• ~A~^~@:_~}~@:>~@:>"
                      (length sorted) sorted)
          (setf (builders output) sorted)))))

  output)

;;;

(defmethod aspects:extend! ((aspect aspects::aspect-archive)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (let ((step (shell-command
               aspect (aspects:extend! aspect spec 'string :command))))
    (aspects::register-constraints aspect 'build step 'aspects::archive '((:before t)))
    (push step (builders output))))

(defmethod aspects:extend! ((aspect aspects::aspect-git)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (catch 'aspects::%bail
    (apply
     (lambda (url username password credentials branches local-branch
              clone-timeout wipe-out-workspace? clean-before-checkout?
              checkout-submodules? shallow? sub-directory)
       (let* ((command (with-output-to-string (stream)
                         (aspects:extend! aspect spec stream :command)
                         (when sub-directory
                           (aspects:extend! aspect spec stream :sub-directory-command))))
              (step    (shell-command aspect command)))
         (aspects::register-constraints aspect 'build step 'git '())
         (push step (builders output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-shell)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (catch 'aspects::%bail
    (apply
     (lambda (command)
       (let ((step (shell-command aspect command)))
         (aspects::register-constraints aspect 'build step 'shell '())
         (push step (builders output))))
     (aspects::aspect-process-parameters aspect)))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-cmake/unix)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (catch 'aspects::%bail
    (let ((step (shell-command
                 aspect (aspects:extend! aspect spec 'string :command))))
      (aspects::register-constraints aspect 'build step 'cmake/unix '())
      (push step (builders output))))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-maven)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (catch 'aspects::%bail
    (let ((step (shell-command
                 aspect (aspects:extend! aspect spec 'string :command))))
      (aspects::register-constraints aspect 'build step 'maven '())
      (push step (builders output))))
  output)

(defmethod aspects:extend! ((aspect aspects::aspect-setuptools)
                            (spec   t)
                            (output dockerfile-job)
                            (target (eql :dockerfile)))
  (catch 'aspects::%bail
    (let ((step (shell-command
                 aspect
                 (aspects:extend! aspect spec 'string :command))))
      (aspects::register-constraints aspect 'build step 'setuptools '())
      (push step (builders output))))
  output)
