;;;; phases.lisp --- Machinery for phases within commands.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

;;; Error collection

(defun defer (condition &key debug?)
  (when-let ((restart (find-restart 'defer condition)))
    (invoke-restart restart condition :debug? debug?)))

(defun call-with-error-collection (thunk)
  (let+ ((errors      '())
         (errors-lock (bt:make-lock))
         ((&flet collect-error (condition &key debug?)
            (when debug?
              (bt:with-lock-held (errors-lock)
                (terpri)
                (princ condition)
                (terpri)
                (sb-debug:print-backtrace)))
            (bt:with-lock-held (errors-lock)
              (appendf errors (list condition)))))
         ((&flet deferrable-error (condition)
            (restart-bind ((defer (lambda (condition &key debug?)
                                    (collect-error condition :debug? debug?)
                                    (invoke-restart
                                     (jenkins.util:find-continue-restart
                                      condition))
                                    (abort))
                             :test-function #'jenkins.util:find-continue-restart))
              (error condition)))))
    (lparallel:task-handler-bind
        ;; TODO workaround lparallel bug
        ((error (lambda (condition)
                  (when (typep condition 'jenkins.util:continuable-error)
                    (deferrable-error condition)))))
      (handler-bind
          (((and error jenkins.util:continuable-error)
            #'deferrable-error))
        (funcall thunk (lambda () errors))))))

(defmacro with-error-collection ((errors) &body body)
  (with-gensyms (errors-reader)
    `(call-with-error-collection
      (lambda (,errors-reader)
        (symbol-macrolet ((,errors (funcall ,errors-reader)))
          ,@body)))))

;;; Phase timing

(defun call-with-phase-timing (thunk phase print?)
  (let ((start (get-internal-real-time)))
    (when print?
      (format t "START ~A~%" phase))
    (unwind-protect
         (funcall thunk)
      (let ((end (get-internal-real-time)))
        (when print?
          (format t "~&END   ~A, ~,3F second~:P~2%"
                  phase
                  (/ (- end start)
                     internal-time-units-per-second)))))))

(defmacro with-phase-timing ((phase &key print?) &body body)
  `(call-with-phase-timing (lambda () ,@body) ,phase ,print?))

;;; Phase

(defun call-as-phase (thunk name)
  (let (phase-errors)
    (multiple-value-prog1
        (with-error-collection (errors)
          (multiple-value-prog1
              (with-phase-timing (name)
                (funcall thunk))
            (setf phase-errors errors)))
      (when phase-errors
        (deferred-phase-cerror name phase-errors)))))

(defmacro as-phase ((name) &body body)
  `(call-as-phase (lambda () ,@body) ,name))
