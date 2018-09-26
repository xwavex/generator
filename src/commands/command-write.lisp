;;;; command-write.lisp --- Write different kinds of output based on a distribution.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.project.commands)

(defclass generate-other (distribution-input-mixin
                          mode-mixin)
  ((target    :initarg  :target
              :type     (member :dockerfile :makefile)
              :reader   target
              :initform :dockerfile)
   (arguments :initarg  :arguments
              :type     (or null string)
              :reader   arguments
              :initform nil
              :documentation
              #.(format nil "Arguments for the target.~@
                 ~@
                 something like~@
                 ~@
                 ~2@T{\"base-iamge\":\"debian:latest\", ~
                      \"platform\":[\"ubuntu\",\"xenial\"]}")))
  (:documentation
   "Generate non-Jenkins things for a given distribution."))

(service-provider:register-provider/class
 'command :generate-other :class 'generate-other)

(jenkins.project.commandline-options:define-option-mapping
    (*command-schema* "generate-other")
  (&rest             "distributions" "DISTRIBUTION-NAME"   t)

  (("--mode" "-m")   "mode"          "MODE")
  (("--set" "-D")    "overwrites"    "VARIABLE-NAME=VALUE")

  (("--target" "-t") "target"        "TARGET-SPECIFICATION")
  (("--arguments" "-a") "arguments"  "JSON-OBJECT"))

(defmethod command-execute ((command generate-other))
  (let+ (((&accessors-r/o distributions mode overwrites target arguments)
          command)
         (arguments (when arguments
                      (alist-plist (json:decode-json-from-string arguments))))
         (target*   (apply #'make-instance
                           (case target
                             (:dockerfile 'jenkins.target.dockerfile::dockerfile-target)
                             (:makefile   'jenkins.target.makefile::makefile-target))
                           arguments))
         ((&values distributions projects)
          (generate-load distributions mode overwrites
                         :generator-version (generator-version)))
         (distributions
          (generate-analyze distributions projects
                            :generator-version (generator-version)
                            :cache-directory   *cache-directory*
                            :temp-directory    *temp-directory*))
         (distributions
          (as-phase (:instantiate)
            (mapcan (lambda (distribution-spec)
                      (when-let ((distribution (instantiate distribution-spec)))
                        (list distribution)))
                    distributions))))
    (as-phase (:check-access ; :continuable? nil
               )
      (check-distribution-access distributions))
    (let ((versions (mappend #'versions distributions)))
      (as-phase (:generate)
        (jenkins.model.project::finish-deploy
         distributions
         (deploy-projects versions target)
         #+no (with-sequence-progress (:generate/project versions)
           (iter (for version in versions)
             (progress "~/print-items:format-print-items/"
                       (print-items:print-items version))
             (more-conditions::without-progress
               (with-simple-restart
                   (continue "~@<Skip deploying project version ~S.~@:>" version)
                 (appending (flatten (deploy version target)))))))
         target*)))))
