;;;; jenkins.project.asd ---
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :jenkins.project
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Generates Jenkins jobs from different kinds of recipes."
  :depends-on  (:alexandria
                (:version :split-sequence                        "1.1")
                :iterate
                (:version :let-plus                              "0.1")
                (:version :more-conditions                       "0.1.0")
                (:version :utilities.print-items                 "0.1.0")
                (:version :utilities.print-tree                  "0.1.0")
                :local-time
                :lparallel
                :log4cl

                :puri
                :xml.location
                (:version :esrap                                 "0.9")
                (:version :cl-ppcre                              "2.0.3")
                (:version :cl-json                               "0.4.1")
                :cl-store
                :inferior-shell
                :ironclad

                (:version :rosetta                               "0.4")
                (:version :rosetta-project                       "0.3")

                (:version :jenkins.api                           "0.1")

                :cl-dot

                (:version :jenkins.project.more-conditions-patch (:read-file-form "version-string.sexp")))
  :components  ((:file       "cxml-hack"
                 :pathname   "src/cxml-patch")

                (:module     "version"
                 :pathname   "src/version"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "version")))

                (:module     "analysis"
                 :pathname   "src/analysis"
                 :depends-on ("version")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "variables")
                              (:file     "util")
                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "analysis")

                              ;; Version control systems
                              (:file     "scm-null")
                              (:file     "archive")
                              (:file     "git")
                              (:file     "subversion")
                              (:file     "mercurial")

                              ;; Generic analyses
                              (:file     "license")

                              ;; Build systems
                              (:file     "pkg-config")
                              (:file     "autotools")
                              (:file     "cmake")
                              (:file     "asdf")
                              (:file     "maven")
                              (:file     "ant")
                              (:file     "setuptools")
                              (:file     "ros-package")
                              (:file     "ros-packages")

                              ;; Platform analysis
                              (:file     "platform")))

                (:module     "model-variables"
                 :pathname   "src/model/variables"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "schema")
                              (:file     "trace")
                              (:file     "model")
                              (:file     "grammar")
                              (:file     "evaluation")

                              (:file     "mixins")))

                (:module     "model"
                 :pathname   "src/model"
                 :depends-on ("model-variables")
                 :serial     t
                 :components ((:file     "package")

                              (:file     "schema")

                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "util")

                              (:file     "mixins")))

                (:module     "model-project"
                 :pathname   "src/model/project"
                 :depends-on ("version" "analysis" "model" "model-variables" "model-aspects") ; TODO
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "mixins")
                              (:file     "classes-spec")
                              (:file     "classes-model")

                              (:file     "json")

                              (:file     "progress")))

                (:module     "model-aspects"
                 :pathname   "src/model/aspects"
                 :depends-on ("model")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "conditions")
                              (:file     "protocol")

                              (:file     "aspect")
                              (:file     "mixins")
                              (:file     "macros")

                              (:file     "aspects")
                              (:file     "aspects-scm")
                              (:file     "aspects-artifacts")
                              (:file     "aspects-build")
                              (:file     "aspects-publish")))

                (:module     "report"
                 :pathname   "src/report"
                 :depends-on ("model-project")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "protocol")
                              (:file     "util")
                              (:file     "json")
                              (:file     "graphviz")))))
