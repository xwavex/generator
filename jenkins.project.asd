;;;; jenkins.project.asd ---
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "jenkins.project"
  :description "Generates Jenkins jobs from different kinds of recipes."
  :license     "LLGPLv3" ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "split-sequence"                               "1.1")
                "iterate"
                (:version "let-plus"                                     "0.1")
                (:version "more-conditions"                              "0.1.0")
                (:version "utilities.print-items"                        "0.1.0")
                (:version "utilities.print-tree"                         "0.1.0")
                "local-time"
                "lparallel"
                "log4cl"

                "puri"
                "xml.location"
                (:version "esrap"                                        "0.9")
                (:version "cl-ppcre"                                     "2.0.3")
                (:version "cl-json"                                      "0.4.1")
                (:version "language.yaml"                                "0.1")
                (:version "text.source-location"                         "0.1")
                (:version "text.source-location.print"                   "0.1")
                (:version "text.source-location.source-tracking-builder" "0.1")

                "cl-store"
                "inferior-shell"
                "ironclad"

                (:version "rosetta"                                      "0.4")
                (:version "rosetta-project"                              "0.5")

                (:version "jenkins.api"                                  "0.1")

                "cl-dot"

                (:version "jenkins.project.more-conditions-patch"        (:read-file-form "version-string.sexp")))

  :components  ((:module     "util"
                 :pathname   "src/util"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "restarts")
                              (:file     "strings")
                              (:file     "files")))

                (:module     "version"
                 :pathname   "src/version"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "version")))

                (:module     "analysis"
                 :pathname   "src/analysis"
                 :depends-on ("util" "version")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "variables")
                              (:file     "util")
                              (:file     "conditions")
                              (:file     "protocol")
                              (:file     "dependencies")
                              (:file     "analysis")

                              ;; Cache
                              (:file     "cache")

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
                              (:file     "mps")

                              ;; Platform analysis
                              (:file     "platform")))

                (:module     "model-variables"
                 :pathname   "src/model/variables"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "types")
                              (:file     "variables")
                              (:file     "protocol")
                              (:file     "schema")
                              (:file     "trace")
                              (:file     "model")
                              (:file     "grammar")
                              (:file     "evaluation")
                              (:file     "aggregation")

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

                              (:file     "progress")))

                (:module     "concrete-syntax"
                 :pathname   "src/model/project/concrete-syntax"
                 :depends-on ("model-project")
                 :serial     t
                 :components ((:file     "locations")
                              (:file     "conditions")

                              (:file     "util")

                              (:file     "builder")
                              (:file     "yaml")))

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

                (:module     "target-dockerfile"
                 :pathname   "src/target/dockerfile"
                 :depends-on ("model"
                              "model-project"
                              "model-aspects")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "target")
                              (:file     "aspects")))

                (:module     "target-makefile"
                 :pathname   "src/target/makefile"
                 :depends-on ("model"
                              "model-project"
                              "model-aspects")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "util")
                              (:file     "target")
                              (:file     "aspects")))

                (:module     "report"
                 :pathname   "src/report"
                 :depends-on ("util" "model-project")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "conditions")
                              (:file     "protocol")

                              (:file     "json")
                              (:file     "graphviz")
                              (:file     "catalog"))))

  :in-order-to ((test-op (test-op "jenkins.project/test"))))

(defsystem "jenkins.project/test"
  :description "Unit tests for the jenkins.project system"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "fiveam"          "1.4")

                (:version "jenkins.project" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :components ((:file     "package")))

                (:module     "model-variables"
                 :pathname   "test/model/variables"
                 :depends-on ("test")
                 :serial     t
                 :components ((:file     "package")
                              (:file     "grammar")
                              (:file     "evaluation"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:jenkins.project.test '#:run-tests)))
