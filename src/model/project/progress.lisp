;;;; progress.lisp --- Progress reports for operations.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:jenkins.model.project)

(defmethod deploy :before ((thing job) (target t))
  (progress :deploy/job nil "~A" thing))
