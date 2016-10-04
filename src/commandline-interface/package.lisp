;;;; package.lisp --- Package definition for the commandline-interface module.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:jenkins.project.commandline-interface
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:jenkins.project)

  #+sbcl (:local-nicknames
          (#:clon #:net.didierverna.clon))

  (:shadowing-import-from #:jenkins.project
   #:as)

  (:export
   #:main)

  (:documentation
   "TODO"))

#-sbcl (net.didierverna.clon:nickname-package)
