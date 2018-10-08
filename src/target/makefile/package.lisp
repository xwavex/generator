(cl:defpackage #:jenkins.target.makefile
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:shadow
   #:directory)

  (:local-nicknames
   (#:model   #:jenkins.model)

   (#:var     #:jenkins.model.variables)

   (#:project #:jenkins.model.project)
   (#:aspects #:jenkins.model.aspects)))
