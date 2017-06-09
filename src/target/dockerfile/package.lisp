(cl:defpackage #:jenkins.target.dockerfile
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:model   #:jenkins.model)

   (#:var     #:jenkins.model.variables)

   (#:project #:jenkins.model.project)
   (#:aspects #:jenkins.model.aspects)))
