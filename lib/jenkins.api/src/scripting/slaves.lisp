(dolist (job (all-jobs))
  (when (member "oneiric" (slaves job) :test #'ppcre:scan)
    (setf (slaves job) (remove-if (curry #'ppcre:scan "oneiric") (slaves job)))
    (format t "~64A ~{~A~^, ~}~%" job (slaves job))
    (commit! job)))
