
(define ev (make-event "e0"))

(always@ ev (lambda ()
  (info "aa")))

(initial (lambda ()
  (notify ev)
  (notify ev)))
