
(define ev (make-event "e0"))

(always@ ev (lambda ()
  (info "aa")))

(initial (lambda ()
  (event-notify ev)
  (event-notify ev)))
