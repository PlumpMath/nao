(import nao)

(define ev (make-event "e0"))

(always@ (ev)
  (info (event-name ($)))
  (info "aa"))

(initial
  (@ 1)
  (notify ev)
  (@ 1)
  (notify ev))
