(import nao)

(define ev (make-event "e0"))

(always@ (ev)
  (info "aa"))

(initial
  (notify ev)
  (notify ev))
