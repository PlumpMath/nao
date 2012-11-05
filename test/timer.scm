(import nao)

(define t (make-timer))

(start-timer t 100 (lambda (t)
  (info "aa")))

(always@ (100)
  (info "bb"))
