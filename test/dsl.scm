(import nao)

(define chan (make-chan))
(define ev (make-event))
(define sig (make-sig))

(always@ (chan ev sig)
  (info ($))
  (info ($ 0))
  (info ($ 1))
  (info ($ 2))
  (info ($ 3))
  (info "====="))

(initial
  (!> sig "sig")
  (@ 1)
  (-> chan "chan")
  (@ 1)
  (notify ev))
