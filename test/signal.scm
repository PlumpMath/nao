(import nao)

(define s (make-sig "s"))

(always@ (s)
  (info (<! s)))

(initial
  (!> s "123")
  (nexttick
     (!> s "456")))
