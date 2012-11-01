(define c (make-chan))

(always@ (lambda ()
  (info (<- c)))
  c)

(initial (lambda ()
  (-> c "aa")))
