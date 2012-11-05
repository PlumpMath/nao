(import nao)

(define c (make-chan))

(always@ (c)
  (info (<- c)))

(initial 
  (-> c "aa"))
