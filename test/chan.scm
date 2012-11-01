(define c (make-chan))

(always@ c
  (lambda ()
    (info (<- c))))

(initial (lambda ()
  (-> c "aa")))
