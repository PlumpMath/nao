(define c (make-chan))

(always@ (lambda ()
  (info "a")
  (info "b"))
  c)

(initial (lambda ()
  (-> c "aa")))
