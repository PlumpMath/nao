(display "b\n")
(next-tick (lambda ()
  (display "a\n")
  (next-tick (lambda ()
     (display "d\n")))))

(display "c\n")

(define b)
(define a (make-cor (lambda ()
  (display "aaa\n")
;  (cor-wake b)
   (cor-sleep)
   (cor-wake b)
  (display "ccc\n"))))

(set! b (make-cor (lambda ()
  (display "bbb\n")
 ; (cor-sleep)
  (cor-wake a)
  (cor-sleep)
  (display "eee\n"))))

