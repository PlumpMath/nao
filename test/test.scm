(display "b\n")
(next-tick (lambda ()
  (display "a\n")
  (next-tick (lambda ()
     (display "d\n")))))

(display "c\n")

(define b)
(define a (make-coroutine (lambda ()
  (display "aaa\n")
;  (coroutine-wake b)
   (coroutine-sleep)
   (coroutine-wake b)
  (display "ccc\n"))))

(set! b (make-coroutine (lambda ()
  (display "bbb\n")
 ; (coroutine-sleep)
  (coroutine-wake a)
  (coroutine-sleep)
  (display "eee\n"))))

