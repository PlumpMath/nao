(import nao)

(err "b\n")
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

(define e (make-event))

(define cb (subscribe e (lambda ()
  (display "event\n"))))

(define cb1 (subscribe e (lambda ()
  (display "event1\n"))))

(gc)

(notify e)

(unsubscribe e cb)

(notify e)

(define aa "aa")
(register-object a)
(display (object->id a))
(unregister-object-by-id (object->id a))
;(display (object->id 1000))

