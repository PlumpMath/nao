(err "b\n")
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

(define e (make-event))

(define cb (event-subscribe e (lambda ()
  (display "event\n"))))

(define cb1 (event-subscribe e (lambda ()
  (display "event1\n"))))

(gc)

(event-notify e)

(event-unsubscribe e cb)

(event-notify e)

(define aa "aa")
(register-object a)
(display (object->id a))
(unregister-object-by-id (object->id a))
;(display (object->id 1000))
