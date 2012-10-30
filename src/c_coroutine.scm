(module coroutine (make-coroutine^
                   coroutine-sleep^
                   coroutine-wake^
                   run-scheduler^)
  (import scheme)
  (import chicken)
  (use srfi-69)
  (import tick)

  (require-extension shift-reset)

  (define-record fiber
    name
    alive
    cont)

  (define current-coroutine)

  (define running-q (make-hash-table initial: #f weak-keys: #t weak-values: #f))
  (define sleeping-q (make-hash-table initial: #f weak-keys: #t weak-values: #f))

  (define (make-coroutine^ body)
    (letrec ((c (make-fiber (gensym)
                          #t
                          (lambda (arg)
                            (body)
                            (fiber-alive-set! c #f)))))
      (hash-table-set! running-q c c)
      c))
  
  (define (coroutine-sleep^)
    (%shift cont
      (hash-table-set! sleeping-q current-coroutine current-coroutine)
      (hash-table-delete! running-q current-coroutine)
      (fiber-cont-set! current-coroutine cont)))

  (define (coroutine-wake^ f)
    (hash-table-set! running-q f f)
    (hash-table-delete! sleeping-q f))

  (define (coroutine-alive? f)
    (fiber-alive f))

  (define (run-one)
    (let ((cs (hash-table-keys running-q)))
      (for-each (lambda (c)
        (%reset
          (hash-table-delete! running-q c)
          (if (coroutine-alive? c)
            (begin
              (set! current-coroutine c)
              ((fiber-cont c) (void))))))
        cs)))

  (define (run-scheduler^)
    (letrec ((l (lambda ()
        (run-one)
        (if (not (= (length (hash-table-keys running-q)) 0))
          (next-tick^ l)))))
      (next-tick^ l))))

(import coroutine)

(define make-coroutine make-coroutine^)
(define coroutine-sleep coroutine-sleep^)
(define coroutine-wake coroutine-wake^)
(define run-scheduler run-scheduler^)

