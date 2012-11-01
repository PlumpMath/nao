(module dsl (@^
             always@^)

  (import scheme)
  (import chicken)
  (use srfi-69)
  (import coroutine)
  (import channel)

  (define (@^ . args)
    (let ((r (make-hash-table)))
      (for-each
        (lambda (arg)
          (let* ((c current-coroutine^)
                 (f (lambda ()
                      (coroutine-wake^ c))))
            (hash-table-set! r arg f)
            (subscribe-on-write^ arg f)))
        args)
      (coroutine-sleep^)
      (hash-table-for-each
        (lambda (arg f)
          (unsubscribe-on-write^ arg f))
        r)))
  
  (define (always@^ body . args)
    (make-coroutine^ (lambda ()
      (letrec ((f (lambda ()
                    (apply @^ args)
                    (body)
                    (f))))
        (f))))))

(import dsl)

(define @ @^)
(define always@ always@^)
