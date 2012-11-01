(module logger (info^
                err^
                warn^
                debug^)

  (import scheme)
  (import chicken)
  (import foreign)
  (use extras)

  (define (p . args)
    (let ((ss (fold (lambda (arg init)
                     (append init (list (format #f "~a" arg))))
               init
               args)))
      (display "[nao ")
      (display (string-join ss) " ")
      (display "\n")))

  (define-external (info (nonnull-c-string arg)) void
    (info^ arg))
  (define (info^ . args)
    (apply p (cons "INFO]:" args)))

  (define-external (err (nonnull-c-string arg)) void
    (err^ arg))
  (define (err^ . args)
    (apply p (cons "ERR]:" args)))

  (define-external (warn (nonnull-c-string arg)) void
    (warn^ arg))
  (define (warn^ . args)
    (apply p (cons "WARN]:" args)))

  (define-external (debug (nonnull-c-string arg)) void
    (debug^ arg))
  (define (debug^ . args)
    (apply p (cons "DEBUG]:" args))))

(import logger)

(define info info^)
(define err err^)
(define warn warn^)
(define debug debug^)

