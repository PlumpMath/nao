(module tick (next-tick^)
  (import foreign)
  (import scheme)
  
  (define ticks `())
  (define new-ticks `())
  
  (define (next-tick^ callback)
    (set! new-ticks (cons callback new-ticks))
    (add-tick))
  
  (define add-tick (foreign-lambda void "add_tick"))
  
  (define-external (run_ticks) void
    (for-each (lambda (cb)
      (set! ticks (cons cb ticks)))
      new-ticks)
    (set! new-ticks `())
    (for-each (lambda (cb)
      (cb))
      ticks)
    (set! ticks `()))
  
  (define-external (ticks_empty_p) bool
    (if (= (length new-ticks) 0)
      #t #f)))

(import tick)
(define next-tick next-tick^)
