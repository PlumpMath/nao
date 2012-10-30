(module tick (next-tick^)
  (import foreign)
  (import scheme)
  
  (define ticks `())
  
  (define (next-tick^ callback)
    (set! ticks (cons callback ticks))
    (add-tick))
  
  (define add-tick (foreign-lambda void "add_tick"))
  
  (define-external (run_ticks) void
    (for-each (lambda (cb)
      (cb))
      ticks)
    (set! ticks `()))
  
  (define-external (ticks_empty_p) bool
    (if (= (length ticks) 0)
      #t #f)))

(import tick)
(define next-tick next-tick^)
