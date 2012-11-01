
(define ch (make-chan "chan0"))
(always@ (lambda ()
  (info (<- ch))
  (stop-server))
  ch)

(initial (lambda ()
  (start-server addr: "0.0.0.0" port: 1234)
  (~> "chan0" "abc" port: 1234)))
