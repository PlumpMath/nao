(define ch0 (make-chan "chan0"))
(define ch1 (make-chan "chan1"))

(always@ (list ch0 ch1)
  (lambda ()
    (info (<- ch0))
    (info (<- ch1))
    (stop-server)))

(start-server addr: "0.0.0.0" port: 1234)

(~> "chan0" "Hello" addr: "0.0.0.0" port: 1234)
(~> "chan1" "World" addr: "0.0.0.0" port: 1234)
