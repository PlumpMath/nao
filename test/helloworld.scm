(import nao)

(define ch0 (make-chan "chan0"))
(define ch1 (make-chan "chan1"))

(always@ (ch0 ch1)
  (info (<- ch0))
  (info (<- ch1))
  (stop-server))

(start-server addr: "0.0.0.0" port: 1234)

(define rch0 (make-remote-chan "chan0" addr: "0.0.0.0" port: 1234))
(define rch1 (make-remote-chan "chan1" addr: "0.0.0.0" port: 1234))

(~> rch0 "Hello")
(~> rch1 "World")
