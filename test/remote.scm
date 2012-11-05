(import nao)

(define ch (make-chan "chan0"))
(always@ (ch)
  (info (<- ch)))

(initial
  (start-server addr: "0.0.0.0" port: 1234)
  (~> "chan0" "abc" port: 1234)
  (info (<~ "chan0" port: 1234))
  (info "write done")
  (stop-server))
