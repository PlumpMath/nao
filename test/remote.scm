(import nao)

(define ch (make-chan "chan0"))
(always@ (ch)
  (info (<- ch)))

(define rch (make-remote-chan "chan0" port: 1234))

(initial
  (start-server addr: "0.0.0.0" port: 1234)
  (~> rch 123)
  (info "remote" (<~ rch 2))
  (info "write done")
  (stop-server))
