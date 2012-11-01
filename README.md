### Nao

Nao is a DSL based on chicken scheme for distributed reactive programming.

### Example

Server:

	(define ch0 (make-chan "chan0"))
	(define ch1 (make-chan "chan1"))

	(always@ (lambda ()
	  (info (<- ch0))
	  (info (<- ch1))
	  (stop-server))
	  ch0 ch1)

	(start-server addr: "0.0.0.0" port: 1234)

Client:

	(~> "chan0" "Hello" addr: "0.0.0.0" port: 1234)
	(~> "chan1" "World" addr: "0.0.0.0" port: 1234)

### Install

* git clone https://github.com/wehu/nao.git
* make

### Limitation

