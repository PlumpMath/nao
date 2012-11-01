### Nao

Nao is a DSL based on chicken scheme for distributed reactive programming.

### Example

Server:

	(define ch (make-chan "chan0")
	(always@ (lambda ()
	  (info (<- ch))
	  (stop-server)
	  ch)

	(start-server addr: "0.0.0.0" port: 1234)

Client:

	(~> "chan0" "Hello World!" addr: "0.0.0.0" port: 1234)

### Install

* git clone https://github.com/wehu/nao.git
* make

### Limitation

