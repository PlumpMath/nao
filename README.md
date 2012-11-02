### Nao

Nao is a DSL based on chicken scheme for distributed reactive programming.

Nao is like node.js and running in a event loop based on libuv. 
It also supports co-routine based on chicken scheme's delimited continuation.
Nao gives a DSL which supports reactive programming by co-routine.
By supporting remote read/write, it is easy for distributed programming in nao.

### Example

Server:

	; create two channels
	(define ch0 (make-chan "chan0"))
	(define ch1 (make-chan "chan1"))

	; behaviors : sensitive list
	(always@ (list ch0 ch1)
	  (lambda ()
	    ; body
	    (info (<- ch0))
	    (info (<- ch1))
	    (stop-server)))

	; start server
	(start-server addr: "0.0.0.0" port: 1234)

Client:

	; remote writing
	(~> "chan0" "Hello" addr: "0.0.0.0" port: 1234)
	(~> "chan1" "World" addr: "0.0.0.0" port: 1234)

### Install

Requisites:

* chicken scheme [recommended version: 4.8.0]
* chicken pakage F-operator [for delimited continuation (reset/shift)]

Building commands:

* git clone https://github.com/wehu/nao.git
* make

### Run

	nao [file ...]

### Bugs and limitations

### APIs

