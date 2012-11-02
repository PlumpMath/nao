### Nao

Nao is a DSL based on chicken scheme for distributed reactive programming.

Nao is like node.js and running in a event loop based on libuv. 
It also supports non-preempt thread(coroutine) based on chicken scheme's delimited continuation.
Nao gives a DSL which supports distribued reactive programming by non-preempt thread and libuv's networking.
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

#### Reactive programming

* Channel which data can be push into and read out.
  * (make-chan [name]): Create a channel;
  * (<- chan): Read a data from a channel. If channel is empty, block current thread.
  * (-> chan data): Write a data into a channel.

* Event system.
  * (make-event [name]): Create an event.
  * (event-subscribe event callback): Subscribe a callback into an event.
  * (event-unsubscribe event callback): Unsubscribe a callback from an event.
  * (remove-event event): Remove an event.

* Reactive system. 
  * (always@ sensitive-list-of-channels-or-events body): When any channel of event in the sensitive list is pushed a data or
event happens, the body will be executed. (This is like verilog's always block)
  * (initial body): Creat a non-preempt thread.
  * (@ chan-or-event [chan-or-event ...]): Blocking current thread until the writing of any channel or event in the list happens.

#### Distributed programming

* Server.
  * (start-server [addr: ip] [port: number]): Start a server.
  * (stop-server): stop server.

* Remote read/write.
  * (<~ remote-chan-name [addr: ip] [port: number]): Read a data from a remote channel. 
If remote channel is empty, return "empty channel". 
If remote channel does not exist, return "unkown channel".
  * (~> remote-chan-name data [addr: ip] [port: number]): Write a data into a remote channel.


