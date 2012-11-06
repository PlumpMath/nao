### Nao

Nao is a DSL in chicken scheme for distributed reactive programming.

Nao is like node.js and running in a event loop based on libuv. 
It also supports non-preempt thread(coroutine) based on chicken scheme's delimited continuation.
Nao gives a DSL which supports distributed reactive programming by non-preempt thread and libuv's networking.
By supporting remote read/write, it is easy for distributed programming in nao.

### Example

Server:

	(import nao)

	; create two channels
	(define ch0 (make-chan "chan0"))
	(define ch1 (make-chan "chan1"))

	; behaviors : sensitive list
	(always@ (ch0 ch1)
	  ; body
	  (info (<- ch0))
	  (info (<- ch1))
	  (stop-server))

	; start server
	(start-server addr: "0.0.0.0" port: 1234)

Client:

	(import nao)

	(define rch0 (make-remote-chan "chan0" addr: "0.0.0.0" port: 1234))
	(define rch1 (make-remote-chan "chan1" addr: "0.0.0.0" port: 1234))

	; remote writing
	(-> rch0 "Hello")
	(-> rch1 "World")

### Install

Requisites:

* chicken scheme [recommended version: 4.8.0]
* chicken pakage F-operator [for delimited continuation (reset/shift)]

Building commands:

* git clone https://github.com/wehu/nao.git
* make [PREFIX=path]

### Run

	nao [file ...]

### Bugs and limitations

Only support unix platform so far.

### APIs

#### Reactive programming

###### Signal

* (make-sig [name]): Create a signal.
* (<- signal): Read a data from a signal.
* (-> signal data): Write a data into a signal.
* (remove-sig signal): Remove a signal.

###### Channel

* (make-chan [name]): Create a channel.
* (<- chan [timer]): Read a data from a channel. If channel is empty, block current thread.
If a timer is specified, it will wake up current thread and return "timeout" if time is out.
* (-> chan data): Write a data into a channel.
* (remove-chan chan): Remove a channel.

###### Event

* (make-event [name]): Create an event.
* (subscribe event callback): Subscribe a callback into an event.
* (unsubscribe event callback): Unsubscribe a callback from an event.
* (notify event args ...): Notify an event with arguments.
* (remove-event event): Remove an event.

###### Reactive system

* (always@ (sensitive-list-of-channels-or-events-or-timer) body): When any channel of event in the sensitive list is pushed a data or
event happens or time out, the body will be executed. (This is like verilog's always block)
* (initial body): Creat a non-preempt thread.
* (@ sensitive-list-of-chan-or-event-or-timer): Blocking current thread until the writing of any channel or event in the list happens.
* ($ [index]): snapshot of values for sensitive list.
  * ($): happened event.
  * ($ 0): the value related to happened event.
  * ($ 1..): the value in location indicated by index. 

#### Distributed programming

###### Server

* (start-server [addr: ip] [port: number]): Start a server.
* (stop-server): stop server.

###### Remote read/write

* (make-remote-chan name [addr: ip] [port: number]): Create a remote channel.
* (remove-remote-chan chan): Remove a remote channel.
* (<- remote-chan [timer]): Read a data from a remote channel. 
If remote channel is empty, block current thread.
If a timer is specified, it will wake up current thread and return "timeout" if time is out. 
If remote channel does not exist, return "unkown channel".
* (-> remote-chan data): Write a data into a remote channel.


