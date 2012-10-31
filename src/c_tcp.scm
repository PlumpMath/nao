;;;; 
;; Copyright 2012 The Nao Authors. All Rights Reserved.
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;; http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;;;

(module uv-tcp (make-socket^
                socket-bind^
                socket-listen^
                socket-connect^
                socket-read^
                socket-write^
                remove-socket^)
  
  (import foreign)
  (import event)
  (import chicken)
  (import scheme)
  (use srfi-69)

  (define-record socket
    name
    ref
    events)

  (define sockets (make-hash-table initial: #f weak-keys: #f weak-values: #f)) 

  (define-external (make_socket_with_ref (nonnull-c-pointer ref)) scheme-object
    (let ((s (make-socket
               (gensym)
               ref
               (make-hash-table initial: #f weak-keys: #f weak-values: #f))))
      (socket-ref-set! s ref)
      (hash-table-set! sockets (symbol->string (socket-name s)) s)
      s))

  (define make-socket^^ (foreign-lambda nonnull-c-pointer make_socket))
  (define (make-socket^)
    (let ((s (make-socket
               (gensym)
               (make-socket^^)
               (make-hash-table initial: #f weak-keys: #f weak-values: #f))))
      (hash-table-set! sockets (symbol->string (socket-name s)) s)
      s))

  (define (socket^ name)
    (let ((nn (if (symbol? name) (symbol->string name) name)))
      (if (hash-table-exists? sockets nn)
        (hash-table-ref sockets nn)
        (begin
          (abort "cannot find socket")))))

  (define-external (get_socket (nonnull-c-string name)) scheme-object
    (socket^ name))

  (define-external (socket_event (nonnull-c-string s) (nonnull-c-string ev)) nonnull-c-string
    (let* ((socket (socket^ s))
           (evs (socket-events socket)))
      (if (hash-table-exists? evs ev)
        (symbol->string (hash-table-ref evs ev))
        (abort "cannot find event of socket"))))

  (define socket-bind (foreign-lambda void socket_bind nonnull-c-pointer nonnull-c-string int))
  (define (socket-bind^ socket addr port)
    (socket-bind (socket-ref socket) addr port))

  (define socket-listen (foreign-lambda void socket_listen nonnull-c-pointer symbol))
  (define (socket-listen^ socket callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) "listen" e)
      (event-subscribe^ e callback)
      (socket-listen (socket-ref socket) (socket-name socket))))

  (define socket-connect (foreign-lambda void socket_connect nonnull-c-pointer nonnull-c-string int symbol))
  (define (socket-connect^ socket addr port callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) "connect" e)
      (event-subscribe^ e callback)
      (socket-connect (socket-ref socket) addr port (socket-name socket))))

  (define socket-read (foreign-lambda void socket_read nonnull-c-pointer symbol))
  (define (socket-read^ socket callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) "read" e)
      (event-subscribe^ e callback)
      (socket-read (socket-ref socket) (socket-name socket))))

  (define socket-write (foreign-lambda void socket_write nonnull-c-pointer nonnull-c-string int symbol))
  (define (socket-write^ socket data callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) "write" e)
      (event-subscribe^ e callback)
      (socket-write (socket-ref socket) data (string-length data) (socket-name socket))))

  (define remove-socket (foreign-lambda void remove_socket nonnull-c-pointer))
  (define (remove-socket^ socket)
    (remove-socket (socket-ref socket))
    (for-each (lambda (e)
        (event-remove^ e))
      (hash-table-values (socket-events socket)))))

(import uv-tcp)

(define make-socket make-socket^)
(define socket-bind socket-bind^)
(define socket-listen socket-listen^)
(define socket-connect socket-connect^)
(define socket-read socket-read^)
(define socket-write socket-write^)
(define remove-socket remove-socket^)
