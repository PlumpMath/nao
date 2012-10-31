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

  (define-external (make_socket_with_ref (nonnull-c-pointer ref)) scheme-object
    (let ((s (make-socket
               (gensym)
               ref
               (make-hash-table initial: #f weak-keys: #f weak-values: #f))))
      (socket-ref-set! s ref)
      s))

  (define make-socket^^ (foreign-lambda nonnull-c-pointer make_socket))
  (define (make-socket^ #!key (name #f))
    (let ((s (make-socket
               (if name
                 name
                 (gensym))
               (make-socket^^)
               (make-hash-table initial: #f weak-keys: #f weak-values: #f))))
      s))

  (define socket-bind (foreign-lambda void socket_bind nonnull-c-pointer nonnull-c-string int))
  (define (socket-bind^ socket addr port)
    (socket-bind (socket-ref socket) addr port))

  (define socket-listen (foreign-lambda void socket_listen nonnull-c-pointer symbol))
  (define (socket-listen^ socket callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) 'listen e)
      (event-subscribe^ e callback)
      (socket-listen (socket-ref socket) e)))

  (define socket-connect (foreign-lambda void socket_connect nonnull-c-pointer nonnull-c-string int symbol))
  (define (socket-connect^ socket addr port callback)
    (let ((e (event-name^ (make-event^))))
      (hash-table-set! (socket-events socket) 'connect e)
      (event-subscribe^ e callback)
      (socket-connect (socket-ref socket) addr port e)))

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
(define remove-socket remove-socket^)
