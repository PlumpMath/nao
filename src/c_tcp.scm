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

  (define socket-listen (foreign-lambda void socket_listen nonnull-c-pointer scheme-object))
  (define (socket-listen^ socket callback)
    (let ((e (make-event^)))
      (hash-table-set! (socket-events socket) 'listen e)
      (event-subscribe^ e callback)
      (socket-listen (socket-ref socket) e)))

  (define remove-socket (foreign-lambda void remove_socket nonnull-c-pointer))
  (define (remove-socket^ socket)
    (remove-socket (socket-ref socket))
    (let ((e (hash-table-ref (socket-events socket) 'listen)))
      (if e
         (event-remove^ e)))))

(import uv-tcp)

(define make-socket make-socket^)
(define socket-bind socket-bind^)
(define socket-listen socket-listen^)
(define remove-socket remove-socket^)
