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

(module uv-socket (make-socket^
                socket-bind^
                socket-listen^
                socket-connect^
                socket-read^
                socket-read-stop^
                socket-write^
                remove-socket^)
  
  (import foreign)
  (import event)
  (import chicken)
  (import scheme)
  (use srfi-69)
  (import object)
  (import utils)

  (define-record socket
    id
    ref
    events)

  (define-external (make_socket_with_ref (nonnull-c-pointer ref)) unsigned-long
    (let ((s (make-socket
               #f
               ref
               (make-hash-table))))
      (socket-ref-set! s ref)
      (let ((i (register-object^ s)))
        (socket-id-set! s i)
        i)))

  (define make-socket^^ (foreign-lambda nonnull-c-pointer make_socket unsigned-long))
  (define (make-socket^)
    (let ((s (make-socket
               #f
               #f
               (make-hash-table))))
      (let* ((i (register-object^ s))
             (ss (make-socket^^ i)))
        (socket-id-set! s i)
        (socket-ref-set! s ss))
      s))

  (define-external (socket_event (unsigned-long s-id) (nonnull-c-string ev)) unsigned-long
    (let* ((socket (id->object^ s-id))
           (evs (socket-events socket)))
      (if (hash-table-exists? evs ev)
        (hash-table-ref evs ev)
        (throw^ 'NFSE "cannot find event of socket:" ev))))

  (define socket-bind (foreign-lambda void socket_bind nonnull-c-pointer nonnull-c-string int))
  (define (socket-bind^ socket addr port)
    (socket-bind (socket-ref socket) addr port))

  (define socket-listen (foreign-lambda void socket_listen nonnull-c-pointer))
  (define (socket-listen^ socket callback)
    (let* ((e (make-event^))
           (e-id (register-object^ e)))
      (hash-table-set! (socket-events socket) "listen" e-id)
      (event-subscribe^ e callback)
      (socket-listen (socket-ref socket))))

  (define socket-connect (foreign-lambda void socket_connect nonnull-c-pointer nonnull-c-string int))
  (define (socket-connect^ socket addr port callback)
    (let* ((e (make-event^))
           (e-id (register-object^ e)))
      (hash-table-set! (socket-events socket) "connect" e-id)
      (event-subscribe^ e callback)
      (socket-connect (socket-ref socket) addr port)))

  (define socket-read (foreign-lambda void socket_read nonnull-c-pointer))
  (define (socket-read^ socket callback)
    (let* ((e (make-event^))
           (e-id (register-object^ e)))
      (hash-table-set! (socket-events socket) "read" e-id)
      (event-subscribe^ e callback)
      (socket-read (socket-ref socket))))

  (define socket-read-stop (foreign-lambda void socket_read_stop nonnull-c-pointer))
  (define (socket-read-stop^ socket)
    (socket-read-stop (socket-ref socket)))

  (define socket-write (foreign-lambda void socket_write nonnull-c-pointer nonnull-c-string int unsigned-long))
  (define (socket-write^ socket data callback)
    (let* ((e (make-event^))
           (e-id (register-object^ e)))
      (hash-table-set! (socket-events socket) "write" e-id)
      (event-subscribe^ e callback)
      (socket-write (socket-ref socket) data (string-length data) (socket-id socket))))

  (define remove-socket (foreign-lambda void remove_socket nonnull-c-pointer))
  (define (remove-socket^ socket)
    (remove-socket (socket-ref socket))
    (for-each (lambda (e-id)
        (unregister-object-by-id^ e-id)
        (event-remove^ (id->object^ e-id)))
      (hash-table-values (socket-events socket)))
    (unregister-object^ socket)))

(import uv-socket)

(define make-socket make-socket^)
(define socket-bind socket-bind^)
(define socket-listen socket-listen^)
(define socket-connect socket-connect^)
(define socket-read socket-read^)
(define socket-read-stop socket-read-stop^)
(define socket-write socket-write^)
(define remove-socket remove-socket^)
