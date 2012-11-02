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


(module event (make-event^
               event-name^
               event?^
               event-subscribe^
               event-notify^
               remove-event^
               event-unsubscribe^
               event^)
  (import scheme)
  (import chicken)
  (import foreign)
  (use srfi-69)
  (import tick)
  (import object)
  (import utils)

  (define-record event
     name
     callbacks)

  (define event-name^ event-name)
  (define event?^ event?)

  (define events (make-hash-table))

  (define (make-event^ #!optional (name #f))
    (let* ((n (if name
                 name
                 (gensym)))
           (e (make-event
                n
                (make-hash-table))))
      (hash-table-set! events n e)
      e))

  (define (event^ name)
    (if (hash-table-exists? events name)
      (hash-table-ref events name)
      (begin
        (throw^ 'NFE "cannot find event:" name))))

  (define (event-subscribe^ ev callback)
    (let ((e (cond
               ((event? ev) ev)
               (else (event^ ev)))))
      (hash-table-set! (event-callbacks e) callback callback))
    callback)

  (define (event-notify^ ev . args)
    (let* ((e (cond
                ((event? ev) ev)
                (else (event^ ev))))
           (cbs (hash-table-keys (event-callbacks e))))
      (for-each (lambda (cb)
        (apply cb args))
        cbs)))

  (define (remove-event^ ev)
    (let ((n (cond
               ((event? ev) (event-name ev))
               (else (event^ ev)))))
      (hash-table-delete! events n)))

  (define (event-unsubscribe^ ev callback)
    (let* ((e (cond
                ((event? ev) ev)
                (else (event^ ev))))
           (cbs (event-callbacks e)))
      (hash-table-delete! cbs callback)))

  (define-external (event_notify (long ev-id) (long arg-id)) void
    (let ((ev (id->object^ ev-id))
          (arg (id->object^ arg-id)))
      (next-tick^ (lambda ()
        (let ((args (if (list? arg) arg (list arg))))
           (apply event-notify^ (cons ev args))))))))


(import event)

(define make-event make-event^)
(define subscribe event-subscribe^)
(define notify event-notify^)
(define remove-event remove-event^)
(define unsubscribe event-unsubscribe^)
(define event event^)
(define event-name event-name^)
(define event? event?^)
