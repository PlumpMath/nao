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
               event-subscribe^
               event-notify^
               event-remove^
               event-unsubscribe^
               event^)
  (import scheme)
  (import chicken)
  (import foreign)
  (use srfi-69)
  (import tick)

  (define-record event
     name
     callbacks)

  (define event-name^ event-name)

  (define events (make-hash-table initial: #f weak-keys: #f weak-values: #f))

  (define (make-event^ #!key (name #f))
    (let* ((n (if name
                 name
                 (gensym)))
           (e (make-event
                n
                (make-hash-table initial: #f weak-keys: #t weak-values: #f))))
      (hash-table-set! events (if (symbol? n) (symbol->string n) n) e)
      e))

  (define (event^ name)
    (let ((nn (if (symbol? name) (symbol->string name) name)))
      (if (hash-table-exists? events nn)
        (hash-table-ref events nn)
        (begin
          (abort "cannot find event")))))

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

  (define (event-remove^ ev)
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

  (define-external (event_notify (symbol ev) (scheme-object arg)) void
    (next-tick^ (lambda ()
      (let ((args (if (list? arg) arg (list arg))))
        (apply event-notify^ (cons ev args)))))))


(import event)

(define make-event make-event^)
(define event-subscribe event-subscribe^)
(define event-notify event-notify^)
(define event-remove event-remove^)
(define event-unsubscribe event-unsubscribe^)
(define event event^)
(define event-name event-name^)
