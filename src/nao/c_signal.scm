;;;
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


(define %signals (make-hash-table))

(define-record signal-
  name
  data
  events)

(define sig? signal-?)

(define (make-sig #!optional (name #f))
  (let* ((n (if name name (gensym)))
         (s (make-signal-
              n
              #f
              (make-hash-table))))
    (hash-table-set! %signals n s)
    (hash-table-set! (signal--events s) "read" (make-event))
    (hash-table-set! (signal--events s) "write" (make-event))
    s))

(define (sig name)
  (hash-table-ref %signals name))

(define (remove-sig s)
  (let ((ss (if (sig? s) s (sig s))))
    (hash-table-delete! %signals (signal--name ss))
    (hash-table-for-each (signal--events ss)
      (lambda (n e)
        (remove-event e)))))

(define (sig-event s name)
  (hash-table-ref (signal--events s) name))

(define (<! s)
  (let ((v (signal--data s)))
    (event-notify (sig-event s "read"))
    v))

(define (!> s data)
  (signal--data-set! s data)
  (event-notify (sig-event s "write")))

(define (sig-subscribe-on-read s callback)
  (let ((e (sig-event s "read")))
    (event-subscribe e callback)))

(define (sig-unsubscribe-on-read s callback)
  (let ((e (sig-event s "read")))
    (event-unsubscribe e callback)))

(define (sig-subscribe-on-write s callback)
  (let ((e (sig-event s "write")))
    (event-subscribe e callback)))

(define (sig-unsubscribe-on-write s callback)
  (let ((e (sig-event s "write")))
    (event-unsubscribe e callback)))

