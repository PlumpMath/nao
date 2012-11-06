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


(define %channels (make-hash-table))

(define-record channel-
  name
  queue
  events)

(define chan-name channel--name)
(define chan? channel-?)

(define (chan-exists? name)
  (hash-table-exists? %channels name))

(define (chan name)
  (hash-table-ref %channels name))

(define (make-chan #!optional (name #f))
  (let* ((n (if name name (gensym)))
         (c (make-channel-
              n
              `()
              (make-hash-table))))
    (hash-table-set! %channels n c)
    (let ((evs (channel--events c)))
      (hash-table-set! evs "read" (make-event))
      (hash-table-set! evs "write" (make-event)))
    c))

(define (chan-empty? chan)
  (let ((q (channel--queue chan)))
    (= (length q) 0)))

(define (chan-event chan name)
  (hash-table-ref (channel--events chan) name))

(define (<- chan #!optional (delay -1))
  (letrec ((cf (lambda ()
     (if (chan-empty? chan) (begin
       (if (< delay 0) (@ chan) (@ chan delay))
       (if (equal? ($) chan) (cf)))))))
    (cf))
  (if (equal? ($) chan)
    (let ((d (car (channel--queue chan))))
      (channel--queue-set! chan (cdr (channel--queue chan)))
      (event-notify (chan-event chan "read"))
      d)
    "timeout"))

(define (-> chan data)
  (let ((q (channel--queue chan)))
    (channel--queue-set! chan (append q (list data)))
    (event-notify (chan-event chan "write"))))

(define (chan-subscribe-on-read chan callback)
  (let ((e (chan-event chan "read")))
    (event-subscribe e callback)))

(define (chan-unsubscribe-on-read chan callback)
  (let ((e (chan-event chan "read")))
    (event-unsubscribe e callback)))

(define (chan-subscribe-on-write chan callback)
  (let ((e (chan-event chan "write")))
    (event-subscribe e callback)))

(define (chan-unsubscribe-on-write chan callback)
  (let ((e (chan-event chan "write")))
    (event-unsubscribe e callback)))

(define (remove-chan c)
  (let ((cc (if (chan? c) c (chan c))))
    (hash-table-delete! %channels (chan-name cc))
    (hash-table-for-each (channel--events cc)
      (lambda (n e)
        (remove-event e)))))
