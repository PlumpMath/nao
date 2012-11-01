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


(module channel (make-chan^
                 chan^
                 chan-name^
                 chan-empty?^
                 <-^
                 ->^
                 subscribe-on-read^
                 unsubscribe-on-read^
                 subscribe-on-write^
                 unsubscribe-on-write^)

  (import scheme)
  (import chicken)
  (import uv-socket)
  (import tick)
  (import event)
  (import coroutine)
  
  (use srfi-69)

  (define channels (make-hash-table))

  (define-record channel
    name
    queue
    events)

  (define chan-name^ channel-name)

  (define (chan^ name)
    (hash-table-ref channels name))

  (define (make-chan^ #!key (name #f))
    (let* ((n (if name name (gensym)))
           (c (make-channel
                n
                `()
                (make-hash-table))))
      (let ((evs (channel-events c)))
        (hash-table-set! evs "read" (make-event^))
        (hash-table-set! evs "write" (make-event^)))
      c))

  (define (chan-empty?^ chan)
    (let ((q (channel-queue chan)))
      (= (length q) 0)))

  (define (chan-event chan name)
    (hash-table-ref (channel-events chan) name))

  (define (<-^ chan)
    (if (chan-empty?^ chan) (coroutine-sleep^))
    (let ((d (car (channel-queue chan))))
      (channel-queue-set! chan (cdr (channel-queue chan)))
      (event-notify^ (chan-event chan "read"))
      d))

  (define (->^ chan data)
    (let ((q (channel-queue chan)))
      (channel-queue-set! chan (append q (list data)))
      (event-notify^ (chan-event chan "write"))))

  (define (subscribe-on-read^ chan callback)
    (let ((e (chan-event chan "read")))
      (event-subscribe^ e callback)))

  (define (unsubscribe-on-read^ chan callback)
    (let ((e (chan-event chan "read")))
      (event-unsubscribe^ e callback)))

  (define (subscribe-on-write^ chan callback)
    (let ((e (chan-event chan "write")))
      (event-subscribe^ e callback)))

  (define (unsubscribe-on-write^ chan callback)
    (let ((e (chan-event chan "write")))
      (event-unsubscribe^ e callback))))

(import channel)

(define make-chan make-chan^)
(define chan chan^)
(define chan-name chan-name^)
(define chan-empty? chan-empty?^)
(define <- <-^)
(define -> ->^)
(define subscribe-on-read subscribe-on-read^)
(define unsubscribe-on-read unsubscribe-on-read^)
(define subscribe-on-write  subscribe-on-write^)
(define unsubscribe-on-write unsubscribe-on-write^)

