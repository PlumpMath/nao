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

(define ($ #!optional (index 0))
  (coroutine-field current-coroutine index))

(define (snapshot c . args)
  (fold (lambda (arg i)
    (coroutine-set-field c i
      (cond
        ((sig? arg) (<! arg))
        ((chan? arg) (peek arg))
        (else arg)))
    (+ i 1))
    1
    args))

(define (@ . args)
  (let ((r (make-hash-table)))
    (for-each
      (lambda (a)
        (let* ((arg (if (number? a) (make-timer) a))
               (c current-coroutine)
               (f (lambda (#!rest as)
                    (coroutine-set-field c 0 a)
                    (apply snapshot (cons c args))
                    (coroutine-wake c))))
          (hash-table-set! r arg f)
          (cond
            ((event? arg) (event-subscribe arg f))
            ((sig? arg) (sig-subscribe-on-write arg f))
            ((chan? arg) (chan-subscribe-on-write arg f))
            ((timer? arg) (start-timer arg a f))
            (else (throw 'UT "unsupport type for @")))))
      args)
    (coroutine-sleep)
    (hash-table-for-each
      r
      (lambda (arg f)
        (cond
          ((event? arg) (event-unsubscribe arg f))
          ((sig? arg) (sig-unsubscribe-on-write arg f))
          ((chan? arg) (chan-unsubscribe-on-write arg f))
          ((timer? arg) (remove-timer arg))
          (else (throw 'UT "unsupport type for @")))))))

(define-syntax always@
  (syntax-rules ()
    ((_ (args ...) body body* ...)
     (make-coroutine (lambda ()
        (letrec ((f (lambda ()
                      (@ args ...)
                      body body* ...
                  (f))))
            (f)))))))

(define-syntax initial
  (syntax-rules ()
    ((_ body body* ...)
     (make-coroutine (lambda ()
       (make-coroutine (lambda ()
         body body* ...)))))))

(define-syntax nexttick
  (syntax-rules ()
    ((_ body body* ...)
     (next-tick (lambda () body body* ...)))))
