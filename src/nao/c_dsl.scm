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

(define ($)
  (coroutine-field current-coroutine "current-event"))

(define (@ . args)
  (let ((r (make-hash-table)))
    (for-each
      (lambda (arg)
        (let* ((c current-coroutine)
               (f (lambda ()
                    (coroutine-set-field c "current-event" arg)
                    (coroutine-wake c))))
          (hash-table-set! r arg f)
          (cond
            ((event? arg) (event-subscribe arg f))
            (else (subscribe-on-write arg f)))))
      args)
    (coroutine-sleep)
    (hash-table-for-each
      r
      (lambda (arg f)
        (cond
          ((event? arg) (event-unsubscribe arg f))
          (else (unsubscribe-on-write arg f)))))))

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

