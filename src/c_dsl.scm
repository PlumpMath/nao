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


(module dsl (@^
             always@^
             initial^)

  (import scheme)
  (import chicken)
  (use srfi-69)
  (import coroutine)
  (import channel)

  (define (@^ . args)
    (let ((r (make-hash-table)))
      (for-each
        (lambda (arg)
          (let* ((c current-coroutine^)
                 (f (lambda ()
                      (coroutine-wake^ c))))
            (hash-table-set! r arg f)
            (subscribe-on-write^ arg f)))
        args)
      (coroutine-sleep^)
      (hash-table-for-each
        r
        (lambda (arg f)
          (unsubscribe-on-write^ arg f)))))

  (define (always@^ arg body)
    (let ((args (if (list? arg) arg (list arg))))
      (make-coroutine^ (lambda ()
        (letrec ((f (lambda ()
                    (apply @^ args)
                    (body)
                    (f))))
          (f))))))
  
  (define (initial^ body)
    (make-coroutine^ (lambda ()
      (make-coroutine^ (lambda ()
         (body)))))))

(import dsl)

(define @ @^)
(define always@ always@^) 
(define initial initial^)
