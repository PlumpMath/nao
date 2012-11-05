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


(define-record fiber-
  name
  alive
  cont
  data)

(define current-coroutine)

(define %running-q (make-hash-table hash: eq?-hash))

(define (make-coroutine body #!optional (name #f))
  (letrec ((c (make-fiber- (if name
                            name
                            (gensym))
                        #t
                        (lambda (arg)
                          (body)
                          (fiber--alive-set! c #f))
                        (make-hash-table))))
    (hash-table-set! %running-q c c)
    c))

(define (coroutine-set-field f field data)
  (hash-table-set! (fiber--data f) field data))

(define (coroutine-field f field)
  (hash-table-ref (fiber--data f) field))

(define (coroutine-sleep)
  (%shift cont
    (hash-table-delete! %running-q current-coroutine)
    (fiber--cont-set! current-coroutine cont)))

(define (coroutine-wake f)
  (hash-table-set! %running-q f f))

(define (coroutine-alive? f)
  (fiber--alive f))

(define (run-one)
  (let ((cs (hash-table-keys %running-q)))
    (for-each (lambda (c)
      (hash-table-delete! %running-q c)
      (%reset
        (if (coroutine-alive? c)
          (begin
            (set! current-coroutine c)
            ((fiber--cont c) (void))))))
      cs)))

(define-external (run_scheduler) void
  (run-scheduler))

(define (run-scheduler)
  (letrec ((l (lambda ()
      (run-one)
      (if (or (not (= (length (hash-table-keys %running-q)) 0))
              (not (ticks-empty?)))
        (next-tick l)))))
    (next-tick l)))

