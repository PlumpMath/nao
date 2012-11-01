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


(module logger (info^
                err^
                warn^
                debug^)

  (import scheme)
  (import chicken)
  (import foreign)
  (use extras)

  (define (p . args)
    (let ((ss (fold (lambda (arg init)
                     (append init (list (format #f "~a" arg))))
               init
               args)))
      (display "[nao ")
      (display (string-join ss) " ")
      (display "\n")))

  (define-external (info (nonnull-c-string arg)) void
    (info^ arg))
  (define (info^ . args)
    (apply p (cons "INFO]:" args)))

  (define-external (err (nonnull-c-string arg)) void
    (err^ arg))
  (define (err^ . args)
    (apply p (cons "ERR]:" args)))

  (define-external (warn (nonnull-c-string arg)) void
    (warn^ arg))
  (define (warn^ . args)
    (apply p (cons "WARN]:" args)))

  (define-external (debug (nonnull-c-string arg)) void
    (debug^ arg))
  (define (debug^ . args)
    (apply p (cons "DEBUG]:" args))))

(import logger)

(define info info^)
(define err err^)
(define warn warn^)
(define debug debug^)

