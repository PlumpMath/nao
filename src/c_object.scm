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


(module object (register-object^
                unregister-object^
                unregister-object-by-id^
                object->id^
                id->object^)
  (import scheme)
  (import chicken)
  (import foreign)
  (use srfi-69)

  (define object-counter 0)

  (define id->object (make-hash-table))
  (define object->id (make-hash-table))

  (define (genid)
    (let ((i (if (> object-counter 10000000000000)
              1
              (+ object-counter 1))))
      (set! object-counter i)
      (if (hash-table-exists? id->object i)
        (genid)
        i)))

  (define (object->id^ obj)
    (hash-table-ref object->id obj))

  (define (id->object^ i)
    (hash-table-ref id->object i))

  (define (register-object^ obj)
    (let ((i (if (hash-table-exists? object->id obj)
               (hash-table-ref object->id obj)
               (genid))))
      (hash-table-set! object->id obj i)
      (hash-table-set! id->object i obj)
      i))

  (define (unregister-object^ obj)
    (if (hash-table-exists? object->id obj)
      (let ((i (object->id^ obj)))
        (hash-table-delete! object->id obj)
        (hash-table-delete! id->object i))))

  (define (unregister-object-by-id^ i)
    (if (hash-table-exists? id->object i)
      (let ((obj (id->object^ i)))
        (unregister-object^ obj))))

  (define-external (register_string (nonnull-c-string s)) unsigned-long
    (register-object^ s))

  (define-external (unregister_object (unsigned-long i)) void
    (unregister-object-by-id^ i)))

(import object)

(define register-object register-object^)
(define unregister-object unregister-object^)
(define unregister-object-by-id unregister-object-by-id^)
(define object->id object->id^)
(define id->object id->object^)
