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


(define %timers (make-hash-table))

(define-record timer-
  name
  id
  ref
  events)

(define timer? timer-?)

(define make-timer% (foreign-lambda nonnull-c-pointer make_timer unsigned-long))
(define (make-timer #!optional (name #f))
  (let* ((n (if name name (gensym)))
         (t (make-timer-
              n
              #f
              #f
              (make-hash-table))))
    (timer--id-set! t (register-object t))
    (timer--ref-set! t (make-timer% (timer--id t)))
    (hash-table-set! %timers n t)
    (let ((e-id (register-object (make-event))))
      (hash-table-set! (timer--events t) "timeout" e-id))
    t))

(define (timer-event t e)
  (hash-table-ref (timer--events t) e))

(define (timer n)
  (hash-table-ref %timers n))

(define-external (timer_event (unsigned-long t-id) (nonnull-c-string ev)) unsigned-long
  (let* ((t (id->object t-id))
         (evs (timer--events t)))
    (if (hash-table-exists? evs ev)
      (hash-table-ref evs ev)
      (throw 'NFTE "cannot find event of timer:" ev))))

(define remove-timer% (foreign-lambda void remove_timer nonnull-c-pointer))
(define (remove-timer t)
  (let ((tt (if (timer? t) t (timer t))))
    (stop-timer tt)
    (hash-table-delete! %timers (timer--name tt))
    (for-each (lambda (e)
      (unregister-object-by-id e)
      (remove-event (id->object e)))
      (hash-table-values (timer--events tt)))
    (remove-timer% (timer--ref tt))
    (unregister-object tt)))

(define start-timer% (foreign-lambda void start_timer nonnull-c-pointer unsigned-long))
(define (start-timer t d cb)
  (event-subscribe (id->object (timer-event t "timeout")) cb)
  (start-timer% (timer--ref t) d))

(define stop-timer% (foreign-lambda void stop_timer nonnull-c-pointer))
(define (stop-timer t)
  (stop-timer% (timer--ref t)))


