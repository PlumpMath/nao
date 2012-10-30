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

(module tick (next-tick^)
  (import foreign)
  (import scheme)
  
  (define ticks `())
  (define new-ticks `())
  
  (define (next-tick^ callback)
    (set! new-ticks (cons callback new-ticks))
    (add-tick))
  
  (define add-tick (foreign-lambda void "add_tick"))
  
  (define-external (run_ticks) void
    (for-each (lambda (cb)
      (set! ticks (cons cb ticks)))
      new-ticks)
    (set! new-ticks `())
    (for-each (lambda (cb)
      (cb))
      ticks)
    (set! ticks `()))
  
  (define-external (ticks_empty_p) bool
    (if (= (length new-ticks) 0)
      #t #f)))

(import tick)
(define next-tick next-tick^)
