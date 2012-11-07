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


(define %server (make-socket))
(define %client-counter 0)
(define %stop-counter 0)
(define %max-stop-try 10)

(define (start-server #!key (addr "0.0.0.0") (port 3000))
  (socket-bind %server addr port)
  (socket-listen %server (lambda (c)
    (socket-read c (lambda (d)
      (let* ((dd (string-split d "\n"))
             (cn (car dd))
             (ddd (cdr dd))
             (rw (car ddd)))
        (if (equal? rw "~>")
          (begin
            (if (chan-exists? cn)
              (-> (chan cn) (apply string-append (cdr ddd))))
            (remove-socket c))
          (make-coroutine (lambda ()
            (set! %client-counter (+ %client-counter 1))
            (socket-write c (if (chan-exists? cn)
                                  (<<- (chan cn) (string->number (cadr ddd)))
                                  "unkown channel")
              (lambda (c)
                (remove-socket c)
                (set! %client-counter (- %client-counter 1)))))))))))))

(define (stop-server)
  (if (= %client-counter 0)
    (remove-socket %server)
    (make-coroutine (lambda ()
      (letrec ((l (lambda () 
                    (@ 1)
                    (set! %stop-counter (+ %stop-counter 1))
                    (if (or (= %client-counter 0)
                            (> %stop-counter %max-stop-try))
                      (begin (remove-socket %server)
                             (set! %stop-counter 0))
                      (l)))))
        (l))))))

(define %remote-channels (make-hash-table))

(define-record remote-channel-
  name
  addr
  port)

(define remote-chan? remote-channel-?)

(define (make-remote-chan name #!key (addr "0.0.0.0") (port 3000))
  (let ((c (make-remote-channel-
             name
             addr
             port)))
    (hash-table-set! %remote-channels name c)
    c))

(define (remove-remote-chan chan)
  (let ((rc (if (remote-chan? chan) chan (remote-chan chan))))
    (hash-table-delete! %remote-channels (remote-channel--name chan))))

(define (remote-chan name)
  (hash-table-ref %remote-channels name))

(define (~> chan data)
  (let ((c (make-socket))
        (cc current-coroutine))
    (socket-connect c (remote-channel--addr chan) (remote-channel--port chan) (lambda (c)
      (socket-write c (string-append (remote-channel--name chan) "\n~>\n" (format #f "~a" data))
        (lambda (c)
          (remove-socket c)
          (coroutine-wake cc)))))
    (coroutine-sleep)))

(define (<~ chan #!optional (delay -1))
  (let ((c (make-socket))
        (cc current-coroutine)
        (data "timeout")
        (t (make-timer)))
    (socket-connect c (remote-channel--addr chan) (remote-channel--port chan) (lambda (c)
      (socket-write c (string-append (remote-channel--name chan) "\n<~\n" (number->string delay))
        (lambda (c)
          (socket-read c (lambda (d)
            (set! data d)
            (remove-socket c)
            (coroutine-wake cc)))))))
    (if (>= delay 0)
      (start-timer t delay (lambda (tt)
        (coroutine-wake cc))))
    (coroutine-sleep)
    (remove-timer t)
    data))


