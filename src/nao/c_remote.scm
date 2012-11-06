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
          (begin
            (socket-write c (if (chan-exists? cn)
                                 (if (chan-empty? (chan cn))
                                   "empty channel"
                                   (<- (chan cn)))
                                 "unkown channel")
              (lambda (c)
                (remove-socket c)))))))))))

(define (stop-server)
  (remove-socket %server))

(define %remote-channels (make-hash-table))

(define-record remote-channel-
  name
  addr
  port)

(define (make-remote-chan name #!key (addr "0.0.0.0") (port 3000))
  (let ((c (make-remote-channel-
             name
             addr
             port)))
    (hash-table-set! %remote-channels name c)
    c))

(define (remove-remote-chan chan)
  (hash-table-delete! %remote-channels (remote-channel--name chan)))

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

(define (<~ chan)
  (let ((c (make-socket))
        (cc current-coroutine)
        (data ""))
    (socket-connect c (remote-channel--addr chan) (remote-channel--port chan) (lambda (c)
      (socket-write c (string-append (remote-channel--name chan) "\n<~")
        (lambda (c)
          (socket-read c (lambda (d)
            (set! data d)
            (remove-socket c)
            (coroutine-wake cc)))))))
    (coroutine-sleep)
    data))


