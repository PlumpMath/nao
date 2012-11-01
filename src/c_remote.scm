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


(module remote (start-server^
                stop-server^
                ~>^
                <~^)

  (import scheme)
  (import chicken)
  (use srfi-69)
  (import data-structures)
  (import coroutine)
  (import channel)
  (import uv-socket)
  (import logger)

  (define server (make-socket^))

  (define (start-server^ #!key (addr "0.0.0.0") (port 3000))
    (socket-bind^ server addr port)
    (socket-listen^ server (lambda (c)
      (socket-read^ c (lambda (d)
        (let* ((dd (string-split d "\n"))
               (cn (car dd))
               (ddd (cdr dd))
               (rw (car ddd)))
          (if (equal? rw "~>")
            (begin
              (if (chan-exists?^ cn)
                (->^ (chan^ cn) (apply string-append (cdr ddd))))
              (remove-socket^ c))
            (begin
              (socket-write^ c (if (chan-exists?^ cn)
                                   (if (chan-empty?^ (chan^ cn))
                                     "empty channel"
                                     (<-^ (chan^ cn)))
                                   "unkown channel")
                (lambda (c)
                  (remove-socket^ c)))))))))))

  (define (stop-server^)
    (remove-socket^ server))

  (define (~>^ chan data #!key (addr "0.0.0.0") (port 3000))
    (let ((c (make-socket^))
          (cc current-coroutine^))
      (socket-connect^ c addr port (lambda (c)
        (socket-write^ c (string-append chan "\n~>\n" data)
          (lambda (c)
            (remove-socket^ c)
            (coroutine-wake^ cc)))))
      (coroutine-sleep^)))

  (define (<~^ chan #!key (addr "0.0.0.0") (port 3000))
    (let ((c (make-socket^))
          (cc current-coroutine^)
          (data ""))
      (socket-connect^ c addr port (lambda (c)
        (socket-write^ c (string-append chan "\n<~")
          (lambda (c)
            (socket-read^ c (lambda (d)
              (set! data d)
              (remove-socket^ c)
              (coroutine-wake^ cc)))))))
      (coroutine-sleep^)
      data)))


(import remote)

(define start-server start-server^)
(define stop-server stop-server^)
(define ~> ~>^)
(define <~ <~^)

