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

(module nao (
  ; object
  register-object
  unregister-object
  unregister-object-by-id
  object->id
  id->object
  ; logger
  info
  err
  warn
  debug 
  ; utils
  throw
  ; tick
  next-tick
  ticks-empty?
  init-tick
  ; coroutine
  make-coroutine
  coroutine-sleep
  coroutine-wake
  coroutine-alive?
  current-coroutine
  run-scheduler
  ; event
  make-event
  event-name
  event?
  subscribe
  notify
  remove-event
  unsubscribe
  event
  ; socket
  make-socket
  socket-bind
  socket-listen
  socket-connect
  socket-read
  socket-read-stop
  socket-write
  remove-socket
  ; channel
  make-chan
  chan
  chan?
  chan-name
  chan-exists?
  chan-empty?
  <-
  ->
  subscribe-on-read
  unsubscribe-on-read
  subscribe-on-write
  unsubscribe-on-write
  ; remote
  start-server
  stop-server
  ~>
  <~
  ; dsl
  @
  always@
  initial)

  (import chicken scheme)
  (import foreign)
  (use srfi-69)
  (use srfi-13)
  (use srfi-1)
  (use extras)
  (require-extension shift-reset)
  (import data-structures)

  (include "c_object.scm")
  (include "c_logger.scm")
  (include "c_utils.scm")
  (include "c_tick.scm")
  (include "c_coroutine.scm")
  (include "c_event.scm")
  (include "c_socket.scm")
  (include "c_channel.scm")
  (include "c_remote.scm")
  (include "c_dsl.scm"))


