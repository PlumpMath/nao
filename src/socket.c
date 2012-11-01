/*
* Copyright 2012 The Nao Authors. All Rights Reserved.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
*/


#include "socket.h"
#include <uv.h>
#include <string.h>
#include "logger.h"
#include "utils.h"
#include <assert.h>

typedef struct {
  uv_tcp_t tcp;
  unsigned long socket_id;
} c_tcp_t;

typedef struct {
  uv_write_t req;
  uv_buf_t buf;
  unsigned long socket_id;
} c_write_req_t;

uv_tcp_t * make_socket(unsigned long id){
  uv_tcp_t * socket = (uv_tcp_t *)malloc(sizeof(c_tcp_t));
  assert(socket);
  ((c_tcp_t*)socket)->socket_id = id;
  int r = uv_tcp_init(uv_default_loop(), socket);
  if(r) err(uv_error_msg());
  return socket;
}

void socket_bind(uv_tcp_t * socket, char *addr, int port){
  struct sockaddr_in6 bind_addr = uv_ip6_addr(addr, port);
  int r = uv_tcp_bind6(socket, bind_addr);
  if(r) err(uv_error_msg());
}

static void on_new_connection(uv_stream_t *socket, int status){
  if(status == -1) err(uv_error_msg());
  uv_tcp_t *client = (uv_tcp_t *)malloc(sizeof(uv_tcp_t));
  if(client) {
    int r = uv_tcp_init(uv_default_loop(), client);
    if(r){
        warn(uv_error_msg());
        remove_socket(client);
    } else {
      if(uv_accept(socket, (uv_stream_t *)client) == 0){
        unsigned long s_id = ((c_tcp_t*)socket)->socket_id;
        unsigned long c_id = make_socket_with_ref(client);
        event_notify(socket_event(s_id, "listen"), c_id);
      } else {
        warn(uv_error_msg());
        remove_socket(client);
      }
    }
  }
}

void socket_listen(uv_tcp_t * socket){
  int r = uv_listen((uv_stream_t *) socket, 128, on_new_connection);
  if(r) err(uv_error_msg());
}

static void on_connect(uv_connect_t * req, int status){
  if(status == -1) err(uv_error_msg());
  uv_tcp_t * socket = (uv_tcp_t *)req->handle;
  unsigned long s_id = ((c_tcp_t*)socket)->socket_id;
  event_notify(socket_event(s_id, "connect"), s_id);
  free(req);
}

void socket_connect(uv_tcp_t * socket, char * addr, int port){
  uv_connect_t *connect = (uv_connect_t*)malloc(sizeof(uv_connect_t));
  assert(connect);
  struct sockaddr_in6 dest = uv_ip6_addr(addr, port);
  int r = uv_tcp_connect6(connect, socket, dest, on_connect);
  if(r) err(uv_error_msg());
}

static void on_read(uv_stream_t * socket, ssize_t nread, uv_buf_t buf){
  if(nread == -1){
    socket_read_stop((uv_tcp_t *)socket);
  } else if(nread == 0) {
  } else {
    unsigned long s_id = ((c_tcp_t*)socket)->socket_id;
    unsigned long b_id = register_string(buf.base);
    event_notify(socket_event(s_id, "read"), b_id);
    unregister_object(b_id);
    free(buf.base);
  }
}

static uv_buf_t on_alloc(uv_handle_t *handle, size_t suggested_size){
  char * buf = (char *)malloc(suggested_size);
  assert(buf);
  return uv_buf_init(buf, suggested_size);
}

void socket_read(uv_tcp_t * socket){
  int r = uv_read_start((uv_stream_t *)socket, on_alloc, on_read);
  if(r) err(uv_error_msg());
}

void socket_read_stop(uv_tcp_t * socket){
  int r = uv_read_stop((uv_stream_t *)socket);
  if(r) err(uv_error_msg());
}

static void on_write(uv_write_t * req, int status){
  if(status == -1) err(uv_error_msg());
  unsigned long s_id = ((c_write_req_t *)req)->socket_id;
  event_notify(socket_event(s_id, "write"), s_id);
  free(((c_write_req_t *)req)->buf.base);
  free(req);
}

void socket_write(uv_tcp_t * socket, char *data, int len, unsigned long socket_id){
  c_write_req_t * req = (c_write_req_t *)malloc(sizeof(c_write_req_t));
  assert(req);
  req->socket_id = socket_id;
  char * buf = (char *) malloc(len);
  assert(buf);
  strncpy(buf, data, len);
  req->buf = uv_buf_init(buf, len);
  int r = uv_write((uv_write_t *)req, (uv_stream_t *)socket, &req->buf, 1, on_write);
  if(r) err(uv_error_msg());
}

static void on_close(uv_handle_t * h){
  free(h);
}

void remove_socket(uv_tcp_t * socket){
  uv_close((uv_handle_t *) socket, on_close);
}
