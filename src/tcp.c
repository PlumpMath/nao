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


#include "tcp.h"
#include <uv.h>
#include <string.h>
#include "logger.h"
#include "utils.h"
#include <assert.h>

typedef struct {
  uv_tcp_t orgin;
  char user_data[1024];
} c_tcp_t;

uv_tcp_t * make_socket(){
  uv_tcp_t * socket = (uv_tcp_t *)malloc(sizeof(c_tcp_t));
  assert(socket);
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
        C_word c = make_socket_with_ref(client);
        event_notify(((c_tcp_t*)socket)->user_data, c);
      } else {
        warn(uv_error_msg());
        remove_socket(client);
      }
    }
  }
}

void socket_listen(uv_tcp_t * socket, char * event){
  int r = uv_listen((uv_stream_t *) socket, 128, on_new_connection);
  if(r) err(uv_error_msg());
  strcpy(&((c_tcp_t*)socket)->user_data, event);
}

static void on_connect(uv_connect_t * req, int status){
  if(status == -1) err(uv_error_msg());
  uv_tcp_t * socket = (uv_tcp_t *)req->handle;
  C_word s = make_socket_with_ref(socket);
  event_notify(((c_tcp_t*)socket)->user_data, s);
  free(req);
}

void socket_connect(uv_tcp_t * socket, char * addr, int port, char * event){
  uv_connect_t *connect = (uv_connect_t*)malloc(sizeof(uv_connect_t));
  assert(connect);
  struct sockaddr_in6 dest = uv_ip6_addr(addr, port);
  int r = uv_tcp_connect6(connect, socket, dest, on_connect);
  if(r) err(uv_error_msg());
  strcpy(&((c_tcp_t*)socket)->user_data, event);
}

static void on_close(uv_handle_t * h){
  free(h);
}

void remove_socket(uv_tcp_t * socket){
  uv_close((uv_handle_t *) socket, on_close);
}
