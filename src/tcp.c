#include "tcp.h"
#include <uv.h>
#include "logger.h"
#include "utils.h"
#include <assert.h>

typedef struct {
  uv_tcp_t orgin;
  C_word user_data;
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
        uv_close((uv_handle_t *) client, NULL);
    } else {
      if(uv_accept(socket, (uv_stream_t *)client) == 0){
        event_notify(((c_tcp_t*)socket)->user_data, client);
      } else {
        warn(uv_error_msg());
        uv_close((uv_handle_t *) client, NULL);
      }
    }
  }
}

void socket_listen(uv_tcp_t * socket, C_word event){
  int r = uv_listen((uv_stream_t *) socket, 128, on_new_connection);
  if(r) err(uv_error_msg());
  ((c_tcp_t*)socket)->user_data = event;
}

void remove_socket(uv_tcp_t * socket){
  uv_close((uv_handle_t *) socket, NULL);
}
