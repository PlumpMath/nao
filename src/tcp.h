#ifndef __TCP_H__
#define __TCP_H__
#include <uv.h>
#include <chicken.h>

uv_tcp_t * make_socket();
void socket_bind(uv_tcp_t *, char *, int);
void socket_listen(uv_tcp_t *, C_word);
void remove_socket(uv_tcp_t *);

#endif
