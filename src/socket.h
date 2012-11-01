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


#ifndef __SOCKET_H__
#define __SOCKET_H__
#include <uv.h>
#include <chicken.h>

uv_tcp_t * make_socket(unsigned long);
void socket_bind(uv_tcp_t *, char *, int);
void socket_listen(uv_tcp_t *);
void socket_connect(uv_tcp_t *, char *, int, unsigned long);
void socket_read(uv_tcp_t *);
void socket_read_stop(uv_tcp_t * socket);
void socket_write(uv_tcp_t *, char *, int, unsigned long);
void remove_socket(uv_tcp_t *);

#endif
