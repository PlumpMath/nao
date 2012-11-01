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


#include "tick.h"
#include <uv.h>
#include "utils.h"

static uv_prepare_t prepare_watcher;
static uv_check_t check_watcher;
static uv_idle_t spinner;

static void _run_ticks(){
  run_ticks();
  if(uv_is_active((uv_handle_t*) &spinner) && ticks_empty_p()){
    int r = uv_idle_stop(&spinner);
    if(r) err(uv_error_msg());
  } 
}

static void prepare(uv_prepare_t *handle, int status){
  run_scheduler();
  _run_ticks();
}

static void check(uv_check_t *handle, int status){
  run_scheduler();
  _run_ticks();
}

static void idle(uv_idle_t *handle, int status){
  _run_ticks();
}

void add_tick(){
  if(!uv_is_active((uv_handle_t*) &spinner)){
    int r = uv_idle_start(&spinner, idle);
    if(r) err(uv_error_msg());
  }
}

void init_tick(){

  int r = 0;

  uv_loop_t * loop = uv_default_loop();

  r = uv_prepare_init(loop, &prepare_watcher);
  if(r) err(uv_error_msg());
  r = uv_prepare_start(&prepare_watcher, prepare);
  if(r) err(uv_error_msg());
  uv_unref((uv_handle_t *) &prepare_watcher);

  r = uv_check_init(loop, &check_watcher);
  if(r) err(uv_error_msg());
  r = uv_check_start(&check_watcher, check);
  if(r) err(uv_error_msg());
  uv_unref((uv_handle_t *) &check_watcher);

  r = uv_idle_init(loop, &spinner);
  if(r) err(uv_error_msg());

}
