#include <uv.h>
#include "timer.h"
#include "logger.h"
#include "utils.h"
#include <assert.h>

c_timer_t * make_timer(unsigned long id){
  c_timer_t * timer = (c_timer_t *)malloc(sizeof(c_timer_t));
  assert(timer);
  timer->timer_id = id;
  int r = uv_timer_init(uv_default_loop(), (uv_timer_t *)timer);
  if(r) err(uv_error_msg());
  return timer;
}

static void on_timeout(uv_timer_t * timer, int status){
  unsigned long t_id = ((c_timer_t*)timer)->timer_id;
  event_notify(timer_event(t_id, "timeout"), t_id);
}

void start_timer(c_timer_t * timer, unsigned long delay){
  int r = uv_timer_start((uv_timer_t *)timer, on_timeout, delay, 0);
  if(r) err(uv_error_msg());
}

void stop_timer(c_timer_t * timer){
  int r = uv_timer_stop((uv_timer_t *)timer);
  if(r) err(uv_error_msg());
}

static void on_close(uv_handle_t * h){
  free(h);
}

void remove_timer(c_timer_t * timer){
  uv_close((uv_handle_t *)timer, on_close);
}
