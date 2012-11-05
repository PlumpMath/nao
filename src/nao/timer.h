#ifndef __TIMER_H__
#define __TIMER_H__

#include <uv.h>

typedef struct {
  uv_timer_t timer;
  unsigned long timer_id;
} c_timer_t;

c_timer_t * make_timer(unsigned long);
void start_timer(c_timer_t *, unsigned long);
void stop_timer(c_timer_t *);
void remove_timer(c_timer_t *);

#endif
