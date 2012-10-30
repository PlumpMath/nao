#include <uv.h>
#include "tick.h"
#include <chicken.h>

int main(int argc, char **argv){

  uv_loop_t * loop = uv_default_loop();

  init_tick();

  CHICKEN_run(C_toplevel);

  return uv_run(loop);

}
