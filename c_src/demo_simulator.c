#include <stdbool.h>
#include <assert.h>
#include <time.h>
#include <SDL.h>
#include "simulator_lib.h"

// Variables declared in simulator_lib.c

extern double min_x;
extern double max_x;
extern double min_y;
extern double max_y;
extern double x_scale_factor;
extern double y_scale_factor;

extern int players;
demo_uplayer_t *demo_up[1024];

extern SDL_mutex *simulator_lock;

int init_demo(void *data) {
  min_x = -HALF_WINDOW_WIDTH;
  max_x = HALF_WINDOW_WIDTH;
  min_y = -HALF_WINDOW_HEIGHT;
  max_y = HALF_WINDOW_HEIGHT;
  x_scale_factor = 1;
  y_scale_factor = 1;

  SDL_LockMutex(simulator_lock);

  players = 64;
  for (int i = 0; i < players; i++) {
    demo_up[i] = (demo_uplayer_t *)malloc(sizeof(demo_uplayer_t));
    sprintf(demo_up[i]->name, "p%d", i);
    demo_up[i]->self_index = i;
    demo_up[i]->angle = random_double(0, 2 * PI);
    demo_up[i]->n = 0;
    demo_up[i]->x1 = random_int(-HALF_WINDOW_WIDTH, HALF_WINDOW_WIDTH);
    demo_up[i]->y1 = random_int(-HALF_WINDOW_HEIGHT, HALF_WINDOW_HEIGHT);
    demo_up[i]->x2 = NOT_SET;
    if (random_int(0, 100) > 95) {
      demo_up[i]->number_of_neighbours = 2;
      demo_up[i]->neighbours[0] = random_int(0, players - 1);
      demo_up[i]->neighbours[1] = random_int(0, players - 1);
    } else {
      demo_up[i]->number_of_neighbours = 0;
    }
    demo_up[i]->label = NULL;
    demo_up[i]->buffer_size = 1.0; //random_double(0, 1);
  }
  SDL_UnlockMutex(simulator_lock);
  return 0;
}

int main() {
  //setbuf(stdout, NULL);
  unsigned long seed = mix(clock(), time(NULL), getpid());
  srand(seed);
  simulator_lock = SDL_CreateMutex();
  SDL_Thread *thread = SDL_CreateThread(init_demo, "demo", (void *)NULL);
  assert(thread != NULL);
  init_simulator(true);
  return 0;
}
