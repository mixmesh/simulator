#ifndef SIMULATOR_LIB_H_
#define SIMULATOR_LIB_H_

#include <stdbool.h>
#include <simple2d.h>
#include <erl_driver.h>
#include <erl_nif.h>

#define PI 3.14159265358979323846

#define WINDOW_WIDTH 1000.0
#define WINDOW_HEIGHT 1000.0
#define HALF_WINDOW_WIDTH WINDOW_WIDTH / 2
#define HALF_WINDOW_HEIGHT WINDOW_HEIGHT / 2

#define TICKS_PER_SECOND 1
#define SKIP_TICKS 1000 / TICKS_PER_SECOND
#define SCALE_JUMP 0.2
#define MAX_SCALING 100
#define MAX_PLAYERS 1024

#define MAX_BUFFER_MESSAGES 1000

#define FOREGROUND_COLOR 0
#define BACKGROUND_COLOR 1
#define ACCENT_COLOR1 2
#define ACCENT_COLOR2 3
#define ACCENT_COLOR3 4

#define IS_NOTHING 0
#define IS_FORWARDER 1
#define IS_SOURCE 2
#define IS_TARGET 3

typedef struct {
  // Name and index
  char name[64];
  int self_index;
  // First known position
  double x0;
  double y0;
  Uint32 ticks0;
  bool is_set0;
  // Second known position
  double x1;
  double y1;
  Uint32 ticks1;
  bool is_set1;
  // Current neighbours
  int number_of_neighbours;
  int neighbours[128];
  // Label
  S2D_Text *label;
  // Buffer size
  int buffer_size;
  // Is zombie?
  bool is_zombie;
  // Pick mode (IS_*)
  int pick_mode;
  // Reset render interpolation
  bool reset_render_interpolation;
} uplayer_t;

typedef struct {
  // Start position
  double x0;
  double y0;
  Uint32 ticks0;
  bool is_set0;
  // Buffer size
  int buffer_size;
  // End position
  double x1;
  double y1;
  Uint32 ticks1;
  bool is_set1;
  // Interpolation parameters
  double angle;
  double total_distance;
  Uint32 total_ticks;
  double x;
  double y;
  Uint32 ticks;
  // Pick mode
  int pick_mode;
} rplayer_t;

int random_int(int from, int to);
double random_double(double from, double to);
double vector_length(double x1, double y1, double x2, double y2);
void hinsert(char *name, uplayer_t *up0);
bool hlookup(char *name, uplayer_t **up0);
unsigned long mix(unsigned long a, unsigned long b, unsigned long c);
void add_cabs(uplayer_t *up, int n);
void init_simulator(void);
void stop_simulator(void);

// Is not published in erl_driver.h
int erl_drv_steal_main_thread(char *name,
			      ErlDrvTid *dtid,
			      void *(*func)(void *),
			      void *arg,
			      ErlDrvThreadOpts *opts);

#endif
