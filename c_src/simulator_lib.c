#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <erl_nif.h>
#include <stdbool.h>
#include <assert.h>
#include <search.h>
#include <time.h>
#include "simulator_lib.h"

const double fg_r = 0xd7 / 255.0;
const double fg_g = 0xd6 / 255.0;
const double fg_b = 0xd5 / 255.0;

// Shared variables

double min_x;
double max_x;
double min_y;
double max_y;
double neighbour_distance;
double x_scale_factor;
double y_scale_factor;

double offset_x = 0;
double offset_y = 0;
double scale = 1;

int players = 0;
uplayer_t *up[MAX_PLAYERS];
rplayer_t rp[MAX_PLAYERS];

SDL_mutex *simulator_lock;
Uint32 next_game_ticks;
S2D_Window *window;

int random_int(int from, int to) {
  unsigned int delta = to - from;
  unsigned int r = rand() / (RAND_MAX / delta);
  return from + r;
}

double random_double(double from, double to) {
  double delta = to - from;
  double r = (double)rand() / (RAND_MAX / fabs(delta));
  return from + r;
}

double vector_length(double x1, double y1, double x2, double y2) {
  return sqrtf(powf(x2 - x1, 2) + powf(y2 - y1, 2));
}

void hinsert(char *name, uplayer_t *up0) {
  ENTRY item;
  item.key = name,
  item.data = up0;
  hsearch(item, ENTER);
}

bool hlookup(char *name, uplayer_t **up0) {
  ENTRY item, *found_item;
  item.key = name;
  if((found_item = hsearch(item, FIND)) != NULL) {
    *up0 = found_item->data;
    return true;
  } else {
    return false;
  }
}

// https://gist.github.com/badboy/6267743
unsigned long mix(unsigned long a, unsigned long b, unsigned long c) {
  a = a - b; a = a - c; a = a ^ (c >> 13);
  b = b - c; b = b - a; b = b ^ (a << 8);
  c = c - a; c = c - b; c = c ^ (b >> 13);
  a = a - b; a = a - c; a = a ^ (c >> 12);
  b = b - c; b = b - a; b = b ^ (a << 16);
  c = c - a; c = c - b; c = c ^ (b >> 5);
  a = a - b; a = a - c; a = a ^ (c >> 3);
  b = b - c; b = b - a; b = b ^ (a << 10);
  c = c - a; c = c - b; c = c ^ (b >> 15);
  return c;
}

void scale_to_window(double x, double y, double *wx, double *wy) {
  *wx = (x - min_x) / x_scale_factor;
  *wx = (*wx + offset_x) * scale;
  *wy = -(y - min_y) / y_scale_factor + WINDOW_HEIGHT;
  *wy = (*wy + offset_y) * scale;
}

void draw_line(GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2) {
  S2D_DrawLine(x1, y1, x2, y2, 1,
               fg_r, fg_g, fg_b, 1,
               fg_r, fg_g, fg_b, 1,
               fg_r, fg_g, fg_b, 1,
               fg_r, fg_g, fg_b, 1);
}

void draw_neighbour_distance() {
  double wx1 = WINDOW_WIDTH / 20;
  double wy1 = WINDOW_HEIGHT - WINDOW_HEIGHT / 20;
  double distance = neighbour_distance / x_scale_factor * scale;
  double wx2 = wx1 + distance;
  double wy2 = wy1;
  draw_line(wx1, wy1, wx2, wy2);
}

void draw_quad(GLfloat x1, GLfloat y1,
               GLfloat x2, GLfloat y2,
               GLfloat x3, GLfloat y3,
               GLfloat x4, GLfloat y4) {
  S2D_DrawQuad(x1, y1, fg_r, fg_g, fg_b, 1,
               x2, y2, fg_r, fg_g, fg_b, 1,
               x3, y3, fg_r, fg_g, fg_b, 1,
               x4, y4, fg_r, fg_g, fg_b, 1);
}

void draw_player(double x, double y) {
  double wx, wy;
  scale_to_window(x, y, &wx, &wy);
  if (wx > 0 && wx < WINDOW_WIDTH && wy > 0 && wy < WINDOW_HEIGHT) {
    double size = 4;
    draw_quad(wx - size, wy - size,
              wx + size, wy - size,
              wx + size, wy + size,
              wx - size, wy + size);
  }
}

void draw_neighbour_link(double x1, double y1, double x2, double y2) {
  double wx1, wy1;
  scale_to_window(x1, y1, &wx1, &wy1);
  double wx2, wy2;
  scale_to_window(x2, y2, &wx2, &wy2);
  if ((wx1 > 0 && wx1 < WINDOW_WIDTH && wy1 > 0 && wy1 < WINDOW_HEIGHT) ||
      (wx2 > 0 && wx2 < WINDOW_WIDTH && wy2 > 0 && wy2 < WINDOW_HEIGHT)) {
    draw_line(wx1, wy1, wx2, wy2);
  }
}

void draw_buffer(S2D_Text *label, int buffer_size) {
  if (label->x > 0 && label->x < WINDOW_WIDTH &&
      label->y > 0 && label->y < WINDOW_HEIGHT) {
    double width = 4;
    double height = label->height * ((float)buffer_size / MAX_BUFFER_MESSAGES);
    double half_width = width / 2;
    double half_height = height / 2;
    double x = label->x + label->width + width + width / 2;
    double y = label->y + label->height / 2;
    S2D_DrawQuad(x - half_width, y - half_height, 1, 0, 0, 1,
                 x + half_width, y - half_height, 1, 0, 0, 1,
                 x + half_width, y + half_height, 0, 1, 0, 1,
                 x - half_width, y + half_height, 0, 1, 0, 1);
  }
}

void set_color(S2D_Color *color, GLfloat r, GLfloat g, GLfloat b) {
  color->r = r;
  color->g = g;
  color->b = b;
}

void set_predefined_color(S2D_Color *color, int color_name) {
  // https://wiki.qt.io/Colors_and_Font_Guidelines
  float ff = 255;
  switch (color_name) {
    case FOREGROUND_COLOR:
      set_color(color, 0xd7 / ff, 0xd6 / ff, 0xd5 / ff);
      break;
    case BACKGROUND_COLOR:
      set_color(color, 0x35 / ff, 0x32 / ff, 0x2f / ff);
      break;
    case ACCENT_COLOR1:
      set_color(color, 0xb4 / ff, 0, 0);
      break;
    case ACCENT_COLOR2:
      set_color(color, 0xe0 / ff, 0xc3 / ff, 0x1e / ff);
      break;
    case ACCENT_COLOR3:
      set_color(color, 0x14 / ff, 0xaa / ff, 1);
      break;
    default:
      assert(false);
  }
}

void make_label(uplayer_t *up0, rplayer_t *rp0) {
  if (up0->label == NULL) {
#if defined(__APPLE__) && defined(__MACH__)
    char font[] = "/Library/Fonts/Arial.ttf";
#else
    char font[] = "/usr/share/fonts/truetype/ubuntu/Ubuntu-R.ttf";
#endif
    up0->label = S2D_CreateText(font, up0->name, 12);
  }
  if (up0->pick_mode != rp0->pick_mode) {
    switch (up0->pick_mode) {
      case IS_NOTHING:
        set_predefined_color(&up0->label->color, FOREGROUND_COLOR);
        break;
      case IS_FORWARDER:
        set_predefined_color(&up0->label->color, ACCENT_COLOR2);            
        break;
      case IS_SOURCE:
        set_predefined_color(&up0->label->color, ACCENT_COLOR1);
        break;
      case IS_TARGET:
        set_predefined_color(&up0->label->color, ACCENT_COLOR3);
        break;
      default:
        assert(false);
    }
  }

  if (up0->buffer_size != rp0->buffer_size) {
    char string[128];
    sprintf(string, "%s:%d", up0->name, up0->buffer_size);
    S2D_SetText(up0->label, string);
  }
}

void draw_label(S2D_Text *label, double x, double y) {
  double wx, wy;
  scale_to_window(x, y, &wx, &wy);
  label->x = wx - label->width / 2;
  label->y = wy; // - label->height / 2;
  S2D_DrawText(label);
}

void render() {
  if (SDL_TryLockMutex(simulator_lock) != 0) {
    //fprintf(stderr, "Skipped a render tick!\n");
    return;
  }

  draw_neighbour_distance();

  Uint32 ticks = SDL_GetTicks();
  for (int i = 0; i < players; i++) {
    if (up[i]->is_zombie) {
      continue;
    }

    // Draw markers (debug only)
    /*
    draw_player(up[i]->x0, up[i]->y0);
    if (up[i]->is_set1) {
      draw_player(up[i]->x1, up[i]->y1);
    }
    */

    if (!rp[i].is_set0 && up[i]->is_set0) {
      rp[i].x0 = up[i]->x0;
      rp[i].y0 = up[i]->y0;
      rp[i].x = up[i]->x0;
      rp[i].y = up[i]->y0;
      rp[i].ticks0 = up[i]->ticks0;
      rp[i].is_set0 = true;
      rp[i].buffer_size = 0;
      rp[i].pick_mode = IS_NOTHING;
      continue;
    }

    if (up[i]->reset_render_interpolation) {
      up[i]->reset_render_interpolation = false;
      rp[i].x0 = rp[i].x;
      rp[i].y0 = rp[i].y;
      rp[i].ticks0 = rp[i].ticks;
      rp[i].x1 = up[i]->x1;
      rp[i].y1 = up[i]->y1;
      rp[i].is_set1 = true;
      rp[i].ticks1 = up[i]->ticks1;
      rp[i].angle = atan2(rp[i].y1 - rp[i].y0, rp[i].x1 - rp[i].x0);
      rp[i].total_distance =
        vector_length(rp[i].x0, rp[i].y0, rp[i].x1, rp[i].y1);
      rp[i].total_ticks = rp[i].ticks1 - rp[i].ticks0;
    }

    if (rp[i].is_set0 && rp[i].is_set1) {
      // Travel on linear interpolation equation
      Uint32 ticks_left = ticks - rp[i].ticks1;
      if (ticks_left > rp[i].total_ticks) {
        rp[i].x = rp[i].x1;
        rp[i].y = rp[i].y1;
        rp[i].ticks = rp[i].ticks1;
      } else {
        double f = (double)ticks_left / rp[i].total_ticks * rp[i].total_distance;
        rp[i].x = cos(rp[i].angle) * f + rp[i].x0;
        rp[i].y = sin(rp[i].angle) * f + rp[i].y0;
        rp[i].ticks = rp[i].ticks1 - ticks_left;
      }

      make_label(up[i], &rp[i]);
      draw_label(up[i]->label, rp[i].x, rp[i].y);

      draw_buffer(up[i]->label, up[i]->buffer_size);

      // Draw lines between neighbouring players
      for (int j = 0; j < up[i]->number_of_neighbours; j++) {
        rplayer_t *neighbour = &rp[up[i]->neighbours[j]];
        draw_neighbour_link(rp[i].x, rp[i].y, neighbour->x, neighbour->y);
      }

      rp[i].buffer_size = up[i]->buffer_size;
      rp[i].pick_mode = up[i]->pick_mode;

      //draw_player(rp[i].x, rp[i].y);
    }
  }

  SDL_UnlockMutex(simulator_lock);
}

// FIXME: Center on scaling
void on_key(S2D_Event e) {
  if (e.type == S2D_KEY_UP) {
    double viewport_width, viewport_height;
    double jump_x, jump_y;
    double updated_offset_x, updated_offset_y;
    switch (*e.key) {
      // Zoom in
      case 'A':
        scale += SCALE_JUMP;
        if (scale > MAX_SCALING) {
          scale = 10;
        }
        break;
      // Zooom out
      case 'S':
        scale -= SCALE_JUMP;
        if (scale < 1) {
          scale = 1;
        }
        break;
      // Left arrow
      case 'L':
        viewport_width = WINDOW_WIDTH / scale;
        jump_x = viewport_width / 10;
        offset_x = offset_x - jump_x;
        break;
      // Right arrow
      case 'R':
        viewport_width = WINDOW_WIDTH / scale;
        jump_x = viewport_width / 10;
        offset_x = offset_x + jump_x;
        break;
      // Down arrow
      case 'D':
        viewport_height = WINDOW_HEIGHT/ scale;
        jump_y = viewport_height / 10;
        offset_y = offset_y + jump_y;
        break;
      // Up arrow
      case 'U':
        viewport_height = WINDOW_HEIGHT/ scale;
        jump_y = viewport_height / 10;
        offset_y = offset_y - jump_y;
        break;
    }
  }
}

void init_simulator(void) {
  SDL_LockMutex(simulator_lock);
  next_game_ticks = SDL_GetTicks();
  assert(hcreate(MAX_PLAYERS) != 0);
  time_t t = time(NULL);
  struct tm tm = *localtime(&t);
  char date[256];
  sprintf(date, "Obscrete run: %d-%d-%d %d:%d:%d", tm.tm_year + 1900,
          tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
  window = S2D_CreateWindow(date, WINDOW_WIDTH, WINDOW_HEIGHT, NULL, render, 0);
  set_predefined_color(&window->background, BACKGROUND_COLOR);
  //window->vsync = false;
  window->on_key = on_key;
  SDL_UnlockMutex(simulator_lock);
  S2D_Show(window);
}

void stop_simulator(void) {
  S2D_Close(window);
}
