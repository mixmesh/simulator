#include <errno.h>
#include <erl_nif.h>
#include <assert.h>
#include <time.h>
#include "simulator_lib.h"

// Variables declared in simulator_lib.c

extern double min_x;
extern double max_x;
extern double min_y;
extern double max_y;
extern double neighbour_distance;
extern double x_scale_factor;
extern double y_scale_factor;

extern int players;
extern uplayer_t *up[MAX_PLAYERS];
extern rplayer_t rp[MAX_PLAYERS];

extern SDL_mutex *simulator_lock;

/*
 * add_players
 */

bool get_double(ErlNifEnv* env, ERL_NIF_TERM term, double* d) {
  if (!enif_get_double(env, term, d)) {
    int i;
    if (!enif_get_int(env, term, &i)) {
      return false;
    } else {
      *d = (double)i;
      return true;
    }
  }
}

void release_players(int i) {
  for (int j = players; j <= i; j++) {
    free(up[j]);
  }
}

static ERL_NIF_TERM _add_players(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }

  SDL_LockMutex(simulator_lock);

  ERL_NIF_TERM list = argv[0];
  ERL_NIF_TERM head;

  // Read list with {?name, ?x, ?y} tuples
  int i = players;
  while (enif_get_list_cell(env, list, &head, &list)) {
    // Allocate memory for player
    up[i] = (uplayer_t *)malloc(sizeof(uplayer_t));

    // Read tuple
    int arity;
    const ERL_NIF_TERM *tuple;
    if (!enif_get_tuple(env, head, &arity, &tuple)) {
      release_players(i);
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }
    if (arity != 3) {
      release_players(i);
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Read name
    if (enif_get_string(env, tuple[0], up[i]->name, 64, ERL_NIF_LATIN1) < 0) {
      release_players(i);
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Read x
    double x;
    if (!get_double(env, tuple[1], &x)) {
      release_players(i);
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Read y
    double y;
    if (!get_double(env, tuple[2], &y)) {
      release_players(i);
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Initialize player
    up[i]->self_index = i;
    up[i]->x0 = x;
    up[i]->y0 = y;
    up[i]->ticks0 = SDL_GetTicks();
    up[i]->is_set0 = true;
    up[i]->is_set1 = false;
    up[i]->number_of_neighbours = 0;
    up[i]->label = NULL;
    up[i]->buffer_size = 0;
    up[i]->is_zombie = false;
    up[i]->pick_mode = IS_NOTHING;
    up[i]->reset_render_interpolation = false;

    // Add player in hash table
    hinsert(up[i]->name, up[i]);

    ++i;
  }

  players = i;
  SDL_UnlockMutex(simulator_lock);
  return enif_make_atom(env, "ok");
}

/*
 * start
 */

static ERL_NIF_TERM _initialize(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
  if (!get_double(env, argv[0], &min_x)) {
    return enif_make_badarg(env);
  }

  if (!get_double(env, argv[1], &max_x)) {
    return enif_make_badarg(env);
  }

  if (!get_double(env, argv[2], &min_y)) {
    return enif_make_badarg(env);
  }

  if (!get_double(env, argv[3], &max_y)) {
    return enif_make_badarg(env);
  }

  if (!get_double(env, argv[4], &neighbour_distance)) {
    return enif_make_badarg(env);
  }

  x_scale_factor = (max_x - min_x) / WINDOW_WIDTH;
  y_scale_factor = (max_y - min_y) / WINDOW_HEIGHT;

  for (int i = 0; i < MAX_PLAYERS; i++) {
    rp[i].is_set0 = false;
    rp[i].is_set1 = false;
  }

  return enif_make_atom(env, "ok");
}

/*
 * update_players
 */

static ERL_NIF_TERM _update_players(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  if (!enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }

  SDL_LockMutex(simulator_lock);
  Uint32 ticks = SDL_GetTicks();

  // Read list with {?name, updated_values} tuples
  ERL_NIF_TERM updated_players_list = argv[0];
  ERL_NIF_TERM updated_players_head;
  while (enif_get_list_cell(env, updated_players_list, &updated_players_head,
                            &updated_players_list)) {
    // Read tuple {?name, updated_values} tuple
    int arity;
    const ERL_NIF_TERM *updated_player_tuple;
    if (!enif_get_tuple(env, updated_players_head, &arity,
                        &updated_player_tuple)) {
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }
    if (arity != 2) {
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Read name
    char name[64];
    if (enif_get_string(env, updated_player_tuple[0], name, 64,
                        ERL_NIF_LATIN1) < 0) {
      SDL_UnlockMutex(simulator_lock);
      return enif_make_badarg(env);
    }

    // Lookup player (must exist!)
    uplayer_t *named_up;
    assert(hlookup(name, &named_up));

    // Read list with {?tag, ?updated_value} tuples
    ERL_NIF_TERM updated_values_list = updated_player_tuple[1];
    ERL_NIF_TERM updated_values_head;
    while (enif_get_list_cell(env, updated_values_list, &updated_values_head,
                              &updated_values_list)) {
      // Read {?tag, ?updated_value} tuple
      int arity;
      const ERL_NIF_TERM *updated_value_tuple;
      if (!enif_get_tuple(env, updated_values_head, &arity,
                          &updated_value_tuple)) {
        SDL_UnlockMutex(simulator_lock);
        return enif_make_badarg(env);
      }
      if (arity != 2) {
        SDL_UnlockMutex(simulator_lock);
        return enif_make_badarg(env);
      }

      // Read tag
      char tag[64];
      if (enif_get_atom(env, updated_value_tuple[0], tag, 64,
                        ERL_NIF_LATIN1) < 0) {
        SDL_UnlockMutex(simulator_lock);
        return enif_make_badarg(env);
      }

      if (strcmp(tag, "x") == 0) {
        // Read x value
        double x;
        if (!get_double(env, updated_value_tuple[1], &x)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }

        // Note: The next updated value must be y!
        assert(enif_get_list_cell(env, updated_values_list,
                                  &updated_values_head, &updated_values_list));

        // Read y tuple
        int arity;
        const ERL_NIF_TERM *y_tuple;
        if (!enif_get_tuple(env, updated_values_head, &arity, &y_tuple)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }
        if (arity != 2) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }

        // Read tag
        char tag[64];
        if (enif_get_atom(env, y_tuple[0], tag, 64, ERL_NIF_LATIN1) < 0) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }
        assert(strcmp(tag, "y") == 0);

        // Read y value
        double y;
        if (!get_double(env, y_tuple[1], &y)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }

        // Update player's x, y coordinates
        if (!named_up->is_set1) {
          named_up->x1 = x;
          named_up->y1 = y;
          named_up->ticks1 = ticks;
          named_up->is_set1 = true;
          //named_up->reset_render_interpolation = true;
        } else {
          named_up->x0 = named_up->x1;
          named_up->y0 = named_up->y1;
          named_up->x1 = x;
          named_up->y1 = y;
          named_up->ticks1 = ticks;
          named_up->reset_render_interpolation = true;
        }
      } else if (strcmp(tag, "neighbours") == 0) {
        // Read list with neighbour names
        ERL_NIF_TERM neighbour_list = updated_value_tuple[1];
        ERL_NIF_TERM neighbour_head;
        int n = 0;
        while (enif_get_list_cell(env, neighbour_list, &neighbour_head,
                                  &neighbour_list)) {
          // Read neighbour name
          char name[64];
          if (enif_get_string(env, neighbour_head, name, 64,
                              ERL_NIF_LATIN1) < 0) {
            SDL_UnlockMutex(simulator_lock);
            return enif_make_badarg(env);
          }
          uplayer_t *neighbour_up;
          assert(hlookup(name, &neighbour_up));
          named_up->neighbours[n++] = neighbour_up->self_index;
        }
        named_up->number_of_neighbours = n;
      } else if (strcmp(tag, "buffer_size") == 0) {
        // Read buffer_size
        if (!enif_get_int(env, updated_value_tuple[1], &named_up->buffer_size)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }
      } else if (strcmp(tag, "is_zombie") == 0) {
        // Read is_zombie
        int i;
        if (!enif_get_int(env, updated_value_tuple[1], &i)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }
        named_up->is_zombie = i;
      } else if (strcmp(tag, "pick_mode") == 0) {
        // Read pick_mode
        int pick_mode;
        if (!enif_get_int(env, updated_value_tuple[1], &pick_mode)) {
          SDL_UnlockMutex(simulator_lock);
          return enif_make_badarg(env);
        }
        named_up->pick_mode = pick_mode;
      } else {
        fprintf(stderr, "Unknown tag: %s\n", tag);
        assert(false);
      }
    }
  }
  
  SDL_UnlockMutex(simulator_lock);
  return enif_make_atom(env, "ok");
}

// Initialize NIF functions

static void *load_simulator(void *arg) {
  init_simulator();
  return NULL;
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  // Seed random generator
  unsigned long seed = mix(clock(), time(NULL), getpid());
  srand(seed);

  // Create mutex used to protect shared variables (see above)
  simulator_lock = SDL_CreateMutex();

  // Start main thread
  ErlNifTid *tid = enif_alloc(sizeof(ErlNifTid));
#if defined(__APPLE__) && defined(__MACH__)
  // On OSX we identify ourselves as the main thread to ensure that
  // we are compatible with libraries that require it. For example
  // this is necessary with SDL2 in order to receive input events.
  // We use the undocumented erl_drv_steal_main_thread because SDL
  // must run in the main thread on macOS.
  erl_drv_steal_main_thread("simulator", tid, load_simulator, NULL, NULL);
#else
  enif_thread_create("simulator", tid, load_simulator, NULL, NULL);
#endif

  *priv_data = (void *)tid;
  return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
  ErlNifTid *tid = (ErlNifTid *)priv_data;
  stop_simulator();
  enif_thread_join(*tid, NULL);
  enif_free(tid);
}

static ErlNifFunc nif_funcs[] = {
  {"add_players", 1, _add_players},
  {"initialize", 5, _initialize},
  {"update_players", 1, _update_players},
};

ERL_NIF_INIT(simulator_nif, nif_funcs, load, NULL, NULL, unload)
