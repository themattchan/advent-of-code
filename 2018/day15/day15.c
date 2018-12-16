#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int WIDTH = 0;
int HEIGHT = 0;
int NUM_UNITS = 0;
int NUM_G = 0;
int NUM_E = 0;

char *map = NULL;
struct unit * units = NULL;
// ptrs to entry in units
struct unit ** GS = NULL;
// ptrs to entry in units
struct unit ** ES = NULL;

// char MAP(int,int)
#define MAP(x,y) (map[(y*WIDTH)+x])

void
print_map()
{
  for (int y = 0; y < HEIGHT; ++y) {
    for (int x = 0; x < WIDTH; ++x) {
      putchar(MAP(x,y));
    }
    putchar('\n');
  }
  putchar('\n');
}

enum unit_type {G,E};

const char*
print_unit_type(enum unit_type ty)
{
  switch (ty) {
  case G: return "GOBLIN";
  case E: return "ELF";
  }
  return NULL;
}

struct
unit
{
  int x;
  int y;
  int attack;
  int hit;
  enum unit_type type;
};

// bool DEAD(struct unit *)
#define DEAD(x) (((x)->hit <= 0))

inline void
make_unit(struct unit * unit, struct unit ** ptr, enum unit_type ty, int x, int y)
{
  unit->x = x;
  unit->y = y;
  unit->attack = 3;
  unit->hit = 200;
  unit->type = ty;
  *ptr = unit;
}

bool check_done(int round)
{
  int E_hits = 0;
  int E_remaining = 0;
  int G_hits = 0;
  int G_remaining = 0;
  for (int i = 0; i < NUM_UNITS; ++i) {
    if (units[i].type == E) {
      if (units[i].hit > 0) {
        E_remaining++;
        E_hits += units[i].hit;
      }
    } else {
      if (units[i].hit > 0) {
        G_remaining++;
        G_hits += units[i].hit;
      }
    }
  }

  if (E_remaining == 0 || G_remaining == 0) {
    printf("Combat ends after %d rounds\n", round);
    printf("%s wins with %d total hit points left\n",
           (E_remaining > 0 ? print_unit_type(E) : print_unit_type(G)),
           (E_remaining > 0 ? E_hits : G_hits));
    printf("Outcome: %d\n", round * (E_remaining > 0 ? E_hits : G_hits));
    return true;
  }
  return false;
}


void
print_units()
{
  for (int i = 0; i < NUM_UNITS; ++i) {
    printf("Unit %d: type=%s at %d,%d, attack=%d, hit=%d\n",
           i,
           print_unit_type(units[i].type),
           units[i].x,
           units[i].y,
           units[i].attack,
           units[i].hit);
  }
}

int
main(int argc, char * argv[])
{
  // input parsing
  {
    FILE *fp;
    // preprocessing
    fp = fopen((argc > 1 ? argv[1] : "input"), "r");
    char c;
    int widthl = 0;
    while ((c = getc(fp)) != EOF) {
      if (c == '\n') {
        HEIGHT++;
        if (widthl > WIDTH) WIDTH = widthl;
        widthl = 0;
      }
      else {
        if (c == 'G') {
          NUM_UNITS++;
          NUM_G++;
        }
        if (c == 'E') {
          NUM_UNITS++;
          NUM_E++;
        }
        widthl++;
      }
    }

    rewind(fp);

    map = (char*) calloc(WIDTH*HEIGHT, sizeof(char));
    units = (struct unit*) calloc(NUM_UNITS, sizeof(struct unit));
    GS = (struct unit**) calloc(NUM_G, sizeof(struct unit*));
    ES = (struct unit**) calloc(NUM_E, sizeof(struct unit*));

    // actually parse it
    int x = 0;
    int y = 0;
    int u = 0;
    int g = 0;
    int e = 0;
    while ((c = getc(fp)) != EOF) {
      if (c =='\n') {
        // all lines should have the same length
        assert(WIDTH == x);
        y++;
        x = 0;
      }
      else {
        MAP(x,y) = c;

        switch (c) {
        case 'G':
          make_unit(&units[u++], &GS[g++], G, x, y);
          break;
        case 'E':
          make_unit(&units[u++], &ES[e++], E, x, y);
          break;
        }
        x++;
      }
    }
    fclose(fp);
  }

  //  print_units();
  // print_map();

  int round = 0;
  while (true) {
    if (check_done(round)) break;

    // do it
    for (int i = 0; i < NUM_UNITS; ++i) {
      struct unit ** enemy = units[i].type == G ? ES : GS;

    }

    round++;
  } // while true

 done:
  free(GS);
  free(ES);
  free(units);
  free(map);

  return EXIT_SUCCESS;
}
