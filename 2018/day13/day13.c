#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

//
// globals
//

int WIDTH = 0;
int HEIGHT = 0;
int NUM_CARTS = 0;

char *map = NULL;
struct cart * carts = NULL;

// char MAP(int,int)
#define MAP(x,y) (map[(y*WIDTH)+x])


//
// directions
//

#define U 0
#define R 1
#define D 2
#define L 3

#define TURN_RIGHT(x) (x=(((x)+1)%4))
#define TURN_LEFT(x) (x=(((x-1)+4)%4))

typedef char Dir;

const char *
print_dir(Dir d)
{
  switch (d) {
  case U: return "UP";
  case D: return "DOWN";
  case L: return "LEFT";
  case R: return "RIGHT";
  }
  return NULL;
}

//
// intersection turns
//

#define LEFT 0
#define STRAIGHT 1
#define RIGHT 2
#define NEXT_INTERSECTION_TURN(x) (x=(((x)+1)%3))

typedef char Turn;

const char *
print_turn(Turn d)
{
  switch (d) {
  case LEFT: return "LEFT";
  case STRAIGHT: return "STRAIGHT";
  case RIGHT: return "RIGHT";
  }
  return NULL;
}

//
// carts
//

struct
cart
{
  int x;
  int y;
  Dir dir;
  Turn turn;
  bool active;
};

inline void
make_cart(struct cart * cart, int x, int y, Dir d)
{
  cart->x = x;
  cart->y = y;
  cart->dir = d;
  cart->turn = LEFT;
  cart->active = true;
}

// return -1 if c1p comes first, 0 if equal, 1 if c2p comes first
int
sort_carts_cmp(const void * c1p, const void * c2p)
{
  struct cart * c1 = (struct cart *) c1p;
  struct cart * c2 = (struct cart *) c2p;

  if (!c1->active && !c2->active) return 0;
  else if (! c1->active) return 1;
  else if (! c2->active) return -1;
  else {
    int dy = (c1->y) - (c2->y);
    if (dy == 0) return (c1->x) - (c2->x);
    else return dy;
  }
}

inline void
sort_carts(struct cart * carts)
{
  qsort((void*)carts, NUM_CARTS, sizeof(struct cart), sort_carts_cmp);
}

bool
check_collision(int k)
{
  for (int i = 0; i < NUM_CARTS; ++i) {
    int kx = carts[k].x;
    int ky = carts[k].y;
    if (k != i &&
        kx == carts[i].x &&
        ky == carts[i].y)
      {
        printf("collision at %d,%d\n\n", kx, ky);
        return true;
      }
  }
  return false;
}

bool
check_collision2(int k)
{
  for (int i = 0; i < NUM_CARTS; ++i) {
    int kx = carts[k].x;
    int ky = carts[k].y;
    if (k != i &&
        carts[k].active &&
        carts[i].active &&
        kx == carts[i].x &&
        ky == carts[i].y)
      {
        printf("collision at %d,%d\n\n", kx, ky);
        carts[k].active = false;
        carts[i].active = false;
        return true;
      }
  }
  return false;
}

//
// main
//

void
print_cart(struct cart * k)
{
  switch (k->dir) {
  case U: putchar('^'); break;
  case D: putchar('v'); break;
  case L: putchar('<'); break;
  case R: putchar('>'); break;
  }
}

void
print_state()
{
  for (int y = 0; y < HEIGHT; ++y) {
    for (int x = 0; x < WIDTH; ++x) {
      bool is_cart = false;
      for (int k = 0; k < NUM_CARTS; ++k) {
        if (carts[k].x == x && carts[k].y == y) {
          is_cart = true;
          print_cart(&carts[k]);
        }
      }
      if (! is_cart)
        putchar(MAP(x, y));
    }
    putchar('\n');
  }
  putchar('\n');
}

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

void
print_carts_state()
{
  printf("There are %d carts\n", NUM_CARTS);
  for (int i = 0; i < NUM_CARTS; ++i) {
    printf("Cart %3d: active=%d, x=%3d, y=%3d, dir=%s\n", i, carts[i].active, carts[i].x, carts[i].y, print_dir(carts[i].dir));
  }
  putchar('\n');
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
        if (c == '<' || c == '>' || c == '^' || c == 'v')
          NUM_CARTS++;
        widthl++;
      }
    }

    rewind(fp);

    map = (char*) calloc(WIDTH*HEIGHT, sizeof(char));
    carts = (struct cart*) calloc(NUM_CARTS, sizeof(struct cart));

    // actually parse it
    int x = 0;
    int y = 0;
    int cur_cart = 0;
    while ((c = getc(fp)) != EOF) {
      if (c =='\n') {
        for (; x < WIDTH; ++x) MAP(x,y) = ' ';
        y++;
        x = 0;
      }
      else {
        switch (c) {
        case '<':
          make_cart(&carts[cur_cart++], x, y, L);
          MAP(x,y) = '-';
          break;

        case '>':
          make_cart(&carts[cur_cart++], x, y, R);
          MAP(x,y) = '-';
          break;

        case '^':
          make_cart(&carts[cur_cart++], x, y, U);
          MAP(x,y) = '|';
          break;

        case 'v':
          make_cart(&carts[cur_cart++], x, y, D);
          MAP(x,y) = '|';
          break;

        default:
          MAP(x,y) = c;
          break;
        }
        x++;
      }
    }
    assert(y==HEIGHT);
    fclose(fp);
  }

  //  print_state();
  // print_carts_state();

  while (1) {
    //    print_state();

    sort_carts(carts);
    // after removing all colliding pairs one will remain at carts[0]
    if (carts[0].active && !carts[1].active) {
      printf("last cart at: %d,%d\n\n", carts[0].x, carts[0].y);
      goto done;
    }

    for (int i = 0; i < NUM_CARTS; ++i) {
      struct cart * k = &carts[i];
      if (! k->active) continue;

      char cur;

      // move
      cur = MAP(k->x, k->y);
      switch (cur) {
      case '|':
        switch (k->dir) {
        case U: k->y--; break;
        case D: k->y++; break;
        default: goto fail;
        }
        break;

      case '-':
        switch (k->dir) {
        case L: k->x--; break;
        case R: k->x++; break;
        default: goto fail;
        }
        break;

      case '/':
      case '\\':
      case '+':
        switch (k->dir) {
        case U: k->y--; break;
        case L: k->x--; break;
        case D: k->y++; break;
        case R: k->x++; break;
        }
        break;
      } // end switch(cur)

      //      if (check_collision(i)) goto done;
      if (check_collision2(i)) continue;

      // try turn
      cur = MAP(k->x, k->y);
      switch (cur) {
      case '/':
        switch (k->dir) {
          // moving up, go right
        case U: TURN_RIGHT(k->dir); break;
          // moving left FROM RIGHT, go down
        case L: TURN_LEFT(k->dir); break;
          // go left
        case D: TURN_RIGHT(k->dir); break;
          // moving right FROM LEFT, go up
        case R: TURN_LEFT(k->dir); break;
        }
        break;

      case '\\':
        switch (k->dir) {
          // gg left
        case U: TURN_LEFT(k->dir); break;
          // moving left FROM RIGHT, go up
        case L: TURN_RIGHT(k->dir); break;
          // go right
        case D: TURN_LEFT(k->dir); break;
          // go down
        case R: TURN_RIGHT(k->dir); break;
        }
        break;

      case '+':
        switch (k->turn) {
        case LEFT: TURN_LEFT(k->dir); break;
        case RIGHT: TURN_RIGHT(k->dir); break;
        case STRAIGHT: break;
        }
        NEXT_INTERSECTION_TURN(k->turn);
        break;

      default:
        break;
      }

      continue;


    fail:
      fprintf(stderr, "Invalid move: cart[%d] at (%d,%d) going %s but map piece is %c\n", i, k->x, k->y, print_dir(k->dir), cur);
      free(map);
      free(carts);
      return EXIT_FAILURE;

    } // end for(k in carts)

  } // end while(1)

 done:
  print_carts_state();

  free(map);
  free(carts);

  return EXIT_SUCCESS;
}
