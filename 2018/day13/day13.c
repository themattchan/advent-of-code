#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdbool.h>

//
// globals
//

int WIDTH = 0;
int HEIGHT = 0;
int NUM_CARTS = 0;

char *map = NULL;
struct cart * carts = NULL;

// char MAP(int,int)
#define MAP(x,y) (map[(y*HEIGHT)+x])


//
// directions
//

#define U 0
#define R 1
#define D 2
#define L 3
#define TURN_RIGHT(x) ((((x)+1)%4))
#define TURN_LEFT(x) ((((x-1)+4)%4))

typedef char Dir;

const char * print_dir(Dir d)
{
  switch (d) {
  case U: return "UP";
  case D: return "DOWN";
  case L: return "LEFT";
  case R: return "RIGHT";
  }
}

//
// intersection turns
//

#define LEFT 0
#define STRAIGHT 1
#define RIGHT 2
#define NEXT_INTERSECTION_TURN(x) ((((x)+1)%3))

typedef char Turn;

const char * print_turn(Turn d)
{
  switch (d) {
  case LEFT: return "LEFT";
  case STRAIGHT: return "STRAIGHT";
  case RIGHT: return "RIGHT";
  }
}

//
// carts
//

struct cart
{
  int x;
  int y;
  Dir dir;
  Turn turn;
};

inline void make_cart(struct cart * cart, int x, int y, Dir d)
{
  cart->x = x;
  cart->y = y;
  cart->dir = d;
  cart->turn = LEFT;
}

// return -1 if c1p comes first, 0 if equal, 1 if c2p comes first
int sort_carts_cmp(const void * c1p, const void * c2p)
{
  struct cart * c1 = (struct cart *) c1p;
  struct cart * c2 = (struct cart *) c2p;

  int dy = (c1->y) - (c2->y);
  if (dy == 0) return (c1->x) - (c2->x);
  else return dy;
}

inline void sort_carts(struct cart * carts)
{
  qsort((void*)carts, NUM_CARTS, sizeof(struct cart), sort_carts_cmp);
}

// INPUT: a sorted carts array
bool check_collision(struct cart *carts)
{
  for (int i = 0; i < NUM_CARTS-1; ++i) {
    if (carts[i].x == carts[i+1].x &&
        carts[i].y == carts[i+1].y)
      {
        printf("collision at %d,%d\n", carts[i].x, carts[i].y);
        return true;
      }
  }
  return false;
}

//
// main
//

int main ()
{
  // input parsing
  {
    FILE *fp;
    // preprocessing
    fp = fopen("input", "r");
    char c;
    int widthl = 0;
    while ((c = getc(fp)) != EOF) {
      if (c == '\n') {
        HEIGHT++;
        if (widthl > WIDTH) WIDTH = widthl;
        widthl = 0;
      }
      else {
        if (c == '<' || c == '>' || c == '^' || c == 'V')
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

        case 'V':
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
    fclose(fp);
  }

  /* for (int i = 0; i < NUM_CARTS; ++i) {
   *   printf("Cart %d: x=%d, y=%d, dir=%d\n", i, carts[i].x, carts[i].y, carts[i].dir);
   * } */

  /* for (int i = 0; i < HEIGHT; ++i) {
   *   for (int j = 0; j < WIDTH; ++j) {
   *     putchar(MAP(j,i));
   *   }
   *   putchar('\n');
   * } */

  while (1) {
    sort_carts(carts);

    if (check_collision(carts)) break;

    for (int i = 0; i < NUM_CARTS; ++i) {
      // turn then move
      struct cart * k = &carts[i];
      char cur = MAP(k->x, k->y);
      switch (cur) {
      case '|':
        switch (k->dir) {
        case U: (k->y)--; break;
        case D: (k->y)++; break;
        default: goto fail;
        }
        break;
      case '-':
        switch (k->dir) {
        case L: (k->x)--; break;
        case R: (k->x)++; break;
        default: goto fail;
        }
        break;

      case '/':
        switch (k->dir) {
          // moving up, go right
        case U: (k->dir) = TURN_RIGHT(k->dir); (k->x)++; break;
          // moving left FROM RIGHT, go down
        case L: (k->dir) = TURN_LEFT(k->dir); (k->y)++; break;
          // go left
        case D: (k->dir) = TURN_RIGHT(k->dir); (k->x)--; break;
          // moving right FROM LEFT, go up
        case R: (k->dir) = TURN_LEFT(k->dir); (k->y)--; break;
        }
        break;

      case '\\':
        switch (k->dir) {
          // gg left
        case U: (k->dir) = TURN_LEFT(k->dir); (k->x)--; break;
          // moving left FROM RIGHT, go up
        case L: (k->dir) = TURN_RIGHT(k->dir); (k->y)--; break;
          // go right
        case D: (k->dir) = TURN_LEFT(k->dir); (k->x)++; break;
          // go down
        case R: (k->dir) = TURN_RIGHT(k->dir); (k->y)++; break;
        }
        break;

      case '+':
        switch (k->turn) {
        case LEFT: (k->dir) = TURN_LEFT(k->dir); break;
        case STRAIGHT: break;
        case RIGHT: (k->dir) = TURN_RIGHT(k->dir); break;
        }
        (k->turn) = NEXT_INTERSECTION_TURN(k->turn);

        switch (k->dir) {
        case U: (k->y)--; break;
        case D: (k->y)++; break;
        case L: (k->x)--; break;
        case R: (k->x)++; break;
        }
        break;
      } // end switch(cur)

      continue;


    fail:
      printf("Invalid move: cart[%d] at (%d,%d) going %s but map piece is %c\n", i, k->x,k->y, print_dir(k->dir), cur);
      return EXIT_FAILURE;

    } // end for(k in carts)
  } // end while(1)

  free(map);
  free(carts);

  return EXIT_SUCCESS;
}
