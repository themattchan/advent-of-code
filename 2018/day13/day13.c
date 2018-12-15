#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#define MAP(x,y) (map[(y*HEIGHT)+x])

// directions
#define U 0
#define R 1
#define D 2
#define L 3
#define TURN_RIGHT(x) ((((x)+1)%4))
#define TURN_LEFT(x) ((((x-1)+4)%4))

typedef char Dir;

// intersection turns
#define LEFT 0
#define STRAIGHT 1
#define RIGHT 2
#define NEXT_INTERSECTION_TURN(x) ((((x)+1)%3))

typedef char Turn;

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

/* int sort_carts(const void * c1, const void * c2)
 * {
 * } */

int main ()
{
  int WIDTH = 0;
  int HEIGHT = 0;
  int NUM_CARTS = 0;

  char *map = NULL;
  struct cart * carts = NULL;

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

  for (int i = 0; i < NUM_CARTS; ++i) {
    printf("Cart %d: x=%d, y=%d, dir=%d\n", i, carts[i].x, carts[i].y, carts[i].dir);
  }

  /* for (int i = 0; i < HEIGHT; ++i) {
   *   for (int j = 0; j < WIDTH; ++j) {
   *     putchar(MAP(j,i));
   *   }
   *   putchar('\n');
   * } */

  free(map);
  free(carts);
  return 0;
}
