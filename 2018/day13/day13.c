#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#define MAP(x,y) (map[(y*HEIGHT)+x])

// directions
#define U 0
#define R 1
#define D 2
#define L 3
#define TURN_RIGHT(x) (((x+1)%4))
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
  struct cart * next;
};

struct cart * make_cart(int x, int y, Dir d, struct cart * carts)
{
  struct cart * cart = (struct cart *) malloc(sizeof(struct cart));
  cart->x = x;
  cart->y = y;
  cart->dir = d;
  cart->turn = LEFT;
  cart->next = carts;
  return cart;
}

void free_carts(struct cart * carts)
{
  while (carts) {
    struct cart * next = carts->next;
    free(carts);
    carts = next;
  }
}

int main ()
{
  int WIDTH = 0;
  int HEIGHT = 0;
  char* map;

  // input parsing
  {
    FILE *fp;
    // find width and height
    {
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
          widthl++;
        }
      }
    }
    fp = fopen("input", "r");
    map = (char*) calloc(WIDTH*HEIGHT, sizeof(char));
    struct cart * carts = NULL;

    // actually parse it
    {
      int x = 0;
      int y = 0;
      char c;
      while ((c = getc(fp)) != EOF) {

        if (c == '\n') {
          y++;
          x = 0;
        } else {

          MAP(x,y) = c;
          // parse the carts
          carts
        }
      }
    }
  }


  return 0;
}
