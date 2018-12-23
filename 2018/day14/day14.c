#include <stdlib.h>
#include <stdio.h>

#define INPUT 681901
#define SIZE (INPUT*100) // INPUT+10

typedef unsigned char D;

int main ()
{
  D *buf = (D*) calloc(SIZE, sizeof(D));
  buf[0] = 3;
  buf[1] = 7;

  {
    int i = 0;
    int j = 1;
    int cur = 2;
    while (cur < SIZE) {
      D ix = buf[i];
      D jx = buf[j];
      D x = ix+jx;
      D x1 = x /10;
      D x2 = x %10;

      if (x1 != 0) buf[cur++] = x1;
      buf[cur++] = x2;

      i = (i + 1 + (int)ix) % cur;
      j = (j + 1 + (int)jx) % cur;
    }
  }

  printf("part1: ");
  for (int i = INPUT; i < INPUT+10; ++i) putchar(buf[i] + '0');
  putchar('\n');

#define M(i,n) ((buf[x+i]==n))
  for (int x = 0; x < SIZE; ++x) {
    if (M(0,6) && M(1,8) && M(2,1) && M(3,9) && M(4,0) && M(5,1)) {
      printf("part2: %d\n",x);
      break;
    }
  }
#undef M

  free(buf);
  return 0;
}
