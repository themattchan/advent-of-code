#include <stdlib.h>
#include <stdio.h>

#define INPUT 681901

typedef unsigned char D;

int main ()
{
  D *buf = (D*) calloc((INPUT+10), sizeof(D));

  buf[0] = 3;
  buf[1] = 7;

  {
    int i = 0;
    int j = 1;
    int cur = 2;
    while (cur < INPUT+10) {
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

  for (int i = INPUT; i < INPUT+10; ++i) putchar(buf[i] + '0');
  putchar('\n');

  free(buf);
  return 0;
}
