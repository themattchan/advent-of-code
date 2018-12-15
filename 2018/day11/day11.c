#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#define INPUT 4151
#define SIZE 300

int main ()
{
  int grid[SIZE+1][SIZE+1];

  for (int y = 1; y <= SIZE; ++y) {
    for (int x = 1; x <= SIZE; ++x) {
      int rack_id = x + 10;
      int power = rack_id * y;
      power += INPUT;
      power *= rack_id;
      power /= 100;
      power %= 10;
      power -= 5;
      grid[y][x] = power;
    }
  }

  {
    int maxY = 0;
    int maxX = 0;
    int maxP = INT_MIN;

    for (int y = 1; y <= SIZE-2; ++y) {
      for (int x = 1; x <= SIZE-2; ++x) {
        int newMaxP =
          grid[y  ][x] + grid[y  ][x+1] + grid[y  ][x+2]
          + grid[y+1][x] + grid[y+1][x+1] + grid[y+1][x+2]
          + grid[y+2][x] + grid[y+2][x+1] + grid[y+2][x+2];

        if (newMaxP > maxP) {
          maxP = newMaxP;
          maxX = x; maxY = y;
        }
      }
    }
    printf("part1: %d,%d\n", maxX, maxY);
  }

  // you can probably optimise this further by exploiting some property of
  // overlapping squares
  {
    int maxY = 0;
    int maxX = 0;
    int maxS = 0;
    int maxP = INT_MIN;

    // at iteration s, sums[y][x] stores the sum of the size-(s-1) square with
    // top left corner as (x,y)
    int sums[SIZE+1][SIZE+1] = {0};

    for (int s = 1; s <= SIZE; ++s) {
      for (int y = 1; y <= SIZE-(s-1); ++y) {
        for (int x = 1; x <= SIZE-(s-1); ++x) {
          int newMaxP = sums[y][x];

          // add L, increase sum by 1 square size
          for (int i = 0; i < s-1; ++i) {
            // add bottom row
            newMaxP += grid[y+(s-1)][x+i];
            // add right hand col
            newMaxP += grid[y+i][x+(s-1)];
          }
          // add bottom right corner
          newMaxP += grid[y+(s-1)][x+(s-1)];

          sums[y][x] = newMaxP;

          if (newMaxP > maxP) {
            maxP = newMaxP;
            maxX = x; maxY = y; maxS = s;
          }
        }
      }
    }
    printf("part2: %d,%d,%d\n", maxX, maxY, maxS);
  }

  return 0;
}

/*  time ./day11
 * part1: 20,46
 * part2: 231,65,14
 * ./day11  0.68s user 0.00s system 99% cpu 0.679 total */
