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

  // yes you can solve this with DP but the brute force solution is fast enough
  {
    int maxY = 0;
    int maxX = 0;
    int maxS = 0;
    int maxP = INT_MIN;

    for (int s = 1; s <= SIZE; ++s) {
      for (int y = 1; y <= SIZE-(s-1); ++y) {
        for (int x = 1; x <= SIZE-(s-1); ++x) {
          int newMaxP = 0;
          for (int i = 0; i < s; ++i) {
            for (int j = 0; j < s; ++j) {
              newMaxP += grid[y+i][x+j];
            }
          }

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
