#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int
main()
{
  FILE *fp;
  fp = fopen("input", "r");
  int num_players, end_points;
  fscanf(fp, "%d players; last marble is worth %d points", &num_players, &end_points);
  fclose(fp);


  return EXIT_SUCCESS;
}
