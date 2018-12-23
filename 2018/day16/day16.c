#include "../register.h"

// there are 16 opcodes
int
try_op(u_int16_t which_one[16], const int before[4], int op[4], const int after[4])
{
  int count = 0;
  int actualOp = op[0];

  for (int opNum = 0; opNum < 16; ++opNum) {
    op[0] = opNum;

    int reg[4] = { before[0], before[1], before[2], before[3] };

    do_op_proper(op, reg);

    bool same = after[op[3]] == reg[op[3]];

    count += same;
    if (!same && !(ispow2(which_one[actualOp]))) {
      unsetbit(which_one[actualOp], opNum);
    }
  }
  return count;
}

void
resolve(u_int16_t which_one[16])
{
  int todo = 0;
  for (int i = 0; i < 16; ++i)
    todo += !ispow2(which_one[i]);

  while (todo) {
    /* printf("NEW ITERATION: todo=%d\n",todo);
     * for (int i = 0; i < 16; ++i) {
     *   char bin[17] = {0};
     *   bin16(which_one[i], bin);
     *   printf("%3d --> %3d (0b%s)\n", i, whichbit(which_one[i]), bin);
     * } */

    for (int i = 0; i < 16; ++i) {
      if (! ispow2(which_one[i])) {
        u_int16_t tryclr = which_one[i];
        for (int j = 0; j < 16; ++j)
          if (j != i) tryclr &= ~which_one[j];

        /* char bin[17] = {0};
         * bin16(tryclr, bin);
         * printf("%3d --> 0b%s ispow2=%d\n", i, bin, ispow2(tryclr)); */

        if (ispow2(tryclr)) {
          todo--;
          which_one[i] = tryclr;
        }
      }
    }
  }

  /* printf("Resolve done\n"); */
}

int
main()
{
  FILE *fp;
  fp = fopen("input", "r");

  int before[4] = {0}; // register 0,1,2,3
  int after[4] = {0}; // register 0,1,2,3

  // op: num, in1, in2, out.
  // if input or output is REGISTER then value of arg is register index
  // if input or output is VALUE then value of arg is value
  int op[4] = {0};

  int more_than_3 = 0;

  u_int16_t which_one[16] = {0};

  for (int i = 0; i < 16; ++i) {
    // set all bits to 1
    which_one[i] = ~which_one[i];
  }

  while (true) {
    int ret = fscanf(fp, "Before: [%d, %d, %d, %d]\n", &before[0], &before[1], &before[2], &before[3]);
    if (ret != 4) break;

    fscanf(fp, "%d %d %d %d\n", &op[0], &op[1], &op[2], &op[3]);
    fscanf(fp, "After: [%d, %d, %d, %d]\n", &after[0], &after[1], &after[2], &after[3]);

    int num_matched = try_op(which_one, before, op, after);
    if (num_matched >= 3) {
      more_than_3++;
    }
  }

  printf("More than 3: %d\n\n", more_than_3);

  resolve(which_one);

  printf ("Mapping:\n");
  for (int i = 0; i < 16; ++i) {
    char bin[17] = {0};
    bin16(which_one[i], bin);
    printf("%3d --> %3d (0b%s)\n", i, whichbit(which_one[i]), bin);
  }
  putchar('\n');

  int reg[4] = {0};
  int instr = 0;
  while (! feof(fp)) {
    instr++;
    int ret = fscanf(fp, "%d %d %d %d\n", &op[0], &op[1], &op[2], &op[3]);
    if (ret != 4) fprintf(stderr, "could not parse input");

    //  printf("%d %d %d %d\n", op[0],op[1],op[2],op[3]);
    op[0] = whichbit(which_one[op[0]]); // translate to proper op
    do_op_proper(op, reg);
  }

  printf("test program finished, executed %d instructions\n", instr);
  printf("registers are:\n");
  for (int i = 0; i < 4; ++i)
    printf("reg[%d]=%d\n", i, reg[i]);


  fclose(fp);
  return EXIT_SUCCESS;
}
