#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool
check_array(int expect[4], int actual[4])
{
  for (int i = 0; i < 4; ++i) {
    if (expect[i] != actual[i])
      return false;
  }
  return true;
}

void
init_array(int from[4], int to[4])
{
  for (int i = 0; i < 4; ++i)
    to[i] = from[i];
}

int try_op(int before[4], int op[4], int after[4])
{
  int count = 0;

  int reg[4];

#define TEST(f) {                               \
    init_array(before, reg);                    \
    f;                                          \
    count += check_array(after, reg);           \
  }

  // Addition:
  //
  // addr (add register) stores into register C the result of adding register A
  // and register B.
  TEST(reg[op[3]] = reg[op[1]] + reg[op[2]]);

  // addi (add immediate) stores into register C the result of adding register A
  // and value B.
  TEST(reg[op[3]] = reg[op[1]] + op[2]);

  // Multiplication:
  //
  // mulr (multiply register) stores into register C the result of multiplying
  // register A and register B.
  TEST(reg[op[3]] = reg[op[1]] * reg[op[2]]);

  // muli (multiply immediate) stores into register C the result of multiplying
  // register A and value B.
  TEST(reg[op[3]] = reg[op[1]] * op[2]);

  // Bitwise AND:
  //
  // banr (bitwise AND register) stores into register C the result of the bitwise
  // AND of register A and register B.
  TEST(reg[op[3]] = reg[op[1]] & reg[op[2]]);

  // bani (bitwise AND immediate) stores into register C the result of the bitwise
  // AND of register A and value B.
  TEST(reg[op[3]] = reg[op[1]] & op[2]);

  // Bitwise OR:
  //
  // borr (bitwise OR register) stores into register C the result of the bitwise
  // OR of register A and register B.
  TEST(reg[op[3]] = reg[op[1]] | reg[op[2]]);

  // bori (bitwise OR immediate) stores into register C the result of the bitwise
  // OR of register A and value B.
  TEST(reg[op[3]] = reg[op[1]] | op[2]);

  // Assignment:
  //
  // setr (set register) copies the contents of register A into register C. (Input
  // B is ignored.)
  TEST(reg[op[3]] = reg[op[1]]);

  // seti (set immediate) stores value A into register C. (Input B is ignored.)
  TEST(reg[op[3]] = op[1]);

  // Greater-than testing:
  //
  // gtir (greater-than immediate/register) sets register C to 1 if value A is
  // greater than register B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = op[1] > reg[op[2]] ? 1 : 0);

  // gtri (greater-than register/immediate) sets register C to 1 if register A is
  // greater than value B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = reg[op[1]] > op[2] ? 1 : 0);

  // gtrr (greater-than register/register) sets register C to 1 if register A is
  // greater than register B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = reg[op[1]] > reg[op[2]] ? 1 : 0);

  // Equality testing:
  //
  // eqir (equal immediate/register) sets register C to 1 if value A is equal to
  // register B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = op[1] == reg[op[2]] ? 1 : 0);

  // eqri (equal register/immediate) sets register C to 1 if register A is equal
  // to value B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = reg[op[1]] == op[2] ? 1 : 0);

  // eqrr (equal register/register) sets register C to 1 if register A is equal to
  // register B. Otherwise, register C is set to 0.
  TEST(reg[op[3]] = reg[op[1]] == reg[op[2]] ? 1 : 0);

#undef TEST

  return count;
}


int main()
{
  FILE *fp;
  char *line = NULL;
  size_t len = 0;
  ssize_t llen;
  fp = fopen("input", "r");
  /* while () {
   *   if ((llen = getline(&line, &len, fp)) != -1) {
   *   printf("line: len=%zu, %s\n", llen, line);
   *   }
   * } */
  int before[4] = {0}; // register A, B, C, D
  int after[4] = {0}; // register A, B, C, D

  // op: num, in1, in2, out.
  // if input or output is REGISTER then value of arg is register index
  // if input or output is VALUE then value of arg is value
  int op[4] = {0};

  int more_than_3 = 0;
  int is_part1 = 1;

  while (is_part1) {
    int ret = fscanf(fp, "Before: [%d, %d, %d, %d]\n", &before[0], &before[1], &before[2], &before[3]);
    if (ret != 4) is_part1 = false;

    fscanf(fp, "%d %d %d %d\n", &op[0], &op[1], &op[2], &op[3]);
    fscanf(fp, "After: [%d, %d, %d, %d]\n", &after[0], &after[1], &after[2], &after[3]);

    int num_matched = try_op(before, op, after);
    if (num_matched >= 3) {
      more_than_3++;
    }
  }
  //  printf("true=%d; false=%d\n", true,false);
  printf("More than 3: %d\n", more_than_3);

  fclose(fp);
  return EXIT_SUCCESS;
}
