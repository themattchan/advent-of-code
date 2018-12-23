#ifndef _REGISTER_2018
#define _REGISTER_2018

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define setbit(bv, bit) (bv |= (1 << bit))
#define unsetbit(bv, bit) (bv &= ~(1 << bit))
#define testbit(bv, bit) ((bv >> i) & 1)

#define ispow2(x) ((((x) != 0) && (((x) & (x)-1) == 0)))

void
bin16(u_int16_t bv, char bin[16])
{
  for (int i = 0; i < 16; ++i) {
    bin[15-i] = testbit(bv, i) + '0';
  }
}

int
whichbit(u_int16_t bv)
{
  if (! ispow2(bv)) return -1; // more than 1 bit on
  u_int16_t i = 1;
  int ret = 0;
  while (! (i&bv)) {
    i <<= 1;
    ret++;
  }
  return ret;
}

void
do_op_proper(int op[4], int reg[4])
{
  switch (op[0]) {
    // Addition:
    //
    // addr (add register) stores into register C the result of adding register A
    // and register B.
  case 0:   reg[op[3]] = reg[op[1]] + reg[op[2]]; break;

    // addi (add immediate) stores into register C the result of adding register A
    // and value B.
  case 1:   reg[op[3]] = reg[op[1]] + op[2]; break;

    // Multiplication:
    //
    // mulr (multiply register) stores into register C the result of multiplying
    // register A and register B.
  case 2:   reg[op[3]] = reg[op[1]] * reg[op[2]]; break;

    // muli (multiply immediate) stores into register C the result of multiplying
    // register A and value B.
  case 3:   reg[op[3]] = reg[op[1]] * op[2]; break;

    // Bitwise AND:
    //
    // banr (bitwise AND register) stores into register C the result of the bitwise
    // AND of register A and register B.
  case 4:   reg[op[3]] = reg[op[1]] & reg[op[2]]; break;

    // bani (bitwise AND immediate) stores into register C the result of the bitwise
    // AND of register A and value B.
  case 5:   reg[op[3]] = reg[op[1]] & op[2]; break;

    // Bitwise OR:
    //
    // borr (bitwise OR register) stores into register C the result of the bitwise
    // OR of register A and register B.
  case 6:   reg[op[3]] = reg[op[1]] | reg[op[2]]; break;

    // bori (bitwise OR immediate) stores into register C the result of the bitwise
    // OR of register A and value B.
  case 7:   reg[op[3]] = reg[op[1]] | op[2]; break;

    // Assignment:
    //
    // setr (set register) copies the contents of register A into register C. (Input
    // B is ignored.)
  case 8:   reg[op[3]] = reg[op[1]]; break;

    // seti (set immediate) stores value A into register C. (Input B is ignored.)
  case 9:   reg[op[3]] = op[1]; break;

    // Greater-than testing:
    //
    // gtir (greater-than immediate/register) sets register C to 1 if value A is
    // greater than register B. Otherwise, register C is set to 0.
  case 10:   reg[op[3]] = op[1] > reg[op[2]] ? 1 : 0; break;

    // gtri (greater-than register/immediate) sets register C to 1 if register A is
    // greater than value B. Otherwise, register C is set to 0.
  case 11:   reg[op[3]] = reg[op[1]] > op[2] ? 1 : 0; break;

    // gtrr (greater-than register/register) sets register C to 1 if register A is
    // greater than register B. Otherwise, register C is set to 0.
  case 12:   reg[op[3]] = reg[op[1]] > reg[op[2]] ? 1 : 0; break;

    // Equality testing:
    //
    // eqir (equal immediate/register) sets register C to 1 if value A is equal to
    // register B. Otherwise, register C is set to 0.
  case 13:   reg[op[3]] = op[1] == reg[op[2]] ? 1 : 0; break;

    // eqri (equal register/immediate) sets register C to 1 if register A is equal
    // to value B. Otherwise, register C is set to 0.
  case 14:   reg[op[3]] = reg[op[1]] == op[2] ? 1 : 0; break;

    // eqrr (equal register/register) sets register C to 1 if register A is equal to
    // register B. Otherwise, register C is set to 0.
  case 15:   reg[op[3]] = reg[op[1]] == reg[op[2]] ? 1 : 0; break;
  }
}


#endif
