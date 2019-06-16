#include "../register.h"

int IP_REG = 0;

void doit(int lines, int prog[][4], int reg[]) {
  int ip = 0;
  while (ip >= 0 && ip < lines) {
    reg[IP_REG] = ip;
    do_op_proper(prog[ip], reg);
    ip = reg[IP_REG];
    ip++;
  }
  printf("registers are:\n");
  for (int i = 0; i < 4; ++i)
    printf("reg[%d]=%d\n", i, reg[i]);
}

int main()
{
  FILE *fp;
  fp = fopen("input", "r");

  int lines = 0;
  while (! feof(fp)) if (fgetc(fp) == '\n') lines++;
  lines--; // skip #ip

  rewind(fp);
  fscanf(fp, "#ip %d\n", &IP_REG);

  int reg[6] = {0};
  int prog[lines][4];

  int line = 0;
  while (! feof(fp)) {
    char opname[5];
    fscanf(fp, "%s %d %d %d\n", opname, prog[line]+1,prog[line]+2,prog[line]+3);
    prog[line][0] = op_s2i(opname);
    line++;
  }
  fclose(fp);

  doit(lines,prog,reg);

  /* for (int i = 0; i < 6; i++) reg[i]=0;
   * reg[0] = 1;
   * doit(lines,prog,reg); */

  return EXIT_SUCCESS;
}
