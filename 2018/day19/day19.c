#include "../register.h"

int main()
{
  FILE *fp;
  fp = fopen("input", "r");

  int lines = 0;
  while (! feof(fp)) if (fgetc(fp) == '\n') lines++;
  lines--; // skip #ip

  rewind(fp);
  int ip;
  fscanf(fp, "#ip %d\n", &ip);

  int reg[6] = {0};
  int prog[lines][4];

  int line = 0;
  while (! feof(fp)) {
    char opname[5];
    fscanf(fp, "%s %d %d %d\n", opname, prog[line]+1,prog[line]+2,prog[line]+3);
    prog[line][0] = op_s2i(opname);
    line++;
  }
  do_op_proper();

}
