#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool check3conseq(char * s)
{
	char *s0 = s+0;
	char *s1 = s+1;
	char *s2 = s+2;
	while (s2 && s0 && s1) {
//		puts("HERE");
		if ((*s0)+1 == *s1 && (*s1)+1 == *s2) {
//			puts("YES");
			return true;
		}
		s0++; s1++; s2++;
	}
	return false;
}

bool noiol(char *s)
{
	while (s) {
		if ((int)(*s) == 105 || (int)(*s) == 108 || (int)(*s) == 111)
			return false;
		s++;
	}
	return true;
}

bool twooverlap(char *s)
{
	int x = 0;
	char *s0 = s+0;
	char *s1 = s+1;
	while (s1) {
		if (*s0 == *s1) {
			x++;
			s0 += 2; s1 += 2;
		}
		else {
			s0++; s1++;
		}
	}
	return x >= 2;
}

#define INCR(x) ((x) == 'z' ? 'a' : (x)+1)

void incrs(char *sbeg, char *send)
{
	char* cur = send;
	*cur = INCR(*cur);
	/* optimisation: if *cur is IOL after incr, then set everything to the
	 * right to 'a' */
	while (*cur == 'a' && cur != sbeg) {
		cur--;
		*cur = INCR(*cur);
	}
}

int main(int argc, char * argv[])
{
//	char * in = "hxbxwxba";
	if (argc < 2) {
		puts("NO INPUT\n");
		return 1;
	}
	char *in = argv[1];
	int end = strlen(in) -1;
//	printf("strlen: %d\n", end);
//	printf("lastchar: %c\n", *(in+end));
	while (! (check3conseq(in) && noiol(in) && twooverlap(in))) {
		incrs(in, in+end);
//		printf("INCREMENTED: %s\n",in);
	}
	puts(in);
	return 0;
}
