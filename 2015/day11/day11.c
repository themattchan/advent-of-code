#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool check3conseq(char * s)
{
	char *s0 = s+0;
	char *s1 = s+1;
	char *s2 = s+2;
	while (s2) {
		if (*s0+1 == *s1 && *s1+1 == *s2)
			return true;

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

		s0++; s1++;
	}
	return x >= 2;

}

#define INCR(x) ((x) == 'z' ? 'a' : (x)+1)

void incrs(char *sbeg, char *send)
{
	char* cur = send;
	*cur = INCR(*cur);
	while (*cur == 'a' && cur != sbeg) {
		cur=cur-1;
		*cur = INCR(*cur);
	}
}

int main()
{
	char * in = "hxbxwxba";
	int end = strlen(in);
	while (! (check3conseq(in) && noiol(in) && twooverlap(in))) {
		incrs(in, in+end);
	}
	puts(in);
	return 0;
}
