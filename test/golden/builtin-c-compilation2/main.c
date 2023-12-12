#include <stdio.h>

extern int myfunc(int);

int
main(void) {
	printf("result: %d\n", myfunc(2));
	return 0;
}
