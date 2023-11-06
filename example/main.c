#include <stdio.h>

extern int myfunc(int);

int
main(void)
{
	printf("Hello %d!\n", myfunc(2));
}
