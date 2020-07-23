#include <stdio.h>

void my_init(void);
void my_init(void) __attribute__((constructor(101)));
void my_init(void)
{
	printf("Almost ready!\n");
}
int main(void)
{
	printf("Hello, world!\n");
	return 0;
}
