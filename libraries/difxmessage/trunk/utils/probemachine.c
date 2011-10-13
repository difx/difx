#include <stdio.h>
#include <string.h>
#include "testmachine.h"

int main()
{
	char hostname[100];
	int v;

	strcpy(hostname, "localhost");

	v = pingtest(hostname);

	printf("ping = %d\n", v);

	return 0;
}
