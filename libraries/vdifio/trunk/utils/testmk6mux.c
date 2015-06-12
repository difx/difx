#include <stdio.h>
#include <stdlib.h>
#include "vdifmark6.h"

int main(int argc, char **argv)
{
	struct vdif_mark6_mux *vm;
	int framesPerSecond;

	if(argc < 4)
	{
		printf("please supply <template filename> , <scan file name> , and <frames per second>\n");

		exit(EXIT_SUCCESS);
	}

	framesPerSecond = atoi(argv[3]);

	vm = configurevdifmark6mux(argv[1], argv[2], framesPerSecond);

	printvdifmark6mux(vm);

	deletevdifmark6mux(vm);

	return 0;
}
