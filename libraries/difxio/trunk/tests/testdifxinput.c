#include <stdio.h>
#include "difx_input.h"

int main(int argc, char **argv)
{
	DifxInput *D;
	
	if(argc < 2)
	{
		printf("Usage : %s <inputfilebase>\n", argv[0]);
		return 0;
	}
	
	D = loadDifxInput(argv[1]);
	if(!D)
	{
		fprintf(stderr, "D == 0.  quitting\n");
		return 0;
	}

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "update failed: D == 0.  quitting\n");
		return 0;
	}

	printDifxInput(D);

	writeDifxCalc(D, "calc.out");
	writeDifxInput(D, "input.out");

	deleteDifxInput(D);

	return 0;
}
