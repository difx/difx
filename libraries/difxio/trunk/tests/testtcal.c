#include <stdio.h>
#include "difxio/difx_tcal.h"

int main()
{
	DifxTcal *T;
	int v;
	float t;

	T = newDifxTcal();

	printf("Initial: ");
	summarizeDifxTcal(T);

	v = setDifxTcalVLBA(T, "/home/jansky3/vlbaops/TCAL");
	printf("\nv = %d\n", v);

	printf("VLBA: ");
	summarizeDifxTcal(T);

	t = getDifxTcal(T, 0, "pt", "6cm", 'R', 5000.0);
	printf("\n5000@pt -> %f\n", t);

	printf("pt: ");
	summarizeDifxTcal(T);

	for(v = 1; v < 100; v+=10)
	{
		t = getDifxTcal(T, 0, "pt", "", 'R', 1000.0*v + 0.5);
		printf("%f@pt -> %f\n", 1000.0*v + 0.5, t);
	}

	printf("allpt: ");
	summarizeDifxTcal(T);

	for(v = 1; v < 100; v+=1)
	{
		t = getDifxTcal(T, 0, "pt", "", 'R', 1000.0*v + 0.5);
		printf("%f@pt -> %f\n", 1000.0*v + 0.5, t);
	}


	deleteDifxTcal(T);

	return 0;
}
