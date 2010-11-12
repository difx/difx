#include "transient_wrapper_data.h"

TransientWrapperData *newTransientWrapperData()
{
	TransientWrapperData *T;

	T = (TransientWrapperData *)calloc(1, sizeof(TransientWrapperData));

	T->difxState = DIFX_STATE_SPAWNING;

	return T;
}

void deleteTransientWrapperData(TransientWrapperData *T)
{
	if(T)
	{
		if(T->D)
		{
			deleteDifxInput(T->D);
		}

		if(T->filePrefix)
		{
			free(T->filePrefix);
		}
	}
}

void printTransientWrapperData(const TransientWrapperData *T)
{
	printf("TransientWrapperData [%p]\n", T);
	if(T)
	{
		printf("  DifxState = %s [%d]\n", DifxStateStrings[T->difxState], T->difxState);
		printf("  filePrefix = %s\n", T->filePrefix);
		printf("  identifier = %s\n", T->identifier);
		printf("  monitorThreadDie = %d\n", T->monitorThreadDie);
		printf("  verbose = %d\n", T->verbose);
		printf("  rank = %d\n", T->rank);
		printf("  doCopy = %d\n", T->doCopy);
	}
}
