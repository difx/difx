#include "transient_wrapper_data.h"

TransientWrapperData *newTransientWrapperData()
{
	TransientWrapperData *T;

	T = (TransientWrapperData *)calloc(1, sizeof(TransientWrapperData));

	T->difxState = -1;

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
