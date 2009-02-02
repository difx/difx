#include "replaced.h"

void countReplaced(const unsigned long *data, int len, 
	long long *wGood, long long *wBad)
{
	int i;
	int nBad=0;

	for(i = 0; i < len; i++)
	{
		if(data[i] == MARK5_FILL_WORD32)
		{
			nBad++;
		}
	}

	*wGood += (len-nBad);
	*wBad += nBad;
}

