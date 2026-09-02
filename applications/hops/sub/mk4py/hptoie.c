/********************************************************/
/*							*/
/* Converts HP1000-series  floating point data to IEEE 	*/
/* format.  						*/
/* Input and output may be the same physical address	*/
/* Returns 0 on success, -1 on detection of non-	*/
/* -normalized input, and -2 on detection of bad type	*/
/* specification.					*/
/* Type should be 1 for single precision and 2 for	*/
/* double precision.					*/
/*							*/
/* CJL  April 16 1991					*/
/********************************************************/
#include "mk4_util.h"

int
hptoie (int *input, int *output, int type)
{
	int ret;

	switch (type)
	{
	    case 1:
		ret = hptoie4 ((float *)input, (float *)output);
		break;

	    case 2:
		ret = hptoie8 ((double *)input, (double *)output);
		break;

	    default:
		ret = -2;
	}
	return (ret);
}
