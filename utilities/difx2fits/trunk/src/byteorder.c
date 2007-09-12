/*  @(#)byteorder.c  version 1.3  created 05/08/30 17:03:23
                 fetched from SCCS 05/09/22 15:30:26
 LANGUAGE: C
 ENVIRONMENT: Any, but meant for linux since all byte orders
              in the VLBA system are that of LITTLE_ENDIAN
 AUTHOR: Walter Brisken
*/
#include <stdio.h>
#include <stdlib.h>
#include "byteorder.h"

/* Returns BO_LITTLE_ENDIAN if the system is intel byte-ordered.  
 * BO_BIG_ENDIAN otherwise.
 */
int byteorder()
{
	static union
	{
		char c[4];
		int i;
	} u;

	u.c[0] = 1;
	u.c[1] = u.c[2] = u.c[3] = 0;

	return u.i == 1 ? BO_LITTLE_ENDIAN : BO_BIG_ENDIAN;
}

/* byteswap will perform a byteswap given a sequence of element
 * types.  for example: int bso[][2] = {{1, 2}, {4, 4}, {2, 8}, {0, 0}};
 * will treat the first datum as a 2-byte integer to swap, the
 * next 4 as four byte integers (or floats), and the next 2 as
 * 8 byte ints (or doubles), ...  if {1000, 1} is given, the
 * next 1000 bytes will be unchanged.  to terminate list, include
 * {0, 0} at the end.
 */
int swapbytes(void *data, int order[][2])
{
	char *cdata;
	int temp;
	int t, n, b;

	cdata = (char *)data;
	for(n = 0; order[n][0] != 0; n++)
	{
		t = order[n][1];
		if(t == 1) 
		{
			cdata += order[n][0];	/* No swapping */
		}
		else 
		{
			for(b = 0; b < order[n][0]; b++)
			{
				switch(t)
				{
				case 2:
					temp = *cdata;
					*cdata = *(cdata+1);
					*(cdata+1) = temp;
					break;
				case 4:
					temp = *cdata;
					*cdata = *(cdata+3);
					*(cdata+3) = temp;
					temp = *(cdata+1);
					*(cdata+1) = *(cdata+2);
					*(cdata+2) = temp;
					break;
				case 8:
					temp = *cdata;
					*cdata = *(cdata+7);
					*(cdata+7) = temp;
					temp = *(cdata+1);
					*(cdata+1) = *(cdata+6);
					*(cdata+6) = temp;
					temp = *(cdata+2);
					*(cdata+2) = *(cdata+5);
					*(cdata+5) = temp;
					temp = *(cdata+3);
					*(cdata+3) = *(cdata+4);
					*(cdata+4) = temp;
					break;
				default:
					return 0;
				}
				cdata += t;
			}
		}
	}	
	return 1;
}
