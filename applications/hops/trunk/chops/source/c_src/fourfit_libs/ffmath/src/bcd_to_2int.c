/*******************************************************************************/
/*                                                                             */
/* Takes a 4-byte integer containing a bcd-format number, and returns the      */
/* corresponding integer values before and after the floating point.  The      */
/* number of digits (right justified) 					       */
/* and the number of digits after the decimal point are required as arguments. */
/* No range checking on the digit values is performed, all values assumed +ve  */
/*                                                                             */
/* Original version  CJL August 5 1991                                         */
/*                                                                             */
/*******************************************************************************/

#define LOW4 017

#include <math.h>

void
bcd_to_2int (int input, int ndigits, int npoint, int result[2])
{
    int i, total, power_10;

    result[0] = result[1] = 0;
    power_10 = 1;
    for (i = 0; i < npoint; i++)
	{
	result[1] += power_10 * ((input >> (4*i)) & LOW4);
	power_10 *= 10;
	}
    power_10 = 1;
    for (i = npoint; i < ndigits; i++)
	{
	result[0] += power_10 * ((input >> (4*i)) & LOW4);
	power_10 *= 10;
	}
}
