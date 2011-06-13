/********************************************************/
/*							*/
/* Converts HP1000-series single-precision floating	*/
/* point data to IEEE format.  No attempt is made to	*/
/* deal with input numbers that are not normalized	*/
/* Input and output may be the same physical address	*/
/* Returns 0 on success					*/
/*							*/
/* CJL  April 16 1991					*/
/********************************************************/
#include "mk4_util.h"

#define SBIT  0x80000000
#define BIT23 0x800000
#define LOW23 0x7fffff
#define LOW7  0x7f

int
hptoie4 (float *finput, float *foutput)
{
	unsigned int in, out, sign, hp_mant;
	unsigned int ieee_exp;
	char hp_exp;

	in = *(unsigned int *) finput;			/* Work on integer */

	if (in == 0)					/* Zero is a special case */
	{
	    *foutput = in;
	    return(0);
	}

	sign = in >> 31;				/* Get sign bit */

	hp_mant = (in >> 8);				/* Literal HP mantissa */
	if (sign) hp_mant = ((~hp_mant) & LOW23) + 1;	/* If -ve, 2's complement */

	if (hp_mant < 4194304) return (-1);		/* Not normalized! */

	hp_exp = (in >> 1) & LOW7;			/* Literal HP exponent */
	if (in & 1) hp_exp |= 0x80;			/* Make into signed int */
	ieee_exp = 126 + hp_exp;			/* Convert to biased int */
	if (hp_mant & BIT23) ieee_exp += 1;		/* Spec. case for carry bit */
							/* in hp mantissa 2's comp. */

	out = ((hp_mant << 1) & LOW23)			/* IEEE has implied bit */
		 + (ieee_exp << 23);			/* so left shift and chop */
	if (sign) out |= SBIT;				/* Copy in sign bit */

	*foutput = *(float *) (&out);			/* Put it where expected */
	return (0);
}
