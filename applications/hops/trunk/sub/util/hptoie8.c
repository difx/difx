/********************************************************/
/*							*/
/* Converts HP1000-series double-precision floating	*/
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
#define LOW20 0xfffff
#define LOW7  0x7f

int
hptoie8 (double *dinput, double *doutput)
{
	unsigned int *in, out[2], sign, hp_mant[2];
	unsigned int ieee_exp;
	char hp_exp;
	double dtemp;

	dtemp = *dinput;				/* Local copy in int array */
	in = (unsigned int *) &dtemp;

	if (in[0] == 0 && in[1] == 0)			/* Zero is a special case */
	{
	    *doutput = *dinput;
	    return(0);
	}

	sign = in[0] >> 31;				/* Get sign bit */

	hp_mant[1] = in[1] >> 8;			/* Literal HP mantissa */
	hp_mant[1] += in[0] << 24;			/* Implied and sign bits */
	hp_mant[0] = (in[0] >> 8) & LOW23;		/* masked out later */
	if (sign) 
	{
	    hp_mant[1] = ~hp_mant[1] + 1;		/* Two's complement when -ve */
	    hp_mant[0] = (~hp_mant[0]) & LOW23;
	    if (hp_mant[1] == 0) hp_mant[0] += 1;	/* Carry bit from second */
	}						/* 4-byte word */

	if (hp_mant[0] < 4194304) return (-1);		/* Not normalized! */

	hp_exp = (in[1] >> 1) & LOW7;			/* Literal HP exponent */
	if (in[1] & 1) hp_exp |= 0x80;			/* Make into signed int */
	ieee_exp = 1022 + hp_exp;			/* Convert to biased int */
	if (hp_mant[0] & BIT23) ieee_exp += 1;		/* Spec. case for carry bit */

	out[1] = (hp_mant[1] >> 2) + (hp_mant[0] << 30);/* Copy relevant bits into */
	out[0] = (hp_mant[0] >> 2) & LOW20;		/* output */
	out[0] += ieee_exp << 20;
	if (sign) out[0] |= SBIT;			/* Copy in sign bit */

	*doutput = *(double *) out;			/* Put it where expected */
	return (0);
}
