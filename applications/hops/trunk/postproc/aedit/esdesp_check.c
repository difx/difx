/************************************************************************/
/*									*/
/* A simple routine to look at the ESDESP tape quality codes, and	*/
/* decide if they constitute a reason to demote a datum to suspect or	*/
/* bad status for colour plotting.					*/
/*									*/
/*	Inputs:		esdesp		Tape quality codes		*/
/*									*/
/*	Output:		return value	datum flagged good (0),		*/
/*					suspect (1) or bad (2)		*/
/*									*/
/* Created 26 February 1992 by CJL					*/
/*									*/
/************************************************************************/


int
esdesp_check (esdesp)
int esdesp;
    {
    int referr, remerr, refslip, remslip, disc, frac;
    int esqual;

    referr = esdesp/100000;
    remerr = (esdesp%1000)/100;
/*  refslip = (esdesp%100000)/10000; */
/*  remslip = (esdesp%100)/10; */
/*  disc = (esdesp%10000)/1000; */
    frac = esdesp%10;

    esqual = 0;			/* Start good and look for flaws */

				/* most of these numbers are meaningless */
				/* in the current A-file format ... just */
				/* check the ones of value for now */
    if ((referr < 3) || (remerr < 3)) esqual = 1;
    if (frac < 3) esqual = 1;

    if ((referr < 2) || (remerr < 2)) esqual = 2;

    return (esqual);
    }
