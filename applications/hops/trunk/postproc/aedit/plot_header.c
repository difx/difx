/************************************************************************/
/*									*/
/* Places a nice header, complete with suitable advertising, at the top */
/* of each page.							*/
/*									*/
/*	Inputs:		fqex		structure containing freq, expt */
/*			sptr		pointer to structure containing */
/*					source name (may be null)	*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created  March 6 1992 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "cpgplot.h"
#include "summary.h"
#include "aedit.h"

void plot_header (struct frqexp fqex, srcsum *sptr)
    {
    char buf[100], *dummy = "";

    cpgsvp (0.0, 1.0, 0.0, 0.9);

    if (sptr == NULL)
	sprintf (buf, "AEDIT plot - Expt %d, Freq %c",
				fqex.expt_no, fqex.freq_code);
    else
	sprintf (buf, "AEDIT plot - %s, Expt %d, Freq %c",
				sptr->name, fqex.expt_no, fqex.freq_code);

    cpgsch (1.0);
    cpgscf (2);
    cpgslw (2);
    cpglab (dummy, dummy, buf);
    cpgscf (1);
    cpgslw (1);
/*  cpgiden();       Doesn't work!  */
    }
