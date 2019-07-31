/************************************************************************/
/*									*/
/* This routine determines the status of a data point with respect to	*/
/* the input filter settings.  It has two modes.  If the mode is QUICK,	*/
/* the routine returns a non-zero value as soon as a filter is not	*/
/* passed, without checking additional filters.  If the mode is SLOW, 	*/
/* all filters are checked, and the return value has the appropriate	*/
/* bit set for each filter not passed.  Efficiency is achieved by first	*/
/* calling active_filter(), which allows filters in their pass-all	*/
/* state to be completely bypassed.					*/
/*									*/
/*	Inputs:		datum		one element of data array	*/
/*			mode		QUICK or SLOW			*/
/*			inp structure	User input settings		*/
/*			fcheck,nfilt	Active filter information	*/
/*					(external from active_filter())	*/
/*									*/
/*	Output:		return value	0 ==> passed all filters	*/
/*									*/
/* Created 30 April 1990 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "aedit.h"
#include "flags.h"

#define QUICK 0
#define SLOW 1

int rfilter (rootarray *rdatum, int mode)
    {
    extern struct inputs inp;
    extern int fcheck[12], nfilt;
    int i, j, n, ret, nf, frac, p0;
    float pval;
    rootsum *datum;

    ret = 0;
    datum = &(rdatum->data);
    for (i=0;i<nfilt;i++) 
	{
	switch (fcheck[i]) 
	    {
	    case F_TIMETAG:
		if (inp.begin == 0 && inp.end == 0) break;
		if (datum->time_tag < inp.begin || datum->time_tag > inp.end)
				ret |= BADTIME;
		break;
            case F_PROCDATE:
		if (inp.proc_begin == 0 && inp.proc_end == 0) break;
		if (datum->procdate < inp.proc_begin || datum->procdate > inp.proc_end)
				ret |= BADPROC;
		break;
	    case F_STATION:
		n = strlen (datum->stations);
		for (j=0; j<n; j++)
		    if (strchr (inp.stations, datum->stations[j]) != NULL) break;
		if (j == n) ret |= BADSTATION;
		break;
	    case F_EXPERIMENT:
		if (inp.experiment != datum->expt_no) ret |= BADEXPT;
		break;
	    case F_SOURCE:
		if (smatch (inp.sources, datum->source) != 2) ret |= BADSOURCE;
		break;
	    case F_TYPE:
		if (strchr (inp.type, '0') == NULL) ret |= BADTYPE;
		break;
	    case F_BASELINE:
	    case F_TRIANGLE:
	    case F_QUAD:
	    case F_QCODE:
	    case F_FREQUENCY:
	    case F_SNR:
	    case F_BSNR:
	    case F_LENGTH:
	    case F_FRACTION:
	    case F_NFREQ:
	    case F_PARAMETER:
		break;

	    default:
		msg ("error in rfilter.c", 2);
		return (ERROR);
	    }				/* End switch */

	if (mode == QUICK && ret != 0) return (ret);
        }
    return (ret);
    }
