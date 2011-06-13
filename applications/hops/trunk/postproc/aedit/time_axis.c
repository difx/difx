/************************************************************************/
/*									*/
/* Determines if either axis is time_tag, and sets pd window elements	*/
/* absolute or relative to start of day 1, as requested.  It handles	*/
/* user override from the inputs as well.  All this fiddling is needed	*/
/* to persuade PGPLOT to label time axes sensibly.			*/
/*									*/
/*	Inputs:		pd		plot information structure ptr	*/
/*									*/
/*	Output:		pd		window elements suitably 	*/
/*					adjusted			*/
/*									*/
/* Created 24 February 1994 by CJL					*/
/*									*/
/************************************************************************/
#include "aedit.h"
#include "pstruct.h"
#include "summary.h"

void
time_axis (pd, fqex)
struct plot_info *pd;
struct frqexp fqex;
    {
    extern struct inputs inp;
    int tdiff, tmax, tmin, days;
					/* No time axis, do nothing */
    if ((pd->xaind != AX_TIMETAG) && (pd->yaind != AX_TIMETAG)) return;

					/* Natural range + 2.5% each end */
					/* Use fqex timerange to make all */
					/* plots line up with each other */
    if (inp.begin == inp.end)
	{
	tdiff = fqex.end - fqex.begin;
	if (tdiff == 0) tdiff = 300 * 40;
	tmax = fqex.end + tdiff/40;
	tmin = fqex.begin - tdiff/40;
	}
    else
					/* User override */
	{
	tmax = inp.end;
	tmin = inp.begin;
	}
					/* plot times are relative, subtract days */
    tmin -= pd->toffset;
    if (tmin < 0) tmin = 0;		/* Avoid nasty effects of 2.5% above */
    tmax -= pd->toffset;
					/* Slot into window elements as needed */
    if (pd->xaind == AX_TIMETAG)
	{
	pd->window[0] = tmin;
	pd->window[1] = tmax;
	}
    if (pd->yaind == AX_TIMETAG)
	{
	pd->window[2] = tmin;
	pd->window[3] = tmax;
	}

    return;
    }
