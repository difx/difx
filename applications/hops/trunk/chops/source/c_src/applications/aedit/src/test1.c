/************************************************************************/
/*									*/
/* General test routine.  Put experimental commands here for testing	*/
/* without having to mess with the rest of aedit.			*/
/*									*/
/*	Inputs:		arg1, arg2, remarg				*/
/*									*/
/*	Output:								*/
/*									*/
/* Created 4 March 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "flags.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

void test1 (esum *data, char *arg1, char *arg2, char *remarg)
    {
    fringearray *fdata;
    extern int fscan, fflag;
    int i, index1, index2;
    fringearray *datum1, *datum2;

    fdata = data->fdata;

					/* This little piece of code is for */
					/* reproducing the exact fringe/fourfit */
					/* datasets.  Because the fourfit */
					/* procdate is held only to 1 minute, */
					/* but fourfit executes in less than 1 sec */
					/* it's not easy to reproduce an edit duplicates */
					/* operation on the fringe data.  Here, you */
					/* merge the two datasets, sort to get dups */
					/* together, and zap anything that doesn't */
					/* match the rootcode of the 1st one */
					/* The fringe dataset should be dup edited, */
					/* the fourfit one should not */
    if (arg1[0] == 'x')
	{
	i = 0;
	while (TRUE)
	    {
	    if (i >= fscan) break;
	    index1 = fdata[i++].order;
	    datum1 = fdata + index1;
	    while (TRUE)
		{
		if (i >= fscan) break;
		index2 = fdata[i++].order;
		datum2 = fdata + index2;
		if ((strcmp (datum1->data.baseline, datum2->data.baseline) == 0)
			&& (datum1->data.freq_code == datum2->data.freq_code)
			&& (datum1->data.time_tag == datum2->data.time_tag))
		    {
		    if (strcmp (datum1->data.root_id, datum2->data.root_id) != 0)
			{
			datum2->flag |= ZAPPED;
			}
		    }
		else 
		    {
		    i--;
		    break;
		    }
		}
	    }
	return;
	}

    for (i=0; i<fscan; i+=2)
	{
	datum1 = fdata + i;
	datum2 = fdata + i + 1;

	datum2->flag |= ZAPPED;
	fflag++;

/*	datum1->data.snr -= datum2->data.snr; */
	datum1->data.length -= datum2->data.length;
/*	if (datum1->data.length < 0) datum1->data.length = -datum1->data.length; */
	datum1->data.amp -= datum2->data.amp;
	datum1->data.resid_phas = datum1->data.total_phas - datum2->data.total_phas;
	datum1->data.sbdelay -= datum2->data.sbdelay;
	datum1->data.mbdelay = 1000.0 * (datum1->data.total_mbdelay - 
			datum2->data.total_mbdelay);
	datum1->data.ambiguity *= 1000.0;
	datum1->data.delay_rate -= datum2->data.delay_rate;
/*	datum1->data.delay_rate = 1000.0 * (datum1->data.total_rate - 
			datum2->data.total_rate);	Why doesn't this work? */
	}
    }
