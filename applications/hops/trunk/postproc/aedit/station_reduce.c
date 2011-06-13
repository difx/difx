/************************************************************************/
/*									*/
/* Temporarily tags fringe data, leaving a set of data involving a	*/
/* given station that is non-redundant for station-based quantities.	*/
/* For efficiency in plotting.						*/
/*									*/
/*	Inputs:		fdata		main fringe data array		*/
/*			station		The station to filter for	*/
/*			source		Ignore all other sources	*/
/*			expt		Ignore all other experiments	*/
/*			freq		Ignore all other freq codes	*/
/*									*/
/*	Output:		fdata		All unwanted data have TAGGED	*/
/*					bit set in flag field		*/
/*									*/
/* Created 11 March 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "flags.h"

void
station_reduce (fdata, station, source, expt, freq)
fringearray *fdata;
char station, *source, freq;
int expt;
    {
    extern int fscan;
    int i, index, last_time, maxfreq;
    fringesum *datum;
					/* Time sort already done in plot() */

					/* Figure out how many freqs this station */
					/* subgroup should have in this expt */
					/* Tag all scans not of correct station, */
					/* source, subgroup or expt */
    maxfreq = 0;
    for (i=0; i<fscan; i++)
	{
	index = fdata[i].order;
	if (fdata[index].flag) continue;
	datum = &(fdata[index].data);
					/* Strip out all non-matches first */
	if ((strchr (datum->baseline, station) == NULL) ||
		(strcmp (datum->source, source) != 0) || (datum->expt_no != expt) ||
		(datum->freq_code != freq)) fdata[index].flag |= TAGGED;

	else if (datum->no_freq > maxfreq) maxfreq = datum->no_freq;
	}
					/* Now loop over data tagging duplicates */
					/* and scans with too few freqs */
    for (i=0; i<fscan; i++)
	{
	index = fdata[i].order;
	if (fdata[index].flag) continue;
	datum = &(fdata[index].data);
					/* Filter on #freq */
	if (datum->no_freq < maxfreq) fdata[index].flag |= TAGGED;
					/* Ignore duplicate times.  Only the */
					/* first occurrence remains unflagged */
	else if (datum->time_tag == last_time) fdata[index].flag |= TAGGED;
	else last_time = datum->time_tag;
	}

    return;
    }

