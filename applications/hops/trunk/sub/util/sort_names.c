/************************************************************************/
/*									*/
/* Library routine, to sort the requested input filenames into an	*/
/* order suitable for efficient processing and orderly output.  The 	*/
/* goal is for each root to be followed first by all its associated	*/
/* corel files, then by all its associated fringe, pcal and log files.  */
/* All the files associated with a particular root must reside in the	*/
/* same directory. If they do not, a warning message is printed and the	*/
/* offending file(s) is(are) ignored.  Thus, to call sort_names() on an */
/* fstruct structure array filled in a manner which omits root files is	*/
/* to produce an avalanche of error messages while getting nothing	*/
/* accomplished.  Why would anybody want such a list anyway, never mind	*/
/* wanting to sort it?							*/
/*									*/
/*	Inputs:		files		fstruct array with unsorted	*/
/*					entries				*/
/*			nfiles		Number of files in list		*/
/*									*/
/*	Output:		files		Sorted version, offending files	*/
/*					removed				*/
/*					return value  >0 = number of OK	*/
/*								files	*/
/*						      <0 = error	*/
/*									*/
/* Created 23 September 1992 by CJL for alist				*/
/* Revised and simplified 18 January 1993 by CJL for library use	*/
/* Modified for mk4, August 4 1995 by CJL				*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "fstruct.h"
#include "mk4_util.h"

int
sort_names(fstruct *files, int nfiles)
    {
    int i, j, k, fringe_orphan, pcal_orphan, log_orphan, corel_orphan;
					/* Initialize, throw away previous sorting */
    for (i=0; i<nfiles; i++) 
	{
	files[i].order = -1;
	files[i].done = FALSE;
	}
					/* Loop over all roots, ferreting out all */
					/* associated files and grouping them together */
    j = 0;
    for (i=0; i<nfiles; i++)
	{				/* Skip non-root files */
	if (files[i].type != 0) continue;
					/* Enter the root file itself */
	files[j++].order = i;
					/* Now look for all associated corel files */
					/* and insert those that match */
	for (k=0; k<nfiles; k++)
	    {
	    if (files[k].done) continue;
	    if (files[k].type != 1) continue;

	    if (! root_belong (files[i].name, files[k].name)) continue;

	    files[j++].order = k;
	    files[k].done = TRUE;
	    }
					/* Now look for all associated fringe files */
					/* and insert those that match */
	for (k=0; k<nfiles; k++)
	    {
	    if (files[k].done) continue;
	    if (files[k].type != 2) continue;

	    if (! root_belong (files[i].name, files[k].name)) continue;

	    files[j++].order = k;
	    files[k].done = TRUE;
	    }
					/* Now look for all associated pcal files */
					/* and insert those that match */
	for (k=0; k<nfiles; k++)
	    {
	    if (files[k].done) continue;
	    if (files[k].type != 3) continue;

	    if (! root_belong (files[i].name, files[k].name)) continue;

	    files[j++].order = k;
	    files[k].done = TRUE;
	    }
					/* Now look for associated log files */
					/* and insert those that match */
	for (k=0; k<nfiles; k++)
	    {
	    if (files[k].done) continue;
	    if (files[k].type != 4) continue;

	    if (! root_belong (files[i].name, files[k].name)) continue;

	    files[j++].order = k;
	    files[k].done = TRUE;
	    }
	}

					/* Tack on any orphan fringe, pcal or log files, */
					/* which are not illegal, but are undesirable */
    fringe_orphan = pcal_orphan = log_orphan = 0;
    for (i=0; i<nfiles; i++)
	{
	if ((! files[i].done) && (files[i].type >= 2))
	    {
	    files[j++].order = i;
	    if (files[i].type == 2) fringe_orphan++;
	    if (files[i].type == 3) pcal_orphan++;
	    if (files[i].type == 4) log_orphan++;
	    }
	}
    if (fringe_orphan + pcal_orphan + log_orphan > 0)
	{
	msg ("Warning, there were %d fringe files,", 2, fringe_orphan);
	msg ("  %d pcal files and %d log files specified for", 2, pcal_orphan, log_orphan);
	msg ("  which no parent root file could be found, or for which", 2);
	msg ("  the parent root file is in a different directory.", 2);
	msg ("  They have been placed at the end of the file list.", 2);
	}
					/* Finally we must search for orphan corel files */
					/* (which need to be reported to the user) */
    corel_orphan = 0;
    for (i=0; i<nfiles; i++)
	{
	if ((!files[i].done) && (files[i].type == 1))
	    {
	    msg ("Orphan corel file %s has no root!", 2, files[i].name);
	    corel_orphan++;
	    j++;			/* Keep count ... should add up to nfiles */
	    }
	}

    if (j != nfiles) msg ("Warning ... possible miscount in sort_names()", 2);

    return (j - corel_orphan);
    }
