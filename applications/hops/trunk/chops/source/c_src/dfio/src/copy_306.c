/************************************************************************/
/*									*/
/* This is the inverse of addr_306().  It takes an application		*/
/* structure and a target address, and places an overlay structure	*/
/* of the appropriate version into the target address.  Sometimes this	*/
/* will require a field-by-field copying operation, sometimes it will	*/
/* be a ptr assignment operation, depending on version control status.	*/
/* To handle byte-flipping, the copy operation is now done in all cases	*/
/* which allows use of the bytflp.h macros				*/
/*									*/
/*	Inputs:		t306		application structure pointer	*/
/*									*/
/*	Output:		ptr		overlay structure address	*/
/*					with data filled in		*/
/*			return value	number of bytes filled in	*/
/*									*/
/* Created 25 September 1995 by CJL					*/
/* Redesigned 23 September 1997 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_306.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_306 (struct type_306 *t306,
          char **ptr)
    {
    int version;
    int i;
    struct type_306_v0 *t306_v0;
					/* What version is requested for */
					/* the disk format? */
    sscanf (t306->version_no, "%2d", &version);
					/* Disk format same as app struct */
					/* Simple pointer assignment */
    if (version == T306_VERSION) *ptr = (char *)t306;
    else if (version == 0)
	{
	*ptr = (char *)malloc (sizeof (struct type_306_v0));
	if (*ptr == NULL)
	    {
	    msg ("Memory allocation failure in copy_306()", 2);
	    return (-1);
	    }
	}
    else
	{
	msg ("Unrecognized version number %d in copy_306()", 2, version);
	return (-1);
	}
					/* Handle each version number */
					/* individually. */
    if (version == 0)
	{
	t306_v0 = (struct type_306_v0 *) *ptr;
	strncpy (t306_v0->record_id, "306", 3);
	strncpy (t306_v0->version_no, "00", 2);
        cp_short (t306_v0->time.year, t306->time.year);
        cp_short (t306_v0->time.day, t306->time.day);
        cp_short (t306_v0->time.hour, t306->time.hour);
        cp_short (t306_v0->time.minute, t306->time.minute);
        cp_float (t306_v0->time.second, t306->time.second);
        cp_float (t306_v0->duration, t306->duration);
	for (i=0; i<16; i++)
	    {
	    strcpy (t306_v0->stcount[i].chan_id, t306->stcount[i].chan_id);
            cp_int (t306_v0->stcount[i].bigpos, t306->stcount[i].bigpos);
            cp_int (t306_v0->stcount[i].pos, t306->stcount[i].pos);
            cp_int (t306_v0->stcount[i].neg, t306->stcount[i].neg);
            cp_int (t306_v0->stcount[i].bigneg, t306->stcount[i].bigneg);
	    }

	return (sizeof (struct type_306_v0));
	}
    else
	{
	msg ("Unrecognized version number %d in copy_306()", 2, version);
	return (-1);
	}
    }
