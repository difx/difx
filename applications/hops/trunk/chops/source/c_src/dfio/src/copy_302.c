/************************************************************************/
/*									*/
/* This is the inverse of addr_302().  It takes an application		*/
/* structure and a target address, and places an overlay structure	*/
/* of the appropriate version into the target address.  Sometimes this	*/
/* will require a field-by-field copying operation, sometimes it will	*/
/* be a ptr assignment operation, depending on version control status.	*/
/* To handle byte-flipping, the copy operation is now done in all cases	*/
/* which allows use of the bytflp.h macros				*/
/*									*/
/*	Inputs:		t302		application structure pointer	*/
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
#include "type_302.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_302 (struct type_302 *t302,
          char **ptr)
    {
    int version;
    int i;
    struct type_302_v0 *t302_v0;
					/* What version is requested for */
					/* the disk format? */
    sscanf (t302->version_no, "%2d", &version);
					/* Disk format same as app struct */
					/* Simple pointer assignment */
    if (version == T302_VERSION) *ptr = (char *)t302;
    else if (version == 0)
	{
	*ptr = (char *)malloc (sizeof (struct type_302_v0));
	if (*ptr == NULL)
	    {
	    msg ("Memory allocation failure in copy_302()", 2);
	    return (-1);
	    }
	}
    else
	{
	msg ("Unrecognized version number %d in copy_302()", 2, version);
	return (-1);
	}
					/* Handle each version number */
					/* individually. */
    if (version == 0)
	{
	t302_v0 = (struct type_302_v0 *) *ptr;
	strncpy (t302_v0->record_id, "302", 3);
	strncpy (t302_v0->version_no, "00", 2);
        cp_short (t302_v0->interval, t302->interval);
        strncpy (t302_v0->chan_id, t302->chan_id, 32);
	for (i=0; i<6; i++)
	    {
	    cp_double (t302_v0->phase_spline[i], t302->phase_spline[i]);
	    }

	return (sizeof (struct type_302_v0));
	}
    else
	{
	msg ("Unrecognized version number %d in copy_302()", 2, version);
	return (-1);
	}
    }
