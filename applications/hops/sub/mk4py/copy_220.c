/************************************************************************/
/*									*/
/* This is the inverse of addr_220().  It takes an application		*/
/* structure and a target address, and places an overlay structure	*/
/* of the appropriate version into the target address.  Sometimes this	*/
/* will require a field-by-field copying operation, sometimes it will	*/
/* be a ptr assignment operation, depending on version control status	*/
/*									*/
/*	Inputs:		t220		application structure pointer	*/
/*									*/
/*	Output:		ptr		overlay structure address	*/
/*					with data filled in		*/
/*			return value	number of bytes filled in	*/
/*									*/
/* Created 25 September 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_220.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_220 (struct type_220 *t220,
          char **ptr)
    {
    int version;
					/* What version is requested for */
					/* the disk format? */
    sscanf (t220->version_no, "%2d", &version);
					/* Disk format same as app struct */
					/* Simple pointer assignment */
    if (version == T220_VERSION)
	{
	*ptr = (char *)t220;
	return (sizeof (struct type_220));
	}
                                        /* Only one version currently supported */
                                        /* so must be an error to get here */
    msg ("Unsupported record format version number %d for", 2, version);
    msg ("type 220 record in copy_220()", 2);
    return (-1);
    }
