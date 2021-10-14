/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_204().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t204            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_204.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_204 (struct type_204 *t204,
          char **ptr)
    {
    int version;
    struct type_204_v0 *t204_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t204->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T204_VERSION) *ptr = (char *)t204;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_204_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_204()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_204()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t204_v0 = (struct type_204_v0 *) *ptr;
        strncpy (t204_v0->record_id, "204", 3);
        strncpy (t204_v0->version_no, "00", 2);
        cp_short (t204_v0->ff_version[0], t204->ff_version[0]);
        cp_short (t204_v0->ff_version[1], t204->ff_version[1]);
        strncpy (t204_v0->platform, t204->platform, 8);
        strncpy (t204_v0->control_file, t204->control_file, 96);
        cp_short (t204_v0->ffcf_date.year, t204->ffcf_date.year);
        cp_short (t204_v0->ffcf_date.day, t204->ffcf_date.day);
        cp_short (t204_v0->ffcf_date.hour, t204->ffcf_date.hour);
        cp_short (t204_v0->ffcf_date.minute, t204->ffcf_date.minute);
        cp_float (t204_v0->ffcf_date.second, t204->ffcf_date.second);
        strncpy (t204_v0->override, t204->override, 128);

        return (sizeof (struct type_204_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_204()", 2, version);
        return (-1);
        }
    }
