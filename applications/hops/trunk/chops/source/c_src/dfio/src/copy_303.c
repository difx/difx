/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_303().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t303            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/* Adopted for use with az, el, and pa    rjc 2012.2.21                 */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_303.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_303 (struct type_303 *t303,
          char **ptr)
    {
    int version,
        i;
    struct type_303_v0 *t303_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t303->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T303_VERSION) 
        *ptr = (char *)t303;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_303_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_303()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_303()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t303_v0 = (struct type_303_v0 *) *ptr;
        strncpy (t303_v0->record_id, "303", 3);
        strncpy (t303_v0->version_no, "00", 2);

        cp_short (t303->interval, t303->interval);
        strncpy (t303->chan_id, t303->chan_id, 32);
        for (i=0; i<6; i++)
            {
            cp_double (t303->azimuth[i], t303->azimuth[i]);
            cp_double (t303->elevation[i], t303->elevation[i]);
            cp_double (t303->parallactic_angle[i], t303->parallactic_angle[i]);
            cp_double (t303->u[i], t303->u[i]);
            cp_double (t303->v[i], t303->v[i]);
            cp_double (t303->w[i], t303->w[i]);
            }

        return (sizeof (struct type_303_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_303()", 2, version);
        return (-1);
        }
    }
