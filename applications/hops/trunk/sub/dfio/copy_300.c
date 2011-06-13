/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_300().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t300            application structure pointer   */
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
#include <string.h>
#include "bytflp.h"
#include "type_300.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_300 (struct type_300 *t300,
          char **ptr)
    {
    int version;
    struct type_300_v0 *t300_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t300->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T300_VERSION) *ptr = (char *)t300;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_300_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_300()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_300()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t300_v0 = (struct type_300_v0 *) *ptr;
        strncpy (t300_v0->record_id, "300", 3);
        strncpy (t300_v0->version_no, "00", 2);
        t300_v0->SU_number = t300->SU_number;
        t300_v0->id = t300->id;
        strncpy (t300_v0->intl_id, t300->intl_id, 2);
        strncpy (t300_v0->name, t300->name, 9);
        cp_short (t300_v0->model_start.year, t300->model_start.year);
        cp_short (t300_v0->model_start.day, t300->model_start.day);
        cp_short (t300_v0->model_start.hour, t300->model_start.hour);
        cp_short (t300_v0->model_start.minute, t300->model_start.minute);
        cp_float (t300_v0->model_start.second, t300->model_start.second);
        cp_float (t300_v0->model_interval, t300->model_interval);
        cp_short (t300_v0->nsplines, t300->nsplines);

        return (sizeof (struct type_300_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_300()", 2, version);
        return (-1);
        }
    }
