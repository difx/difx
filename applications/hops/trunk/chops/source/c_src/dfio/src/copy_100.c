/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_100().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t100            application structure pointer   */
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
#include "type_100.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_100 (struct type_100 *t100,
          char **ptr)
    {
    int version;
    struct type_100_v0 *t100_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t100->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T100_VERSION) *ptr = (char *)t100;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_100_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_100()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_100()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t100_v0 = (struct type_100_v0 *) *ptr;
        strncpy (t100_v0->record_id, "100", 3);
        strncpy (t100_v0->version_no, "00", 2);
        cp_short (t100_v0->procdate.year, t100->procdate.year);
        cp_short (t100_v0->procdate.day, t100->procdate.day);
        cp_short (t100_v0->procdate.hour, t100->procdate.hour);
        cp_short (t100_v0->procdate.minute, t100->procdate.minute);
        cp_float (t100_v0->procdate.second, t100->procdate.second);
        strncpy (t100_v0->baseline, t100->baseline, 2);
        strcpy (t100_v0->rootname, t100->rootname);
        strncpy (t100_v0->qcode, t100->qcode, 2);
        cp_float (t100_v0->pct_done, t100->pct_done);
        cp_short (t100_v0->start.year, t100->start.year);
        cp_short (t100_v0->start.day, t100->start.day);
        cp_short (t100_v0->start.hour, t100->start.hour);
        cp_short (t100_v0->start.minute, t100->start.minute);
        cp_float (t100_v0->start.second, t100->start.second);
        cp_short (t100_v0->stop.year, t100->stop.year);
        cp_short (t100_v0->stop.day, t100->stop.day);
        cp_short (t100_v0->stop.hour, t100->stop.hour);
        cp_short (t100_v0->stop.minute, t100->stop.minute);
        cp_float (t100_v0->stop.second, t100->stop.second);
        cp_int (t100_v0->ndrec, t100->ndrec);
        cp_int (t100_v0->nindex, t100->nindex);
        cp_short (t100_v0->nlags, t100->nlags);
        cp_short (t100_v0->nblocks, t100->nblocks);

        return (sizeof (struct type_100_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_100()", 2, version);
        return (-1);
        }
    }
