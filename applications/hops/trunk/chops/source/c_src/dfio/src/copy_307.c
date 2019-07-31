/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_307().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t307            application structure pointer   */
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
#include "type_307.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_307 (struct type_307 *t307,
          char **ptr)
    {
    int version;
    int i, j;
    struct type_307_v0 *t307_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t307->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T307_VERSION) *ptr = (char *)t307;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_307_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_307()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_307()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t307_v0 = (struct type_307_v0 *) *ptr;
        strncpy (t307_v0->record_id, "307", 3);
        strncpy (t307_v0->version_no, "00", 2);

        cp_int (t307_v0->su, t307->su);
        cp_double (t307_v0->tot, t307->tot);
        cp_double (t307_v0->rot, t307->rot);
        cp_double (t307_v0->accum_period, t307->accum_period);
        cp_int (t307_v0->frame_count, t307->frame_count);
        for (i=0; i<16; i++)
            {
            for (j=0; j<8; j++) cp_int (t307_v0->counts[i].count[j],
                                                t307->counts[i].count[j]);
            cp_int (t307_v0->counts[i].val_count, t307->counts[i].val_count);
            }

        return (sizeof (struct type_307_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_307()", 2, version);
        return (-1);
        }
    }
