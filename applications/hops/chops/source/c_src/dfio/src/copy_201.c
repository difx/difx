/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_201().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t201            application structure pointer   */
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
#include "type_201.h"
#include "mk4_util.h"

int
copy_201 (struct type_201 *t201,
          char **ptr)
    {
    int version;
    int i;
    struct type_201_v0 *t201_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t201->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T201_VERSION) *ptr = (char *)t201;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_201_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_201()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_201()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t201_v0 = (struct type_201_v0 *) *ptr;
        strncpy (t201_v0->record_id, "201", 3);
        strncpy (t201_v0->version_no, "00", 2);
        strncpy (t201_v0->source, t201->source, 32);
        cp_short (t201_v0->coord.ra_hrs, t201->coord.ra_hrs);
        cp_short (t201_v0->coord.ra_mins, t201->coord.ra_mins);
        cp_float (t201_v0->coord.ra_secs, t201->coord.ra_secs);
        cp_short (t201_v0->coord.dec_degs, t201->coord.dec_degs);
        cp_short (t201_v0->coord.dec_mins, t201->coord.dec_mins);
        cp_float (t201_v0->coord.dec_secs, t201->coord.dec_secs);
        cp_short (t201_v0->coord_date.year, t201->coord_date.year);
        cp_short (t201_v0->coord_date.day, t201->coord_date.day);
        cp_short (t201_v0->coord_date.hour, t201->coord_date.hour);
        cp_short (t201_v0->coord_date.minute, t201->coord_date.minute);
        cp_float (t201_v0->coord_date.second, t201->coord_date.second);
        cp_short (t201_v0->epoch, t201->epoch);
        cp_double (t201_v0->ra_rate, t201->ra_rate);
        cp_double (t201_v0->dec_rate, t201->dec_rate);
        for (i=0; i<4; i++)
            cp_double (t201_v0->pulsar_phase[i], t201->pulsar_phase[i]);
        cp_double (t201_v0->pulsar_epoch, t201->pulsar_epoch);
        cp_double (t201_v0->dispersion, t201->dispersion);

        return (sizeof (struct type_201_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_201()", 2, version);
        return (-1);
        }
    }
