/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_205().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t205            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/*  version 2              2010.1.5  rjc                                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_205.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_205 (struct type_205 *t205,
          char **ptr)
    {
    int version;
    int i;
    struct type_205_v0 *t205_v0;
    struct type_205_v1 *t205_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t205->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T205_VERSION) *ptr = (char *)t205;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_205_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_205()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_205()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t205_v0 = (struct type_205_v0 *) *ptr;
        strncpy (t205_v0->record_id, "205", 3);
        strncpy (t205_v0->version_no, "00", 2);
        cp_short (t205_v0->utc_central.year, t205->utc_central.year);
        cp_short (t205_v0->utc_central.day, t205->utc_central.day);
        cp_short (t205_v0->utc_central.hour, t205->utc_central.hour);
        cp_short (t205_v0->utc_central.minute, t205->utc_central.minute);
        cp_float (t205_v0->utc_central.second, t205->utc_central.second);
        cp_float (t205_v0->offset, t205->offset);
        strncpy (t205_v0->ffmode, t205->ffmode, 8);
        for (i=0; i<6; i++) cp_float (t205_v0->search[i], t205->search[i]);
        for (i=0; i<8; i++) cp_float (t205_v0->filter[i], t205->filter[i]);
        cp_short (t205_v0->start.year, t205->start.year);
        cp_short (t205_v0->start.day, t205->start.day);
        cp_short (t205_v0->start.hour, t205->start.hour);
        cp_short (t205_v0->start.minute, t205->start.minute);
        cp_float (t205_v0->start.second, t205->start.second);
        cp_short (t205_v0->stop.year, t205->stop.year);
        cp_short (t205_v0->stop.day, t205->stop.day);
        cp_short (t205_v0->stop.hour, t205->stop.hour);
        cp_short (t205_v0->stop.minute, t205->stop.minute);
        cp_float (t205_v0->stop.second, t205->stop.second);
        cp_double (t205_v0->ref_freq, t205->ref_freq);
        for (i=0; i<16; i++)
            {
            t205_v0->ffit_chan[i].ffit_chan_id = t205->ffit_chan[i].ffit_chan_id;
            cp_short (t205_v0->ffit_chan[i].channels[0], t205->ffit_chan[i].channels[0]);
            cp_short (t205_v0->ffit_chan[i].channels[1], t205->ffit_chan[i].channels[1]);
            cp_short (t205_v0->ffit_chan[i].channels[2], t205->ffit_chan[i].channels[2]);
            cp_short (t205_v0->ffit_chan[i].channels[3], t205->ffit_chan[i].channels[3]);
            }

        return (sizeof (struct type_205_v0));
        }
    else if (version == 1)
        {
        t205_v1 = (struct type_205_v1 *) *ptr;
        strncpy (t205_v1->record_id, "205", 3);
        strncpy (t205_v1->version_no, "01", 2);
        cp_short (t205_v1->utc_central.year, t205->utc_central.year);
        cp_short (t205_v1->utc_central.day, t205->utc_central.day);
        cp_short (t205_v1->utc_central.hour, t205->utc_central.hour);
        cp_short (t205_v1->utc_central.minute, t205->utc_central.minute);
        cp_float (t205_v1->utc_central.second, t205->utc_central.second);
        cp_float (t205_v1->offset, t205->offset);
        strncpy (t205_v1->ffmode, t205->ffmode, 8);
        for (i=0; i<6; i++) cp_float (t205_v1->search[i], t205->search[i]);
        for (i=0; i<8; i++) cp_float (t205_v1->filter[i], t205->filter[i]);
        cp_short (t205_v1->start.year, t205->start.year);
        cp_short (t205_v1->start.day, t205->start.day);
        cp_short (t205_v1->start.hour, t205->start.hour);
        cp_short (t205_v1->start.minute, t205->start.minute);
        cp_float (t205_v1->start.second, t205->start.second);
        cp_short (t205_v1->stop.year, t205->stop.year);
        cp_short (t205_v1->stop.day, t205->stop.day);
        cp_short (t205_v1->stop.hour, t205->stop.hour);
        cp_short (t205_v1->stop.minute, t205->stop.minute);
        cp_float (t205_v1->stop.second, t205->stop.second);
        cp_double (t205_v1->ref_freq, t205->ref_freq);
        for (i=0; i<64; i++)
            {
            t205_v1->ffit_chan[i].ffit_chan_id = t205->ffit_chan[i].ffit_chan_id;
            cp_short (t205_v1->ffit_chan[i].channels[0], t205->ffit_chan[i].channels[0]);
            cp_short (t205_v1->ffit_chan[i].channels[1], t205->ffit_chan[i].channels[1]);
            cp_short (t205_v1->ffit_chan[i].channels[2], t205->ffit_chan[i].channels[2]);
            cp_short (t205_v1->ffit_chan[i].channels[3], t205->ffit_chan[i].channels[3]);
            }

        return (sizeof (struct type_205_v1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_205()", 2, version);
        return (-1);
        }
    }
