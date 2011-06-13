/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_212().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t212            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 19 April 2001 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_212.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_212 (struct type_212 *t212,
          char **ptr)
    {
    int version;
    int i, nap, size;
    struct type_212_v0 *t212_v0;
    struct type_212_v1 *t212_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t212->version_no, "%2d", &version);
                                        /* Used for size calculations */
    nap = t212->nap;
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T212_VERSION) *ptr = (char *)t212;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_212_v0) + 8*(nap-1));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_212()", 2);
            return (-1);
            }
        }
    else if (version == 1)
        {
        size = sizeof (struct type_212_v1) + 12*(nap-1);
        if ((nap % 2) == 1) size += 12;
        *ptr = (char *)malloc (size);
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_212()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_212()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t212_v0 = (struct type_212_v0 *) *ptr;
        strncpy (t212_v0->record_id, "212", 3);
        strncpy (t212_v0->version_no, "00", 2);
        cp_short (t212_v0->nap, t212->nap);
        cp_short (t212_v0->channel, t212->channel);
        cp_short (t212_v0->sbd_chan, t212->sbd_chan);
        for (i=0; i<nap; i++)
            {
            cp_float (t212_v0->data[i].amp, t212->data[i].amp);
            cp_float (t212_v0->data[i].phase, t212->data[i].phase);
            }

        return (sizeof (struct type_212_v0) + 8*(nap-1));
        }
    else if (version == 1)
        {
        t212_v1 = (struct type_212_v1 *) *ptr;
        strncpy (t212_v1->record_id, "212", 3);
        strncpy (t212_v1->version_no, "01", 2);
        cp_short (t212_v1->nap, t212->nap);
        cp_short (t212_v1->channel, t212->channel);
        cp_short (t212_v1->sbd_chan, t212->sbd_chan);
        for (i=0; i<nap; i++)
            {
            cp_float (t212_v1->data[i].amp, t212->data[i].amp);
            cp_float (t212_v1->data[i].phase, t212->data[i].phase);
            cp_float (t212_v1->data[i].weight, t212->data[i].weight);
            }

        size = sizeof (struct type_212_v1) + 12*(nap-1);
        if ((nap % 2) == 1) size += 12;
        return (size);
        }
    else
        {
        msg ("Unrecognized version number %d in copy_212()", 2, version);
        return (-1);
        }
    }
