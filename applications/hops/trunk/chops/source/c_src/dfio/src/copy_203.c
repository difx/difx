/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_203().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t203            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/* Add version 1 support          2011.10.6        rjc                  */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_203.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_203 (struct type_203 *t203,
          char **ptr)
    {
    int version;
    int i;
    struct type_203_v0 *t203_v0;
    struct type_203_v1 *t203_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t203->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T203_VERSION) *ptr = (char *)t203;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_203_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_203()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_203()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t203_v0 = (struct type_203_v0 *) *ptr;
        strncpy (t203_v0->record_id, "203", 3);
        strncpy (t203_v0->version_no, "00", 2);
        for (i=0; i<32; i++)
            {
            cp_short (t203_v0->channels[i].index, t203->channels[i].index);
            cp_short (t203_v0->channels[i].sample_rate, t203->channels[i].sample_rate);
            t203_v0->channels[i].refsb = t203->channels[i].refsb;
            t203_v0->channels[i].remsb = t203->channels[i].remsb;
            t203_v0->channels[i].refpol = t203->channels[i].refpol;
            t203_v0->channels[i].rempol = t203->channels[i].rempol;
            cp_double (t203_v0->channels[i].ref_freq, t203->channels[i].ref_freq);
            cp_double (t203_v0->channels[i].rem_freq, t203->channels[i].rem_freq);
            strncpy (t203_v0->channels[i].ref_chan_id, t203->channels[i].ref_chan_id, 8);
            strncpy (t203_v0->channels[i].rem_chan_id, t203->channels[i].rem_chan_id, 8);
            }

        return (sizeof (struct type_203_v0));
        }
    else if (version == 1)
        {
        t203_v1 = (struct type_203_v1 *) *ptr;
        strncpy (t203_v1->record_id, "203", 3);
        strncpy (t203_v1->version_no, "01", 2);
        for (i=0; i<8*MAXFREQ; i++)
            {
            cp_short (t203_v1->channels[i].index, t203->channels[i].index);
            cp_short (t203_v1->channels[i].sample_rate, t203->channels[i].sample_rate);
            t203_v1->channels[i].refsb = t203->channels[i].refsb;
            t203_v1->channels[i].remsb = t203->channels[i].remsb;
            t203_v1->channels[i].refpol = t203->channels[i].refpol;
            t203_v1->channels[i].rempol = t203->channels[i].rempol;
            cp_double (t203_v1->channels[i].ref_freq, t203->channels[i].ref_freq);
            cp_double (t203_v1->channels[i].rem_freq, t203->channels[i].rem_freq);
            strncpy (t203_v1->channels[i].ref_chan_id, t203->channels[i].ref_chan_id, 8);
            strncpy (t203_v1->channels[i].rem_chan_id, t203->channels[i].rem_chan_id, 8);
            }

        return (sizeof (struct type_203_v1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_203()", 2, version);
        return (-1);
        }
    }
