/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_206().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t206            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/* expanded to 64 channels          rjc   2010.1.5                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_206.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_206 (struct type_206 *t206,
          char **ptr)
    {
    int version;
    int i;
    struct type_206_v0 *t206_v0;
    struct type_206_v1 *t206_v1;
    struct type_206_v2 *t206_v2;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t206->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T206_VERSION) *ptr = (char *)t206;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_206_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_206()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_206()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t206_v0 = (struct type_206_v0 *) *ptr;
        strncpy (t206_v0->record_id, "206", 3);
        strncpy (t206_v0->version_no, "00", 2);
        cp_short (t206_v0->start.year, t206->start.year);
        cp_short (t206_v0->start.day, t206->start.day);
        cp_short (t206_v0->start.hour, t206->start.hour);
        cp_short (t206_v0->start.minute, t206->start.minute);
        cp_float (t206_v0->start.second, t206->start.second);
        cp_short (t206_v0->first_ap, t206->first_ap);
        cp_short (t206_v0->last_ap, t206->last_ap);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v0->accepted[i].lsb, t206->accepted[i].lsb);
            cp_short (t206_v0->accepted[i].usb, t206->accepted[i].usb);
            }
        cp_float (t206_v0->intg_time, t206->intg_time);
        cp_float (t206_v0->accept_ratio, t206->accept_ratio);
        cp_float (t206_v0->discard, t206->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v0->reason1[i].lsb, t206->reason1[i].lsb);
            cp_short (t206_v0->reason1[i].usb, t206->reason1[i].usb);
            cp_short (t206_v0->reason2[i].lsb, t206->reason2[i].lsb);
            cp_short (t206_v0->reason2[i].usb, t206->reason2[i].usb);
            cp_short (t206_v0->reason3[i].lsb, t206->reason3[i].lsb);
            cp_short (t206_v0->reason3[i].usb, t206->reason3[i].usb);
            cp_short (t206_v0->reason4[i].lsb, t206->reason4[i].lsb);
            cp_short (t206_v0->reason4[i].usb, t206->reason4[i].usb);
            cp_short (t206_v0->reason5[i].lsb, t206->reason5[i].lsb);
            cp_short (t206_v0->reason5[i].usb, t206->reason5[i].usb);
            cp_short (t206_v0->reason6[i].lsb, t206->reason6[i].lsb);
            cp_short (t206_v0->reason6[i].usb, t206->reason6[i].usb);
            cp_short (t206_v0->reason7[i].lsb, t206->reason7[i].lsb);
            cp_short (t206_v0->reason7[i].usb, t206->reason7[i].usb);
            cp_short (t206_v0->reason8[i].lsb, t206->reason8[i].lsb);
            cp_short (t206_v0->reason8[i].usb, t206->reason8[i].usb);
            }
        cp_short (t206_v0->ratesize, t206->ratesize);
        cp_short (t206_v0->mbdsize, t206->mbdsize);
        cp_short (t206_v0->sbdsize, t206->sbdsize);

        return (sizeof (struct type_206_v0));
        }

    else if (version == 1)
        {
        t206_v1 = (struct type_206_v1 *) *ptr;
        strncpy (t206_v1->record_id, "206", 3);
        strncpy (t206_v1->version_no, "01", 2);
        cp_short (t206_v1->start.year, t206->start.year);
        cp_short (t206_v1->start.day, t206->start.day);
        cp_short (t206_v1->start.hour, t206->start.hour);
        cp_short (t206_v1->start.minute, t206->start.minute);
        cp_float (t206_v1->start.second, t206->start.second);
        cp_short (t206_v1->first_ap, t206->first_ap);
        cp_short (t206_v1->last_ap, t206->last_ap);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v1->accepted[i].lsb, t206->accepted[i].lsb);
            cp_short (t206_v1->accepted[i].usb, t206->accepted[i].usb);
            cp_double (t206_v1->weights[i].lsb, t206->weights[i].lsb);
            cp_double (t206_v1->weights[i].usb, t206->weights[i].usb);
            }
        cp_float (t206_v1->intg_time, t206->intg_time);
        cp_float (t206_v1->accept_ratio, t206->accept_ratio);
        cp_float (t206_v1->discard, t206->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v1->reason1[i].lsb, t206->reason1[i].lsb);
            cp_short (t206_v1->reason1[i].usb, t206->reason1[i].usb);
            cp_short (t206_v1->reason2[i].lsb, t206->reason2[i].lsb);
            cp_short (t206_v1->reason2[i].usb, t206->reason2[i].usb);
            cp_short (t206_v1->reason3[i].lsb, t206->reason3[i].lsb);
            cp_short (t206_v1->reason3[i].usb, t206->reason3[i].usb);
            cp_short (t206_v1->reason4[i].lsb, t206->reason4[i].lsb);
            cp_short (t206_v1->reason4[i].usb, t206->reason4[i].usb);
            cp_short (t206_v1->reason5[i].lsb, t206->reason5[i].lsb);
            cp_short (t206_v1->reason5[i].usb, t206->reason5[i].usb);
            cp_short (t206_v1->reason6[i].lsb, t206->reason6[i].lsb);
            cp_short (t206_v1->reason6[i].usb, t206->reason6[i].usb);
            cp_short (t206_v1->reason7[i].lsb, t206->reason7[i].lsb);
            cp_short (t206_v1->reason7[i].usb, t206->reason7[i].usb);
            cp_short (t206_v1->reason8[i].lsb, t206->reason8[i].lsb);
            cp_short (t206_v1->reason8[i].usb, t206->reason8[i].usb);
            }
        cp_short (t206_v1->ratesize, t206->ratesize);
        cp_short (t206_v1->mbdsize, t206->mbdsize);
        cp_short (t206_v1->sbdsize, t206->sbdsize);

        return (sizeof (struct type_206_v1));
        }
    else if (version == 2)
        {
        t206_v2 = (struct type_206_v2 *) *ptr;
        strncpy (t206_v2->record_id, "206", 3);
        strncpy (t206_v2->version_no, "02", 2);
        cp_short (t206_v2->start.year, t206->start.year);
        cp_short (t206_v2->start.day, t206->start.day);
        cp_short (t206_v2->start.hour, t206->start.hour);
        cp_short (t206_v2->start.minute, t206->start.minute);
        cp_float (t206_v2->start.second, t206->start.second);
        cp_short (t206_v2->first_ap, t206->first_ap);
        cp_short (t206_v2->last_ap, t206->last_ap);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v2->accepted[i].lsb, t206->accepted[i].lsb);
            cp_short (t206_v2->accepted[i].usb, t206->accepted[i].usb);
            cp_double (t206_v2->weights[i].lsb, t206->weights[i].lsb);
            cp_double (t206_v2->weights[i].usb, t206->weights[i].usb);
            }
        cp_float (t206_v2->intg_time, t206->intg_time);
        cp_float (t206_v2->accept_ratio, t206->accept_ratio);
        cp_float (t206_v2->discard, t206->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206_v2->reason1[i].lsb, t206->reason1[i].lsb);
            cp_short (t206_v2->reason1[i].usb, t206->reason1[i].usb);
            cp_short (t206_v2->reason2[i].lsb, t206->reason2[i].lsb);
            cp_short (t206_v2->reason2[i].usb, t206->reason2[i].usb);
            cp_short (t206_v2->reason3[i].lsb, t206->reason3[i].lsb);
            cp_short (t206_v2->reason3[i].usb, t206->reason3[i].usb);
            cp_short (t206_v2->reason4[i].lsb, t206->reason4[i].lsb);
            cp_short (t206_v2->reason4[i].usb, t206->reason4[i].usb);
            cp_short (t206_v2->reason5[i].lsb, t206->reason5[i].lsb);
            cp_short (t206_v2->reason5[i].usb, t206->reason5[i].usb);
            cp_short (t206_v2->reason6[i].lsb, t206->reason6[i].lsb);
            cp_short (t206_v2->reason6[i].usb, t206->reason6[i].usb);
            cp_short (t206_v2->reason7[i].lsb, t206->reason7[i].lsb);
            cp_short (t206_v2->reason7[i].usb, t206->reason7[i].usb);
            cp_short (t206_v2->reason8[i].lsb, t206->reason8[i].lsb);
            cp_short (t206_v2->reason8[i].usb, t206->reason8[i].usb);
            }
        cp_short (t206_v2->ratesize, t206->ratesize);
        cp_short (t206_v2->mbdsize, t206->mbdsize);
        cp_short (t206_v2->sbdsize, t206->sbdsize);

        return (sizeof (struct type_206_v2));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_206()", 2, version);
        return (-1);
        }
    }
