/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_207().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t207            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/* Added version 1 16 March 2000 by CJL                                 */
/* Added version 2   2010.1.5    rjc                                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_207.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_207 (struct type_207 *t207,
          char **ptr)
    {
    int version;
    int i;
    struct type_207_v0 *t207_v0;
    struct type_207_v1 *t207_v1;
    struct type_207_v2 *t207_v2;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t207->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T207_VERSION) 
        *ptr = (char *)t207;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_207_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_207()", 2);
            return (-1);
            }
        }
    else if (version == 1)
        {
        *ptr = (char *)malloc (sizeof (struct type_207_v1));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_207()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_207()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t207_v0 = (struct type_207_v0 *) *ptr;
        strncpy (t207_v0->record_id, "207", 3);
        strncpy (t207_v0->version_no, "00", 2);
        for (i=0; i<16; i++)
            {
            cp_float (t207_v0->ref_pcamp[i].lsb, t207->ref_pcamp[i].lsb);
            cp_float (t207_v0->ref_pcamp[i].usb, t207->ref_pcamp[i].usb);
            cp_float (t207_v0->rem_pcamp[i].lsb, t207->rem_pcamp[i].lsb);
            cp_float (t207_v0->rem_pcamp[i].usb, t207->rem_pcamp[i].usb);
            cp_float (t207_v0->ref_pcphase[i].lsb, t207->ref_pcphase[i].lsb);
            cp_float (t207_v0->ref_pcphase[i].usb, t207->ref_pcphase[i].usb);
            cp_float (t207_v0->rem_pcphase[i].lsb, t207->rem_pcphase[i].lsb);
            cp_float (t207_v0->rem_pcphase[i].usb, t207->rem_pcphase[i].usb);
            cp_float (t207_v0->ref_pcfreq[i].lsb, t207->ref_pcfreq[i].lsb);
            cp_float (t207_v0->ref_pcfreq[i].usb, t207->ref_pcfreq[i].usb);
            cp_float (t207_v0->rem_pcfreq[i].lsb, t207->rem_pcfreq[i].lsb);
            cp_float (t207_v0->rem_pcfreq[i].usb, t207->rem_pcfreq[i].usb);
            cp_float (t207_v0->ref_errate[i], t207->ref_errate[i]);
            cp_float (t207_v0->rem_errate[i], t207->rem_errate[i]);
            }
        cp_float (t207_v0->ref_pcrate, t207->ref_pcrate);
        cp_float (t207_v0->rem_pcrate, t207->rem_pcrate);

        return (sizeof (struct type_207_v0));
        }
    else if (version == 1)
        {
        t207_v1 = (struct type_207_v1 *) *ptr;
        strncpy (t207_v1->record_id, "207", 3);
        strncpy (t207_v1->version_no, "01", 2);
        cp_int (t207_v1->pcal_mode, t207->pcal_mode);
        for (i=0; i<16; i++)
            {
            cp_float (t207_v1->ref_pcamp[i].lsb, t207->ref_pcamp[i].lsb);
            cp_float (t207_v1->ref_pcamp[i].usb, t207->ref_pcamp[i].usb);
            cp_float (t207_v1->rem_pcamp[i].lsb, t207->rem_pcamp[i].lsb);
            cp_float (t207_v1->rem_pcamp[i].usb, t207->rem_pcamp[i].usb);
            cp_float (t207_v1->ref_pcphase[i].lsb, t207->ref_pcphase[i].lsb);
            cp_float (t207_v1->ref_pcphase[i].usb, t207->ref_pcphase[i].usb);
            cp_float (t207_v1->rem_pcphase[i].lsb, t207->rem_pcphase[i].lsb);
            cp_float (t207_v1->rem_pcphase[i].usb, t207->rem_pcphase[i].usb);
            cp_float (t207_v1->ref_pcoffset[i].lsb, t207->ref_pcoffset[i].lsb);
            cp_float (t207_v1->ref_pcoffset[i].usb, t207->ref_pcoffset[i].usb);
            cp_float (t207_v1->rem_pcoffset[i].lsb, t207->rem_pcoffset[i].lsb);
            cp_float (t207_v1->rem_pcoffset[i].usb, t207->rem_pcoffset[i].usb);
            cp_float (t207_v1->ref_pcfreq[i].lsb, t207->ref_pcfreq[i].lsb);
            cp_float (t207_v1->ref_pcfreq[i].usb, t207->ref_pcfreq[i].usb);
            cp_float (t207_v1->rem_pcfreq[i].lsb, t207->rem_pcfreq[i].lsb);
            cp_float (t207_v1->rem_pcfreq[i].usb, t207->rem_pcfreq[i].usb);
            cp_float (t207_v1->ref_errate[i], t207->ref_errate[i]);
            cp_float (t207_v1->rem_errate[i], t207->rem_errate[i]);
            }
        cp_float (t207_v1->ref_pcrate, t207->ref_pcrate);
        cp_float (t207_v1->rem_pcrate, t207->rem_pcrate);

        return (sizeof (struct type_207_v1));
        }
    else if (version == 2)
        {
        t207_v2 = (struct type_207_v2 *) *ptr;
        strncpy (t207_v2->record_id, "207", 3);
        strncpy (t207_v2->version_no, "02", 2);
        cp_int (t207_v2->pcal_mode, t207->pcal_mode);
        for (i=0; i<64; i++)
            {
            cp_float (t207_v2->ref_pcamp[i].lsb, t207->ref_pcamp[i].lsb);
            cp_float (t207_v2->ref_pcamp[i].usb, t207->ref_pcamp[i].usb);
            cp_float (t207_v2->rem_pcamp[i].lsb, t207->rem_pcamp[i].lsb);
            cp_float (t207_v2->rem_pcamp[i].usb, t207->rem_pcamp[i].usb);
            cp_float (t207_v2->ref_pcphase[i].lsb, t207->ref_pcphase[i].lsb);
            cp_float (t207_v2->ref_pcphase[i].usb, t207->ref_pcphase[i].usb);
            cp_float (t207_v2->rem_pcphase[i].lsb, t207->rem_pcphase[i].lsb);
            cp_float (t207_v2->rem_pcphase[i].usb, t207->rem_pcphase[i].usb);
            cp_float (t207_v2->ref_pcoffset[i].lsb, t207->ref_pcoffset[i].lsb);
            cp_float (t207_v2->ref_pcoffset[i].usb, t207->ref_pcoffset[i].usb);
            cp_float (t207_v2->rem_pcoffset[i].lsb, t207->rem_pcoffset[i].lsb);
            cp_float (t207_v2->rem_pcoffset[i].usb, t207->rem_pcoffset[i].usb);
            cp_float (t207_v2->ref_pcfreq[i].lsb, t207->ref_pcfreq[i].lsb);
            cp_float (t207_v2->ref_pcfreq[i].usb, t207->ref_pcfreq[i].usb);
            cp_float (t207_v2->rem_pcfreq[i].lsb, t207->rem_pcfreq[i].lsb);
            cp_float (t207_v2->rem_pcfreq[i].usb, t207->rem_pcfreq[i].usb);
            cp_float (t207_v2->ref_errate[i], t207->ref_errate[i]);
            cp_float (t207_v2->rem_errate[i], t207->rem_errate[i]);
            }
        cp_float (t207_v2->ref_pcrate, t207->ref_pcrate);
        cp_float (t207_v2->rem_pcrate, t207->rem_pcrate);

        return (sizeof (struct type_207_v2));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_207()", 2, version);
        return (-1);
        }
    }
