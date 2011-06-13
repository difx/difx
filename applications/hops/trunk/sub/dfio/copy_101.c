/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_101().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t101            application structure pointer   */
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
#include "type_101.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_101 (struct type_101 *t101,
          char **ptr)
    {
    int version;
    int i, nblocks;
    struct type_101_v0 *t101_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t101->version_no, "%2d", &version);
                                        /* Used for size calculations */
    nblocks = t101->nblocks;
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T101_VERSION) *ptr = (char *)t101;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_101_v0) + 4*(nblocks-1));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_101()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_101()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t101_v0 = (struct type_101_v0 *) *ptr;
        strncpy (t101_v0->record_id, "101", 3);
        strncpy (t101_v0->version_no, "00", 2);
        cp_short (t101_v0->nblocks, t101->nblocks);
        cp_short (t101_v0->index, t101->index);
        cp_short (t101_v0->primary, t101->primary);
        strncpy (t101_v0->ref_chan_id, t101->ref_chan_id, 8);
        strncpy (t101_v0->rem_chan_id, t101->rem_chan_id, 8);
        cp_short (t101_v0->corr_board, t101->corr_board);
        cp_short (t101_v0->corr_slot, t101->corr_slot);
        cp_short (t101_v0->ref_chan, t101->ref_chan);
        cp_short (t101_v0->rem_chan, t101->rem_chan);
        cp_int (t101_v0->post_mortem, t101->post_mortem);
        for (i=0; i<nblocks; i++)
            {
            cp_int (t101_v0->blocks[i], t101->blocks[i]);
            }

        return (sizeof (struct type_101_v0) + 4*(nblocks-1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_101()", 2, version);
        return (-1);
        }
    }
