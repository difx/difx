/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.  Either way, byte flipping is performed */
/* as necessary by the architecture-dependent macros cp_xxxx() defined  */
/* in bytflp.h                                                          */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         Return value    Address of filled app structure */
/*                                                                      */
/* Created 15 August 1995 by CJL                                        */
/* Redesigned 17 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_101.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_101 *
addr_101 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    short nblocks, *dummy;
    struct type_101 *t101;
    struct type_101_v0 *t101_v0;
                                        /* Need number of blocks up front */
    dummy = (short *)address;
    cp_short (nblocks, dummy[3]);
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T101_VERSION) t101 = (struct type_101 *)address;
    else
        {
        t101 = (struct type_101 *)malloc (sizeof (struct type_101) + 4*(nblocks-1));
        if (t101 == NULL)
            {
            msg ("Memory allocation failure in addr_101()", 2);
            return (NULL);
            }
        clear_101 (t101);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually.  First overlay */
                                        /* a version-specific structure, */
                                        /* then copy structure elements */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */

        *size = sizeof (struct type_101_v0) + 4*(nblocks-1);
        t101_v0 = (struct type_101_v0 *)address;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t101->record_id, "101", 3);
        strncpy (t101->version_no, "00", 2);
        cp_short (t101->nblocks, t101_v0->nblocks);
        cp_short (t101->index, t101_v0->index);
        cp_short (t101->primary, t101_v0->primary);
        if (t101 != t101_v0)
            {
            strncpy (t101->ref_chan_id, t101_v0->ref_chan_id, 8);
            strncpy (t101->rem_chan_id, t101_v0->rem_chan_id, 8);
            }
        cp_short (t101->corr_board, t101_v0->corr_board);
        cp_short (t101->corr_slot, t101_v0->corr_slot);
        cp_short (t101->ref_chan, t101_v0->ref_chan);
        cp_short (t101->rem_chan, t101_v0->rem_chan);
        cp_int (t101->post_mortem, t101_v0->post_mortem);
        for (i=0; i<nblocks; i++)
            {
            cp_int (t101->blocks[i], t101_v0->blocks[i]);
            }

        return (t101);
        }
    else 
        {
        msg ("Unrecognized type 101 record version number %d", 2, version);
        if (malloced) free (t101);
        return (NULL);
        }
    }
