/************************************************************************/
/*                                                                      */
/* Initialize a type_101 structure                                      */
/*                                                                      */
/*      Inputs:         t101            To be initialized               */
/*                                                                      */
/*      Output:         t101            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_101.h"
#include "mk4_dfio.h"

void
clear_101 (struct type_101 *t101)
    {
    char version[3];

    strncpy (t101->record_id, "101", 3);
    sprintf (version, "%02d", T101_VERSION);
    strncpy (t101->version_no, version, 2);
    t101->status = '\0';
    t101->nblocks = 0;

    t101->index = 0;
    t101->ref_chan_id[0] = '\0';
    t101->rem_chan_id[0] = '\0';
    t101->corr_board = 0;
    t101->corr_slot = 0;
    t101->ref_chan = 0;
    t101->rem_chan = 0;
    t101->post_mortem = 0;
    t101->nblocks = 0;
                                        /* Blocks element variable length */
    }
