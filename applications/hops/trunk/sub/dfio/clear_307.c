/************************************************************************/
/*                                                                      */
/* Initialize a type_307 structure                                      */
/*                                                                      */
/*      Inputs:         t307            To be initialized               */
/*                                                                      */
/*      Output:         t307            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_307.h"
#include "mk4_dfio.h"

void
clear_307 (struct type_307 *t307)
    {
    int i, j;
    char version[3];

    strncpy (t307->record_id, "307", 3);
    sprintf (version, "%02d", T307_VERSION);
    strncpy (t307->version_no, version, 2);
    strncpy (t307->unused1, "   ", 3);

    t307->su = -1;
    t307->tot = 0.0;
    t307->rot = 0.0;
    t307->accum_period = 0.0;
    t307->frame_count = 0.0;

    for (i=0; i<16; i++)
        {
        for (j=0; j<8; j++) t307->counts[i].count[j] = 0;
        t307->counts[i].val_count = 0;
        }

    }
