/************************************************************************/
/*                                                                      */
/* Initialize a type_304 structure                                      */
/*                                                                      */
/*      Inputs:         t304            To be initialized               */
/*                                                                      */
/*      Output:         t304            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_304.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_304 (struct type_304 *t304)
    {
    int i;
    char version[3];

    strncpy (t304->record_id, "304", 3);
    sprintf (version, "%02d", T304_VERSION);
    strncpy (t304->version_no, version, 2);
    strncpy (t304->unused1, "   ", 3);

    clear_date (&(t304->time));
    t304->duration = 0.0;
    for (i=0; i<64; i++) 
        {
        t304->trackstats[i].error_count = 0;
        t304->trackstats[i].frames = 0;
        t304->trackstats[i].bad_frames = 0;
        t304->trackstats[i].slip_sync = 0;
        t304->trackstats[i].missing_sync = 0;
        t304->trackstats[i].crc_error = 0;
        }
    }
