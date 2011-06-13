/************************************************************************/
/*                                                                      */
/* Initialize a type_201 structure                                      */
/*                                                                      */
/*      Inputs:         t201            To be initialized               */
/*                                                                      */
/*      Output:         t201            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_201.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_201 (struct type_201 *t201)
    {
    char version[3];
    int i;

    strncpy (t201->record_id, "201", 3);
    sprintf (version, "%02d", T201_VERSION);
    strncpy (t201->version_no, version, 2);
    strncpy (t201->unused1, "   ", 3);

    t201->source[0] = '\0';
    t201->coord.ra_hrs = 0;
    t201->coord.ra_mins = 0;
    t201->coord.ra_secs = 0.0;
    t201->coord.dec_degs = 0;
    t201->coord.dec_mins = 0;
    t201->coord.dec_secs = 0.0;
    t201->epoch = 0;
    clear_date (&(t201->coord_date));
    t201->ra_rate = 0.0;
    t201->dec_rate = 0.0;
    for (i=0; i<4; i++) t201->pulsar_phase[i] = 0.0;
    t201->pulsar_epoch = 0.0;
    t201->dispersion = 0.0;
    }
