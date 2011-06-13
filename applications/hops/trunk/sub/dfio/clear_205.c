/************************************************************************/
/*                                                                      */
/* Initialize a type_205 structure                                      */
/*                                                                      */
/*      Inputs:         t205            To be initialized               */
/*                                                                      */
/*      Output:         t205            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_205.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_205 (struct type_205 *t205)
    {
    char version[3];
    int i;
    int nchan;

    nchan = (T205_VERSION == 1) ? 64 : 16;

    strncpy (t205->record_id, "205", 3);
    sprintf (version, "%02d", T205_VERSION);
    strncpy (t205->version_no, version, 2);
    strncpy (t205->unused1, "   ", 3);

    clear_date (&(t205->utc_central));
    t205->offset = 0.0;
    strncpy (t205->ffmode, "      ", 6);
    for (i=0; i<6; i++) t205->search[i] = 0.0;
    for (i=0; i<8; i++) t205->filter[i] = 0.0;
    clear_date (&(t205->start));
    clear_date (&(t205->stop));
    t205->ref_freq = 0.0;
    for (i=0; i<nchan; i++)
        {
        t205->ffit_chan[i].ffit_chan_id = ' ';
        t205->ffit_chan[i].channels[0] = -1;
        t205->ffit_chan[i].channels[1] = -1;
        t205->ffit_chan[i].channels[2] = -1;
        t205->ffit_chan[i].channels[3] = -1;
        }
    }
