/************************************************************************/
/*                                                                      */
/* Initialize a type_203 structure                                      */
/*                                                                      */
/*      Inputs:         t203            To be initialized               */
/*                                                                      */
/*      Output:         t203            Initialization complete         */
/*                                                                      */
/* Created May 13 1999 by CJL                                           */
/* modify for version 1 support       2011.10.6     rjc                 */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_203.h"
#include "mk4_dfio.h"

void
clear_203 (struct type_203 *t203)
    {
    char version[3];
    int i;
    int nchan;

    nchan = (T203_VERSION == 1) ? 8 * MAXFREQ : 32;

    strncpy (t203->record_id, "203", 3);
    sprintf (version, "%02d", T203_VERSION);
    strncpy (t203->version_no, version, 2);
    strncpy (t203->unused1, "   ", 3);

    for (i=0; i<nchan; i++)
        {
        t203->channels[i].index = -1;
        t203->channels[i].sample_rate = 0;
        t203->channels[i].refsb = ' ';
        t203->channels[i].remsb = ' ';
        t203->channels[i].refpol = ' ';
        t203->channels[i].rempol = ' ';
        t203->channels[i].ref_freq = 0.0;
        t203->channels[i].rem_freq = 0.0;
        t203->channels[i].ref_chan_id[0] = '\0';
        t203->channels[i].rem_chan_id[0] = '\0';
        }
    }
