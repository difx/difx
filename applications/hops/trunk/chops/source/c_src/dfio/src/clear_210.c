/************************************************************************/
/*                                                                      */
/* Initialize a type_210 structure                                      */
/*                                                                      */
/*      Inputs:         t210            To be initialized               */
/*                                                                      */
/*      Output:         t210            Initialization complete         */
/*                                                                      */
/* Created March 8 2000 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_210.h"
#include "mk4_dfio.h"

void
clear_210 (struct type_210 *t210)
    {
    char version[3];
    int i;
    int nchan;

    nchan = (T210_VERSION == 1) ? 64 : 16;

    strncpy (t210->record_id, "210", 3);
    sprintf (version, "%02d", T210_VERSION);
    strncpy (t210->version_no, version, 2);
    strncpy (t210->unused1, "   ", 3);

    for (i=0; i<nchan; i++)
        {
        t210->amp_phas[i].ampl = 0.0;
        t210->amp_phas[i].phase = 0.0;
        }
    }
