/************************************************************************/
/*                                                                      */
/* Initialize a type_303 structure                                      */
/*                                                                      */
/*      Inputs:         t303            To be initialized               */
/*                                                                      */
/*      Output:         t303            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/* Used for az, el, and pa          rjc   2012.2.21                     */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_303.h"
#include "mk4_dfio.h"

void
clear_303 (struct type_303 *t303)
    {
    int i;
    char version[3];

    strncpy (t303->record_id, "303", 3);
    sprintf (version, "%02d", T303_VERSION);
    strncpy (t303->version_no, version, 2);
    strncpy (t303->unused1, "   ", 3);

    t303->interval = -1;
    t303->chan_id[0] = '\0';
    for (i=0; i<6; i++) 
        {
        t303->azimuth[i] = 0.0;
        t303->elevation[i] = 0.0;
        t303->parallactic_angle[i] = 0.0;
        t303->u[i] = 0.0;
        t303->v[i] = 0.0;
        t303->w[i] = 0.0;
        }
    }
