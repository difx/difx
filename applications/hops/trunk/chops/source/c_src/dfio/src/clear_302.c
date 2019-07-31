/************************************************************************/
/*									*/
/* Initialize a type_302 structure					*/
/*									*/
/*	Inputs:		t302		To be initialized		*/
/*									*/
/*	Output:		t302		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_302.h"
#include "mk4_dfio.h"

void
clear_302 (struct type_302 *t302)
    {
    int i;
    char version[3];

    strncpy (t302->record_id, "302", 3);
    sprintf (version, "%02d", T302_VERSION);
    strncpy (t302->version_no, version, 2);
    strncpy (t302->unused1, "   ", 3);

    t302->interval = -1;
    t302->chan_id[0] = '\0';
    for (i=0; i<6; i++) t302->phase_spline[i] = 0.0;
    }
