/************************************************************************/
/*									*/
/* Initialize a type_301 structure					*/
/*									*/
/*	Inputs:		t301		To be initialized		*/
/*									*/
/*	Output:		t301		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_301.h"
#include "mk4_dfio.h"

void
clear_301 (struct type_301 *t301)
    {
    int i;
    char version[3];

    strncpy (t301->record_id, "301", 3);
    sprintf (version, "%02d", T301_VERSION);
    strncpy (t301->version_no, version, 2);
    strncpy (t301->unused1, "   ", 3);

    t301->interval = -1;
    t301->chan_id[0] = '\0';
    for (i=0; i<6; i++) t301->delay_spline[i] = 0.0;
    }
