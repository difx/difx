/************************************************************************/
/*									*/
/* Initialize a type_306 structure					*/
/*									*/
/*	Inputs:		t306		To be initialized		*/
/*									*/
/*	Output:		t306		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_306.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_306 (struct type_306 *t306)
    {
    int i;
    char version[3];

    strncpy (t306->record_id, "306", 3);
    sprintf (version, "%02d", T306_VERSION);
    strncpy (t306->version_no, version, 2);
    strncpy (t306->unused1, "   ", 3);

    clear_date (&(t306->time));
    t306->duration = 0.0;
    for (i=0; i<16; i++)
	{
	t306->stcount[i].chan_id[0] = '\0';
	t306->stcount[i].bigpos = 0;
	t306->stcount[i].pos = 0;
	t306->stcount[i].neg = 0;
	t306->stcount[i].bigneg = 0;
	}
    }
