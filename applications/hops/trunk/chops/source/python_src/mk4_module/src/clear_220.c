/************************************************************************/
/*									*/
/* Initialize a type_220 structure					*/
/*									*/
/*	Inputs:		t220		To be initialized		*/
/*									*/
/*	Output:		t220		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "type_220.h"
#include "mk4_dfio.h"

void
clear_220 (struct type_220 *t220)
    {
    char version[3];

    strncpy (t220->record_id, "220", 3);
    sprintf (version, "%02d", T220_VERSION);
    strncpy (t220->version_no, version, 2);
    strncpy (t220->unused1, "   ", 3);

    t220->width = 0;
    t220->height = 0;
    t220->fplot = NULL;
    }
