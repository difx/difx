/************************************************************************/
/*									*/
/* Initialize a type_305 structure					*/
/*									*/
/*	Inputs:		t305		To be initialized		*/
/*									*/
/*	Output:		t305		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_305.h"
#include "mk4_dfio.h"

void
clear_305 (struct type_305 *t305)
    {
    char version[3];

    strncpy (t305->record_id, "305", 3);
    sprintf (version, "%02d", T305_VERSION);
    strncpy (t305->version_no, version, 2);
    strncpy (t305->unused1, "   ", 3);

/* INITIALIZE RECORD_SPECIFIC DATA HERE */

    }
