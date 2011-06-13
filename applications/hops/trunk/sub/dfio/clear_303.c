/************************************************************************/
/*									*/
/* Initialize a type_303 structure					*/
/*									*/
/*	Inputs:		t303		To be initialized		*/
/*									*/
/*	Output:		t303		Initialization complete		*/
/*									*/
/* Created September 25 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_303.h"
#include "mk4_dfio.h"

void
clear_303 (struct type_303 *t303)
    {
    char version[3];

    strncpy (t303->record_id, "303", 3);
    sprintf (version, "%02d", T303_VERSION);
    strncpy (t303->version_no, version, 2);
    strncpy (t303->unused1, "   ", 3);

/* INITIALIZE RECORD_SPECIFIC DATA HERE */

    }
