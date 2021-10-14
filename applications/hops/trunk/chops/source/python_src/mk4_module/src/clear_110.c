/************************************************************************/
/*									*/
/* Initialize a type_110 structure					*/
/*									*/
/*	Inputs:		t110		To be initialized		*/
/*									*/
/*	Output:		t110		Initialization complete		*/
/*									*/
/* Created December 18 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include "type_110.h"
#include "mk4_dfio.h"

void
clear_110 (struct type_110 *t110)
    {
    int i;
    char version[3];

    strncpy (t110->record_id, "110", 3);
    sprintf (version, "%02d", T110_VERSION);
    strncpy (t110->version_no, version, 2);
    t110->unused1 = ' ';
    t110->nblocks = 0;

    strncpy (t110->unused2, "  ", 2);
    t110->baseline[0] = ' ';
    t110->baseline[1] = ' ';
    t110->filenum = 0;
    strcpy (t110->rootcode, "      ");
    t110->index = 0;
    t110->ap = 0;
    t110->flag = 0;
    t110->status = 0;
    t110->bitshift = 0.0;
    t110->fbit = 0.0;
    t110->nblocks = 0;
					/* No use initializing raw data since */
					/* we don't know how much there will be */
    }
