/************************************************************************/
/*                                                                      */
/* Initialize a type_221 structure                                      */
/*                                                                      */
/*      Inputs:         t221            To be initialized               */
/*                                                                      */
/*      Output:         t221            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "type_221.h"
#include "mk4_dfio.h"

void
clear_221 (struct type_221 *t221)
    {
    char version[3];

    strncpy (t221->record_id, "221", 3);
    sprintf (version, "%02d", T221_VERSION);
    strncpy (t221->version_no, version, 2);
    t221->unused1 = ' ';
    t221->padded = 0;

    t221->ps_length = 0;
    t221->pplot[0] = '\0';
    }
