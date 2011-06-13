/************************************************************************/
/*                                                                      */
/* Initialize a type_120 structure                                      */
/*                                                                      */
/*      Inputs:         t120            To be initialized               */
/*                                                                      */
/*      Output:         t120            Initialization complete         */
/*                                                                      */
/* Created December 18 1996 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_120.h"
#include "mk4_dfio.h"

void
clear_120 (struct type_120 *t120)
    {
    char version[3];

    strncpy (t120->record_id, "120", 3);
    sprintf (version, "%02d", T120_VERSION);
    strncpy (t120->version_no, version, 2);
    t120->type = 0;
    t120->nlags = 0;

    t120->baseline[0] = ' ';
    t120->baseline[1] = ' ';
    strcpy (t120->rootcode, "      ");
    t120->index = 0;
    t120->ap = 0;
    t120->flag = 0;
    t120->status = 0;
    t120->fr_delay = 0;
    t120->delay_rate = 0;
                                        /* No use initializing lag data since */
                                        /* we don't know how much there will be */
    }
