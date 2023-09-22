/************************************************************************/
/*                                                                      */
/* Initialize a type_212 structure                                      */
/*                                                                      */
/*      Inputs:         t212            To be initialized               */
/*                                                                      */
/*      Output:         t212            Initialization complete         */
/*                                                                      */
/* Created April 19 2001 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_212.h"
#include "mk4_dfio.h"

void
clear_212 (struct type_212 *t212)
    {
    char version[3];

    strncpy (t212->record_id, "212", 3);
    sprintf (version, "%02d", T212_VERSION);
    strncpy (t212->version_no, version, 2);
    t212->nap = 0;

    t212->channel = -1;
    t212->sbd_chan = -1;
                                        /* Data element variable length */
    }
