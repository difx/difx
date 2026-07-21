/************************************************************************/
/*                                                                      */
/* Initialize a type_100 structure                                      */
/*                                                                      */
/*      Inputs:         t100            To be initialized               */
/*                                                                      */
/*      Output:         t100            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_100.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_100 (struct type_100 *t100)
    {
    char version[3];

    strncpy (t100->record_id, "100", 3);
    sprintf (version, "%02d", T100_VERSION);
    strncpy (t100->version_no, version, 2);
    strncpy (t100->unused1, "   ", 3);

    clear_date (&(t100->procdate));
    t100->baseline[0] = ' ';
    t100->baseline[1] = ' ';
    t100->rootname[0] = '\0';
    t100->qcode[0] = ' ';
    t100->qcode[1] = ' ';
    t100->pct_done = 0.0;
    clear_date (&(t100->start));
    clear_date (&(t100->stop));
    t100->ndrec = 0;
    t100->nindex = 0;
    t100->nlags = 0;
    t100->nblocks = 0;
    }
