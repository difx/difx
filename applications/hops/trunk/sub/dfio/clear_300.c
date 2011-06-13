/************************************************************************/
/*                                                                      */
/* Initialize a type_300 structure                                      */
/*                                                                      */
/*      Inputs:         t300            To be initialized               */
/*                                                                      */
/*      Output:         t300            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_300.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_300 (struct type_300 *t300)
    {
    char version[3];

    strncpy (t300->record_id, "300", 3);
    sprintf (version, "%02d", T300_VERSION);
    strncpy (t300->version_no, version, 2);
    strncpy (t300->unused1, "   ", 2);

    t300->SU_number = 0;
    t300->id = ' ';
    t300->intl_id[0] = ' ';
    t300->intl_id[1] = ' ';
    t300->name[0] = '\0';
    clear_date (&(t300->model_start));
    t300->model_interval = 0.0;
    t300->nsplines = 0;
    }
