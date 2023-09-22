/************************************************************************/
/*                                                                      */
/* Initialize a type_204 structure                                      */
/*                                                                      */
/*      Inputs:         t204            To be initialized               */
/*                                                                      */
/*      Output:         t204            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_204.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_204 (struct type_204 *t204)
    {
    char version[3];

    strncpy (t204->record_id, "204", 3);
    sprintf (version, "%02d", T204_VERSION);
    strncpy (t204->version_no, version, 2);
    strncpy (t204->unused1, "   ", 3);

    t204->ff_version[0] = '\0';
    t204->platform[0] = '\0';
    t204->control_file[0] = '\0';
    clear_date (&(t204->ffcf_date));
    t204->override[0] = '\0';
    }
