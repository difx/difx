/************************************************************************/
/*                                                                      */
/* Initialize a type_222 structure                                      */
/*                                                                      */
/*      Inputs:         t222            To be initialized               */
/*                                                                      */
/*      Output:         t222            Initialization complete         */
/*                                                                      */
/* Created  April 3 2017 JPB                                            */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "type_222.h"
#include "mk4_dfio.h"

void
clear_222 (struct type_222 *t222)
    {
    char version[3];
    strncpy (t222->record_id, "222", 3);
    sprintf (version, "%02d", T222_VERSION);
    strncpy (t222->version_no, version, 2);
    t222->unused1 = ' ';
    t222->padded = 0;
    t222->setstring_hash = 0;
    t222->control_hash = 0;
    t222->setstring_length = 0;
    t222->cf_length = 0;
    t222->control_contents[0] = '\0';
    }
