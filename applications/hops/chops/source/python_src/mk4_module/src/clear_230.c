/************************************************************************/
/*                                                                      */
/* Initialize a type_230 structure                                      */
/*                                                                      */
/*      Inputs:         t230            To be initialized               */
/*                                                                      */
/*      Output:         t230            Initialization complete         */
/*                                                                      */
/* Created October 21 2000 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_230.h"
#include "mk4_dfio.h"

void
clear_230 (struct type_230 *t230)
    {
    char version[3];

    strncpy (t230->record_id, "230", 3);
    sprintf (version, "%02d", T230_VERSION);
    strncpy (t230->version_no, version, 2);
    t230->nspec_pts = 0;

/*     strcpy (t230->rootcode, "      "); */
    t230->frq = 0;
    t230->ap = 0;
    t230->usbweight = 0.0;
    t230->lsbweight = 0.0;
                                        /* No use initializing xpower since */
                                        /* we don't know how big array will be */
    }
