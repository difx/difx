/************************************************************************/
/*                                                                      */
/* Initialize a type_202 structure                                      */
/*                                                                      */
/*      Inputs:         t202            To be initialized               */
/*                                                                      */
/*      Output:         t202            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_202.h"
#include "mk4_dfio.h"

void
clear_202 (struct type_202 *t202)
    {
    char version[3];

    strncpy (t202->record_id, "202", 3);
    sprintf (version, "%02d", T202_VERSION);
    strncpy (t202->version_no, version, 2);
    strncpy (t202->unused1, "   ", 3);

    t202->baseline[0] = '\0';
    t202->ref_intl_id[0] = '\0';
    t202->rem_intl_id[0] = '\0';
    t202->ref_name[0] = '\0';
    t202->rem_name[0] = '\0';
    t202->ref_tape[0] = '\0';
    t202->rem_tape[0] = '\0';
    t202->nlags = 0;
    t202->ref_xpos = 0.0;
    t202->rem_xpos = 0.0;
    t202->ref_ypos = 0.0;
    t202->rem_ypos = 0.0;
    t202->ref_zpos = 0.0;
    t202->rem_zpos = 0.0;
    t202->u = 0.0;
    t202->v = 0.0;
    t202->uf = 0.0;
    t202->vf = 0.0;
    t202->ref_clock = 0.0;
    t202->rem_clock = 0.0;
    t202->ref_clockrate = 0.0;
    t202->rem_clockrate = 0.0;
    t202->ref_idelay = 0.0;
    t202->rem_idelay = 0.0;
    t202->ref_zdelay = 0.0;
    t202->rem_zdelay = 0.0;
    t202->ref_elev = 0.0;
    t202->rem_elev = 0.0;
    t202->ref_az = 0.0;
    t202->rem_az = 0.0;
    }
