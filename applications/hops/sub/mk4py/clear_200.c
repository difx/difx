/************************************************************************/
/*                                                                      */
/* Initialize a type_200 structure                                      */
/*                                                                      */
/*      Inputs:         t200            To be initialized               */
/*                                                                      */
/*      Output:         t200            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_200.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_200 (struct type_200 *t200)
    {
    char version[3];
    int i;

    strncpy (t200->record_id, "200", 3);
    sprintf (version, "%02d", T200_VERSION);
    strncpy (t200->version_no, version, 2);
    strncpy (t200->unused1, "   ", 3);

    for (i=0; i<10; i++) t200->software_rev[i] = 0;
    t200->expt_no = 0;
    t200->exper_name[0] = '\0';
    t200->scan_name[0] = '\0';
    t200->correlator[0] = '\0';
    clear_date (&(t200->scantime));
    t200->start_offset = 0;
    t200->stop_offset = 0;
    clear_date (&(t200->corr_date));
    clear_date (&(t200->fourfit_date));
    clear_date (&(t200->frt));
    }
