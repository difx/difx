/************************************************************************/
/*                                                                      */
/* Initialize a type_308 structure                                      */
/*                                                                      */
/*      Inputs:         t308            To be initialized               */
/*                                                                      */
/*      Output:         t308            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_308.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_308 (struct type_308 *t308)
    {
    int i;
    char version[3];

    strncpy (t308->record_id, "308", 3);
    sprintf (version, "%02d", T308_VERSION);
    strncpy (t308->version_no, version, 2);
    strncpy (t308->unused1, "   ", 3);

    clear_date (&(t308->time));
    t308->duration = 0.0;
    for (i=0; i<32; i++)
        {
        t308->pcal[i].chan_id[0] = '\0';
        t308->pcal[i].real = 0.0;
        t308->pcal[i].imaginary = 0.0;
        t308->pcal[i].frequency = 0.0;
        }
    }
