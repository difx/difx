/************************************************************************/
/*                                                                      */
/* Initialize a type_309 structure                                      */
/*                                                                      */
/*      Inputs:         t309            To be initialized               */
/*                                                                      */
/*      Output:         t309            Initialization complete         */
/*                                                                      */
/* Created by modifying cjl's clear_307         rjc  2006.2.6           */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_309.h"
#include "mk4_dfio.h"

void
clear_309 (struct type_309 *t309)
    {
    int i, j, k;
    char version[3];
    int nchan;

    nchan = (T309_VERSION == 1) ? 64 : 16;

    strncpy (t309->record_id, "309", 3);
    sprintf (version, "%02d", T309_VERSION);
    strncpy (t309->version_no, version, 2);
    strncpy (t309->unused1, "   ", 3);

    t309->su = -1;
    t309->ntones = -1;
    
    t309->rot = 0.0;
    t309->acc_period = 0.0;

    for (i=0; i<nchan; i++)
        {
        t309->chan[i].chan_name[0] = '\0';
        t309->chan[i].freq = 0.0;
        
        for (j=0; j<nchan; j++) 
            for (k=0; k<2; k++)
                t309->chan[i].acc[j][k] = 0;
        }

    }
