/************************************************************************/
/*                                                                      */
/* Initialize a type_206 structure                                      */
/*                                                                      */
/*      Inputs:         t206            To be initialized               */
/*                                                                      */
/*      Output:         t206            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/* redimensioned to 64 channels        rjc    2010.1.5                  */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_206.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_206 (struct type_206 *t206)
    {
    char version[3];
    int i;
    int nchan;

    nchan = (T206_VERSION == 2) ? 64 : 16;

    strncpy (t206->record_id, "206", 3);
    sprintf (version, "%02d", T206_VERSION);
    strncpy (t206->version_no, version, 2);
    strncpy (t206->unused1, "   ", 3);

    clear_date (&(t206->start));
    t206->first_ap = 0;
    t206->last_ap = 0;
    for (i=0; i<nchan; i++) 
        {
        t206->accepted[i].usb = t206->accepted[i].lsb = 0;
        t206->weights[i].usb = t206->weights[i].lsb = 0.0;
        }
    t206->intg_time = 0.0;
    t206->accept_ratio = 0.0;
    t206->discard = 0.0;
    for (i=0; i<nchan; i++)
        {
        t206->reason1[i].lsb = 0;
        t206->reason1[i].usb = 0;
        t206->reason2[i].lsb = 0;
        t206->reason2[i].usb = 0;
        t206->reason3[i].lsb = 0;
        t206->reason3[i].usb = 0;
        t206->reason4[i].lsb = 0;
        t206->reason4[i].usb = 0;
        t206->reason5[i].lsb = 0;
        t206->reason5[i].usb = 0;
        t206->reason6[i].lsb = 0;
        t206->reason6[i].usb = 0;
        t206->reason7[i].lsb = 0;
        t206->reason7[i].usb = 0;
        t206->reason8[i].lsb = 0;
        t206->reason8[i].usb = 0;
        }
    t206->ratesize = 0;
    t206->mbdsize = 0;
    t206->sbdsize = 0;
    }
