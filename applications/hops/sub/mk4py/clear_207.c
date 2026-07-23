/************************************************************************/
/*                                                                      */
/* Initialize a type_207 structure                                      */
/*                                                                      */
/*      Inputs:         t207            To be initialized               */
/*                                                                      */
/*      Output:         t207            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_207.h"
#include "mk4_dfio.h"

void
clear_207 (struct type_207 *t207)
    {
    char version[3];
    int i;
    int nchan;

    nchan = (T207_VERSION == 2) ? 64 : 16;

    strncpy (t207->record_id, "207", 3);
    sprintf (version, "%02d", T207_VERSION);
    strncpy (t207->version_no, version, 2);
    strncpy (t207->unused1, "   ", 3);

    t207->pcal_mode = 0;

    for (i=0; i<nchan; i++)
        {
        t207->ref_pcamp[i].lsb = t207->ref_pcamp[i].usb = 0.0;
        t207->rem_pcamp[i].lsb = t207->rem_pcamp[i].usb = 0.0;
        t207->ref_pcphase[i].lsb = t207->ref_pcphase[i].usb = 0.0;
        t207->rem_pcphase[i].lsb = t207->rem_pcphase[i].usb = 0.0;
        t207->ref_pcoffset[i].lsb = t207->ref_pcoffset[i].usb = 0.0;
        t207->rem_pcoffset[i].lsb = t207->rem_pcoffset[i].usb = 0.0;
        t207->ref_pcfreq[i].lsb = t207->ref_pcfreq[i].usb = 0.0;
        t207->rem_pcfreq[i].lsb = t207->rem_pcfreq[i].usb = 0.0;
        t207->ref_errate[i] = 0.0;
        t207->rem_errate[i] = 0.0;
        }
    t207->ref_pcrate = 0.0;
    t207->rem_pcrate = 0.0;
    }
