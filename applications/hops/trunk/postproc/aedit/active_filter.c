/************************************************************************/
/*                                                                      */
/* This routine is a partner to ?filter.c, used for determining if a    */
/* data point passes the filter settings in the inputs.  It is the      */
/* responsibility of this routine to fill up the external "fcheck"      */
/* array identifying those filters which are not in their pass-all      */
/* state.  The actual filtering can then be done more efficiently, by   */
/* only looking at active filters.                                      */
/*                                                                      */
/*      Inputs:         inp structure   Filter settings by user         */
/*                                                                      */
/*      Output:         fcheck          Indices of active filters       */
/*                      nfilt           Number of active filters        */
/*                                                                      */
/* Created 30 April 1990 by CJL                                         */
/* Gradually expanded over versions, latest on 29 August 1994 by CJL    */
/*                                                                      */
/************************************************************************/

#include <string.h>
#include "aedit.h"
#include "flags.h"

int fcheck[17], nfilt;

int
active_filter()
    {
    extern struct inputs inp;

    nfilt = 0;                  /* Find out which filters need checking */
    if (inp.begin != 0 || inp.end != 0) fcheck[nfilt++] = F_TIMETAG;
    if (inp.proc_begin != 0 || inp.proc_end != 0) fcheck[nfilt++] = F_PROCDATE;
    if (strlen (inp.stations) != 0) fcheck[nfilt++] = F_STATION;
    if (strlen (inp.baselines) != 0) fcheck[nfilt++] = F_BASELINE;
    if (strlen (inp.triangles) != 0) fcheck[nfilt++] = F_TRIANGLE;
    if (strlen (inp.quads) != 0) fcheck[nfilt++] = F_QUAD;
    if (strlen (inp.frequencies) != 0) fcheck[nfilt++] = F_FREQUENCY;
    if (strlen (inp.polarizations) != 0) fcheck[nfilt++] = F_POLARIZATION;
    if (inp.experiment != 0) fcheck[nfilt++] = F_EXPERIMENT;
    if (strlen (inp.qcodes) != 0) fcheck[nfilt++] = F_QCODE;
    if (strlen (inp.sources) != 0) fcheck[nfilt++] = F_SOURCE;
    if (strcmp (inp.type, "0,1,2,3,4") != 0) fcheck[nfilt++] = F_TYPE;
    if ((inp.snr[0] != 0.0) || (inp.snr[1] < 100000.0)) fcheck[nfilt++] = F_SNR;
    if ((inp.bsnr[0] != 0.0) || (inp.bsnr[1] < 1000000.0)) fcheck[nfilt++] = F_BSNR;
    if (inp.length != 0) fcheck[nfilt++] = F_LENGTH;
    if (inp.fraction != 0) fcheck[nfilt++] = F_FRACTION;
    if ((inp.nfreq[0] != 1) || (inp.nfreq[1] != MAXFREQ)) fcheck[nfilt++] = F_NFREQ;
    if (inp.parameter[0] > .01) fcheck[nfilt++] = F_PARAMETER;
    }
