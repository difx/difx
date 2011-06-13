/************************************************************************/
/*                                                                      */
/* Initialize a type_208 structure                                      */
/*                                                                      */
/*      Inputs:         t208            To be initialized               */
/*                                                                      */
/*      Output:         t208            Initialization complete         */
/*                                                                      */
/* Created September 25 1995 by CJL                                     */
/* Added version 1 support, Feb 14 2000 by CJL                          */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "type_208.h"
#include "mk4_dfio.h"

void
clear_208 (struct type_208 *t208)
    {
    char version[3];

    strncpy (t208->record_id, "208", 3);
    sprintf (version, "%02d", T208_VERSION);
    strncpy (t208->version_no, version, 2);
    strncpy (t208->unused1, "   ", 3);

    t208->quality = ' ';
    t208->errcode = ' ';
    strncpy (t208->tape_qcode, "000000", 6);
    t208->adelay = 0.0;
    t208->arate = 0.0;
    t208->aaccel = 0.0;
    t208->tot_mbd = 0.0;
    t208->tot_sbd = 0.0;
    t208->tot_rate = 0.0;
                                        /* next 3 lines added by rjc 2000.1.6 */
    t208->tot_mbd_ref = 0.0;
    t208->tot_sbd_ref = 0.0;
    t208->tot_rate_ref = 0.0;

    t208->resid_mbd = 0.0;
    t208->resid_sbd = 0.0;
    t208->resid_rate = 0.0;
    t208->mbd_error = 0.0;
    t208->sbd_error = 0.0;
    t208->rate_error = 0.0;
    t208->ambiguity = 0.0;
    t208->amplitude = 0.0;
    t208->inc_seg_ampl = 0.0;
    t208->inc_chan_ampl = 0.0;
    t208->snr = 0.0;
    t208->prob_false = 0.0;
    t208->totphase = 0.0;
    t208->totphase_ref = 0.0;
    t208->resphase = 0.0;
    }
