/************************************************************************/
/*                                                                      */
/* Trivial routine to initialize a pass structure                       */
/*                                                                      */
/* Created April 13 1992 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "control.h"
#include "param_struct.h"
#include "pass_struct.h"

void
clear_pass (pass)
struct type_pass *pass;
    {
    int i;

    for (i=0; i<MAXFREQ; i++) clear_freq_corel (pass->pass_data + i);
    pass->nfreq = 0;
    pass->channels = 0;
    pass->num_ap = 0;
    pass->ap_off = 0;
    pass->npctones = 0;
    pass->autocorr = FALSE;
    pass->start = 0.0;
    pass->stop = 0.0;
    pass->reftime = 0.0;
    nullify_cblock (&(pass->control));
    }
