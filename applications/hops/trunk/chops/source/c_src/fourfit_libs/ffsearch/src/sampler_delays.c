// sampler_delays adjusts the delay offsets for each pool
// of channels sharing a sampler to have the same, mean value
//
// Initial code                                        rjc  2011.10.24
// do independent averaging for each polarization      rjc  2012.3.23

#include <stdio.h>
#include <string.h>
#include "pass_struct.h"
#include "param_struct.h"
#include "control.h"
#include "mk4_data.h"

static int sampler_fcode (struct freq_corel *fdata,
                          int nfreq,
                          int sc)
    {
    int fno;
    for (fno = 0; fno < nfreq; fno++, fdata++)
        if (fdata->freq_code == sc) 
            return (fno);
    return (-1);
    }

void sampler_delays (struct type_pass *pass)
    {
    int n,
        k,
        ksum,
        fno,
        ap,
        stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}, // [stn][pol] = 0:L/X/H, 1:R/Y/V
        ip;

    char *pool;

    double avg_ref,
           avg_rem;

    struct freq_corel *fdata;
    struct data_corel *datum;
    extern struct type_param param;
                                    // debug print sampler group
    for (n=0; n<pass->control.nsamplers; n++)
	    msg ("Sampler group %d: %s", 0, n, pass->control.psamplers[n]);

                                    // loop over all ap's
    for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)
                                    // loop over both polarizations
      for (ip=0; ip<2; ip++)
                                    // loop over all of the sampler pools
        for (n=0; n<pass->control.nsamplers; n++)
            {
            pool = pass->control.psamplers[n];
            avg_ref = 0.0;
            avg_rem = 0.0;
                                    // find average delay offset for this pool
            ksum = 0;
            for (k=0; k<strlen(pool); k++)
                {
                fno = sampler_fcode (pass->pass_data, pass->nfreq, pool[k]);
                if (fno >= 0)
                    {
                    fdata = pass->pass_data + fno;
                    datum = fdata->data + ap;

                    avg_ref += datum->ref_sdata.mt_delay[ip];
                    avg_rem += datum->rem_sdata.mt_delay[ip];
                    ksum++;
                    }
                }
            if (ksum > 0)
                {
                avg_ref /= ksum;
                avg_rem /= ksum;
                }
                                    // apply average to all members of pool
            for (k=0; k<strlen(pool); k++)
                {
                fno = sampler_fcode (pass->pass_data, pass->nfreq, pool[k]);
                if (fno >= 0)
                    {
                    fdata = pass->pass_data + fno;
                    datum = fdata->data + ap;

                    datum->ref_sdata.mt_delay[ip] = avg_ref;
                    datum->rem_sdata.mt_delay[ip] = avg_rem;
                    }
                }
            }
    }
