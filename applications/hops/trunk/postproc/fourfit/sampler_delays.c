// sampler_delays adjusts the delay offsets for each pool
// of channels sharing a sampler to have the same, mean value
//
// Initial code                                        rjc  2011.10.24

#include <stdio.h>
#include <string.h>
#include "pass_struct.h"
#include "param_struct.h"
#include "control.h"
#include "mk4_data.h"


void sampler_delays (struct type_pass *pass)
    {
    int n,
        k,
        fno,
        ap,
        stnpol[2][4] = {0, 1, 0, 1, 0, 1, 1, 0}; // [stn][pol] = 0:L/X/H, 1:R/Y/V

    char *pool;

    double avg_ref,
           avg_rem;

    struct freq_corel *fdata;
    struct data_corel *datum;
    extern struct type_param param;

    for (n=0; n<pass->control.nsamplers; n++)
	    msg ("Sampler group %d: %s", 1, n, pass->control.psamplers[n]);

                                    // loop over all ap's
    for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)
        {
                                    // loop over all of the sampler pools
        for (n=0; n<pass->control.nsamplers; n++)
            {
            pool = pass->control.psamplers[n];
            avg_ref = 0.0;
            avg_rem = 0.0;
                                    // find average delay offset for this pool
            for (k=0; k<strlen(pool); k++)
                {
                fno = fcode(pool[k]);
                fdata = pass->pass_data + fno;
                datum = fdata->data + ap;
//		fprintf(stderr, "find  %c -> %d %s\n",
//		    pool[k], fno, fdata->data_alloc?"alloc":"ERROR");

                avg_ref += datum->ref_sdata.mt_delay[stnpol[0][param.pol]];
                avg_rem += datum->rem_sdata.mt_delay[stnpol[1][param.pol]];
                }
            avg_ref /= k;
            avg_rem /= k;
                                    // apply average to all members of pool
            for (k=0; k<strlen(pool); k++)
                {
                fno = fcode(pool[k]);
                fdata = pass->pass_data + fno;
                datum = fdata->data + ap;

                datum->ref_sdata.mt_delay[stnpol[0][param.pol]] = avg_ref;
                datum->rem_sdata.mt_delay[stnpol[1][param.pol]] = avg_rem;
                }
            }
        }
    }
