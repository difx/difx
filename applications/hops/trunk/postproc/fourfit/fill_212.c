/************************************************************************/
/*                                                                      */
/* This fills in a type 212 record for 1 channel in the fringe data     */
/* structure.  The AP by AP output is simply the fringe phasor          */
/* calculated for the fringeplot in make_plotdata(), except that the    */
/* amplitudes need to be corrected                                      */
/*                                                                      */
/* Created April 19 2001 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <complex.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

int
fill_212 (
struct type_pass *pass,
struct type_status *status,
struct type_param *param,
int fr,
struct type_212 *t212)
    {
    int i, ap_212, nap, ap, nrec, aprec, phase, pcal1, pcal2, nrec_per_fr, nalloc;
    double factor;
    struct data_corel *datum;
    extern struct type_plot plot;

    clear_212 (t212);
    
    nap = pass->num_ap;
    t212->nap = nap;
    t212->first_ap = pass->ap_off;
    t212->channel = fr;
    t212->sbd_chan = status->max_delchan;
                                        /* Loop over the aps for this pass */
    for (ap = pass->ap_off; ap < pass->ap_off + nap; ap++)
        {
                                        /* Location in 212 array starts at 0 */
        ap_212 = ap - pass->ap_off;
                                        /* Ptr to element in main data array */
        datum = pass->pass_data[fr].data + ap;
                                        /* Data missing, put in -1 */
                                        /* Check on weights is insurance */
        if ((datum->flag == 0) || (plot.weights[fr][ap] == 0))
            {
            t212->data[ap_212].amp = -1.0;
            t212->data[ap_212].phase = 0.0;
            t212->data[ap_212].weight = 0.0;
            continue;
            }
                                        /* Amplitude and phase */
        t212->data[ap_212].amp = cabs (plot.phasor[fr][ap]) * status->amp_corr_fact;
        t212->data[ap_212].phase = carg (plot.phasor[fr][ap]);
        t212->data[ap_212].weight = plot.weights[fr][ap];
        }

    return (0);
    }
