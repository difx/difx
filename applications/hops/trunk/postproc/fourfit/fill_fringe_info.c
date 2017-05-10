/************************************************************************/
/*                                                                      */
/* Fills in the binary data records for the fringe output file.  Each   */
/* type 2** record is handled by a separate routine, where externs are  */
/* discouraged.  This allow one to see dependencies easily from this    */
/* routine.                                                             */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "vex.h"
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"


int
fill_fringe_info (
struct vex *root,
struct type_pass *pass,
char *filename)
    {
    static struct type_200 t200;
    static struct type_201 t201;
    static struct type_202 t202;
    static struct type_203 t203;
    static struct type_204 t204;
    static struct type_205 t205;
    static struct type_206 t206;
    static struct type_207 t207;
    static struct type_208 t208;
    static struct type_210 t210;
    static struct type_000 t2_id;
    double sband_err, ref_freq, sqrt(), fabs();
    int error, nap, xpow_len, fr, ap, size_of_t212, size_of_t230, recno;
    char buf[256];
    char *t212_array, *t230_array, *address;
    extern int write_xpower;
    extern struct mk4_fringe fringe;
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_plot plot;
                                        /* Init */
    clear_mk4fringe (&fringe);

    ref_freq = param.ref_freq;

    strcpy (buf, filename);
    if (init_000 (&t2_id, filename) != 0)
        {
        msg ("Error filling in id record", 2);
        return (-1);
        }

    error = fill_200 (root->ovex, &param, &t200);
    error += fill_201 (root->ovex, &param, &t201);
    error += fill_202 (root, &param, &t202);
    error += fill_203 (root->ovex, &param, &t203);
    error += fill_204 (&t204);
    error += fill_205 (root->ovex, pass, &param, &t203, &t205);
    error += fill_206 (root->ovex, pass, &param, &status, &t206);
    error += fill_207 (pass, &status, &param, &t207);
    error += fill_208 (pass, &param, &status, &t202, &t208);
    error += fill_210 (pass, &status, &t210);

    fringe.id = &t2_id;
    fringe.t200 = &t200;
    fringe.t201 = &t201;
    fringe.t202 = &t202;
    fringe.t203 = &t203;
    fringe.t204 = &t204;
    fringe.t205 = &t205;
    fringe.t206 = &t206;
    fringe.t207 = &t207;
    fringe.t208 = &t208;
    fringe.t210 = &t210;
                                        /* Type 212 (ap-by-ap data) records */
                                        /* Allocate memory as a block */
    nap = pass->num_ap;
    size_of_t212 = sizeof (struct type_212) + 12*(nap-1);
    if ((nap % 2) == 1) size_of_t212 += 12;
    t212_array = (char *)malloc (pass->nfreq * size_of_t212);
    if (t212_array == NULL)
        {
        msg ("Failure allocating memory for type 212 records!", 2);
        return (0);
        }
                                        /* record the allocation */
    fringe.allocated[fringe.nalloc] = t212_array;
    fringe.nalloc += 1;
                                        /* Fill in records and pointers */
    fringe.n212 = pass->nfreq;
    for (fr=0; fr<pass->nfreq; fr++)
        {
        address = t212_array + (fr * size_of_t212);
        fringe.t212[fr] = (struct type_212 *)address;
        error += fill_212 (pass, &status, &param, fr, fringe.t212[fr]);
        }
                                        /* Cross power spectra (if requested) */
    if (write_xpower)
        {
                                        /* Allocate memory as a block */
        xpow_len = 16 * 2 * param.nlags;
        size_of_t230 = sizeof (struct type_230) - sizeof (complex) + xpow_len;
        t230_array = (char *)malloc (pass->nfreq * nap * size_of_t230);
        if (t230_array == NULL)
            {
            msg ("Failure allocating memory for type 230 records!", 2);
            return (0);
            }
                                        /* record the allocation */
        fringe.allocated[fringe.nalloc] = t230_array;
        fringe.nalloc += 1;
                                        /* Loop over all freqs, aps */
        recno = 0;
        for (fr=0; fr<pass->nfreq; fr++)
            for (ap = pass->ap_off; ap < pass->ap_off + nap; ap++)
                {
                address = t230_array + recno * size_of_t230;
                fringe.t230[recno] = (struct type_230 *)address;
                error += fill_230 (pass, &param, fr, ap, fringe.t230[recno]);
                recno++;
                }
        fringe.n230 = recno;
        }

    if (error != 0)
        msg ("Warning - some or all of the output records were not filled", 2);
    
    status.amp_err = status.delres_max / status.snr;
    status.resid_phase = status.coh_avg_phase * ( 180.0 / M_PI);
    status.mod_resid_phase *= 180.0 / M_PI;
    sband_err = sqrt (1.0 + 3.0 * (status.sbavg * status.sbavg));
    status.phase_err = (status.nion == 0) ?
        180.0 * sband_err / (M_PI * status.snr) :
        360.0 * status.ion_sigmas[1];
    status.resid_ph_delay = status.coh_avg_phase / (2.0 * M_PI *ref_freq);
    status.ph_delay_err = sband_err / (2.0 * M_PI * status.snr * ref_freq);

/*     status.rate_ra_width = fabs(0.5 * (param.win_dr[1] - param.win_dr[0])  */
/*       * 1.0E6 / (rbase->t2600.u_rate)); */
/*     status.rate_dec_width = fabs(0.5 * (param.win_dr[1] - param.win_dr[0])  */
/*       * 1.0E6 / (rbase->t2600.v_rate)); */
/*     status.sbd_ra_width = fabs(0.5 * (param.win_sb[1] - param.win_sb[0])  */
/*        / (rbase->t2600.u_1ghz * 1.0E-3)); */
/*     status.sbd_dec_width = fabs(0.5 * (param.win_sb[1] - param.win_sb[0])  */
/*       / (rbase->t2600.v_1ghz * 1.0E-3)); */

    return (0);
    }   
