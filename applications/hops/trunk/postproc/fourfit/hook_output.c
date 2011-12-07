/*
 * $Id$
 *
 * Routine to supply numbers when pc_mode is manual
 * regardless of whether a plot is being made.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"

extern struct mk4_fringe fringe;
extern struct type_param param;
extern struct type_status status;
extern struct type_plot plot;
extern double c_phase();

/* something for a library, later */
static char *pol_string(int pol)
{
    char *polstr;
    switch(pol) {
    case 0: polstr = "LL"; break;
    case 1: polstr = "RR"; break;
    case 2: polstr = "LR"; break;
    case 3: polstr = "RL"; break;
    default: polstr = "XX"; break;
    }
    return(polstr);
}

/*
 * This is lifted from make_postplot().  For each channel in the plot
 * "do the math".
 */
static void phases_feedback(struct type_pass *pass, int first, int final)
{
    int ch;
    double est_phase;

    fprintf (stderr, "pc_phases ");
    for (ch = first; ch <= final; ch++)
        fprintf (stderr, "%c", pass->pass_data[ch].freq_code);
    fprintf (stderr, " ");

    for (ch = first; ch <= final; ch++)
        {
        est_phase = (double)fringe.t207->ref_pcoffset[ch].lsb
                 - (double)fringe.t207->rem_pcoffset[ch].lsb
                 + c_phase (status.fringe[ch]) * 180.0 / M_PI
                 + 360.0 * fringe.t208->resid_mbd *
                    (pass->pass_data[ch].frequency - fringe.t205->ref_freq);
        est_phase = ( fmod(est_phase, 360.0) );
        if (est_phase < 0.0) est_phase += 360.0;
        fprintf (stderr, "%.1f ", est_phase);
        }
    fprintf (stderr, "\n");
}

/*
 * If samplers was used, average the sbd values by the sampler pools.
 */
static void delays_averaged(struct type_pass *pass, int first, int final,
                            double sbd[])
{
    static double ave[MAXFREQ];
    static int cnt[MAXFREQ], chs[MAXFREQ];
    int ch, nn, fc;

    fprintf (stderr, "delay_offs ");
    for (ch = first; ch <= final; ch++)
        {
        fprintf (stderr, "%c", fc = pass->pass_data[ch].freq_code);
        chs[ch] = -1;
        for (nn = 0; nn < pass->control.nsamplers; nn++)
            if (strchr(pass->control.psamplers[nn], fc))
                {
                ave[nn] += sbd[ch];
                cnt[nn] ++;
                chs[ch] = nn;
                break;
                }
        }
    fprintf (stderr, " ");

    for (nn = 0; nn < pass->control.nsamplers; nn++)
        ave[nn] /= (cnt[nn]) ? cnt[nn] : 1;

    for (ch = first; ch <= final; ch++)
        fprintf (stderr, "%.2f ", (chs[ch] >= 0) ? ave[chs[ch]] : 0.0);
        if (chs[ch] >= 0)
    fprintf (stderr, "\n");
}

/*
 * The sbd box is in lag space...  nl = param.nlags.
 */
static void delays_feedback(struct type_pass *pass, int first, int final)
{
    static double sbd[MAXFREQ];
    int ch;
    double est_delay, sbdbox;

    fprintf (stderr, "delay_offs ");
    for (ch = first; ch <= final; ch++)
        fprintf (stderr, "%c", pass->pass_data[ch].freq_code);
    fprintf (stderr, " ");

    for (ch = first; ch <= final; ch++)
        {
        /* Cf. status.sbdbox[MAXFREQ] <=> status.sbd_max */
        sbdbox = (status.sbdbox[ch] - param.nlags - 1) * status.sbd_sep;
        /* change sign for correction, times 1000 to ns */
        sbdbox *= - 1000.0;
        fprintf (stderr, "%.2f ", sbdbox);
        sbd[ch] = sbdbox;
        }
    fprintf (stderr, "\n");

    fprintf (stderr, "* samplers %d", pass->control.nsamplers);
    for (ch = 0; ch < pass->control.nsamplers; ch++)
        fprintf (stderr, " %s", pass->control.psamplers[ch]);
    fprintf (stderr, "\n");
    if (pass->control.nsamplers) delays_averaged(pass, first, final, sbd);
}

/*
 * Entry into more information
 */
static void manual_feedback(struct type_pass *pass)
{
    int first_ch, final_ch;
                                        /* bail if we are not MANUAL */
    if (param.pc_mode[0] != MANUAL || param.pc_mode[1] != MANUAL) return;

    first_ch = (param.first_plot == FALSE) ? 0 : param.first_plot;
    final_ch = (param.nplot_chans == FALSE) ? pass->nfreq : param.nplot_chans;
    final_ch += first_ch - 1;

    fprintf (stderr, "\n");
    fprintf(stderr, "Manual PC Feedback on %.8s - %.8s, "
        "fgroup %c, pol %s channels %c..%c:\n",
        fringe.t202->ref_name, fringe.t202->rem_name,
        pass->pass_data[0].fgroup, pol_string((int)param.pol),
        pass->pass_data[first_ch].freq_code,
        pass->pass_data[final_ch].freq_code);

    phases_feedback(pass, first_ch, final_ch);
    delays_feedback(pass, first_ch, final_ch);
}

#define OUTPUT_HOOK\
    manual_feedback(pass)
#include "output.c"

/*
 * eof
 */
