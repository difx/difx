/************************************************************************/
/*                                                                      */
/* This routine takes a filled in array of codata structures, and plots */
/* amplitude and snr data, with fits, as a function of segmentation     */
/* time for each baseline.  It uses the PGPLOT subroutine library, and  */
/* sends the output to the device specified in the original cpgbeg call */
/* The plot device is assumed to be open already.                       */
/*                                                                      */
/*      Inputs:         codata          struct array filled with data   */
/*      Output:         graphics        using PGPLOT hardcoded routines */
/*                                                                      */
/* Created October 5 1995 by CJL                                        */
/* Hacked up Apr 26 2026 by GBC                                         */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_util.h"
#include "cpgplot.h"
#include "cohfit.h"

void plot_codata (cosumary codata[])
    {
    int i, base, nbase, npt, year, day, hour, min, sec, wid;
    int nxsub, nysub;
    float amplmax, snrmax, amperr, cht = 1.5, lht = cht / 2.0, tht = 1.1*cht;
    float snrhigh[MAX_NSEGLEN], snrlow[MAX_NSEGLEN];
    float amphigh[MAX_NSEGLEN], amplow[MAX_NSEGLEN];
    float log_seglen[MAX_NSEGLEN], log_segmin, log_segmax;
    float log_bpoint, log_ncotime, log_scotime;
    float fit_seglen[3], fitampl[3];
    char label[MAX_TXT], bad_fit[MAX_TXT], bs_pol[MAX_TXT];
    cosumary *codatum;
                                        /* How many baselines are there? */
    nbase = 0;
    while (codata[nbase].datum != NULL) nbase++;
                                        /* Loop over all baselines in codata */
    for (base=0; base<nbase; base++)
        {
                                        /* How many points are we plotting? */
        codatum = codata + base;
        npt = codatum->nsegtime;
                                        /* Establish error bars and extrema */
        amplmax = snrmax = 0.0;
        for (i=0; i<npt; i++)
            {
            snrhigh[i] = codatum->snr[i] + 1.0;
            snrlow[i] = codatum->snr[i] - 1.0;
            if (snrlow[i] < 0.0) snrlow[i] = 0.0;
            if (codatum->snr[i] > 0.0) 
                amperr = codatum->ampl[i] / codatum->snr[i];
            else amperr = 0;
            amphigh[i] = codatum->ampl[i] + amperr;
            amplow[i] = codatum->ampl[i] - amperr;
            if (amplow[i] < 0.0) amplow[i] = 0.0;
            if (snrhigh[i] > snrmax) snrmax = snrhigh[i];
            if (codatum->didfits & FITOPT_SNR_3PT &&
                codatum->fitsnr[i] > snrmax) snrmax = codatum->fitsnr[i];
#if HAVEGSL2P8
            if (codatum->didfits & FITOPT_NDX_2P8 &&
                codatum->fit2p8[i] > snrmax) snrmax = codatum->fit2p8[i];
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
            if (codatum->didfits & FITOPT_NDX_2P7 &&
                codatum->fit2p7[i] > snrmax) snrmax = codatum->fit2p7[i];
#endif /* HAVEGSL2P7 */
            if (snrmax <= 0.0) snrmax = 3.0;
            if (amphigh[i] > amplmax) amplmax = amphigh[i];
            }
        if (codatum->plateau > amplmax) amplmax = codatum->plateau;

        for (i=0; i<npt; i++)
            log_seglen[i] = log10 ((double)codatum->seglen[i]);
        log_segmin = log_seglen[0];
        log_segmax = log_seglen[npt-1];
                                        /* Move to next page/panel */
        cpgpage();

        cpgsci (1);                     /* SNR box, axes, &c in black */
        cpgsch (cht);                   /* Set main character height */
        cpgsvp (0.15, 0.85, 0.1, 0.45);
        cpgswin (log_segmin-0.3, log_segmax+0.3, 0.0, snrmax*1.1);
        cpgbox ("BCLNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
        cpgmtxt ("B", 2, 0.5, 0.5, "Segmentation time (sec)");
        cpgmtxt ("L", 1.7, 0.5, 0.5, "SNR");

        if (codatum->didfits & FITOPT_SNR_3PT) {
            cpgsci (6);                 /* PLOT the 3PT SNR fit in magenta */
            cpgline (npt, log_seglen, codatum->fitsnr);
        }
#if HAVEGSL2P8
        if (codatum->didfits & FITOPT_SNR_2P8) {
            cpgsci (12);                /* PLOT the CBS2.8 SNR fit in purple */
            cpgline (npt, log_seglen, codatum->fit2p8);
        }
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
        if (codatum->didfits & FITOPT_SNR_2P7) {
            cpgsci (14);                /* PLOT the CBS2.7 SNR fit dark gray */
            cpgline (npt, log_seglen, codatum->fit2p7);
        }
#endif /* HAVEGSL2P7 */

        cpgsci (11);                    /* PLOT the SNR data in lt. blue */
        cpgpt (npt, log_seglen, codatum->snr, 3);               /*symbols*/
        cpgerry (npt, log_seglen, snrlow, snrhigh, 0.5);        /*errbars*/

        if (codatum->datum->srch_cotime > 0)
            {
            cpgsci (2);                     /* Draw 3-pt fit in red */
            cpgline (3, &log_seglen[codatum->snr_peak[0]],
                &codatum->snr[codatum->snr_peak[0]]);
            log_scotime = log10 ((double)(codatum->orig_srch));
            if (log_scotime > (log_segmax + 0.3)) log_scotime = log_segmax+0.3;
                                        /* Orig srch_cotime */
            cpgsci (8); cpgqlw (&wid); cpgslw (3 * wid); cpgsls(2);
            cpgmove (log_scotime, 0.0);
            cpgdraw (log_scotime, snrmax*1.1);
            cpgslw (wid); cpgsls(1);
            if (codatum->labels)
                {
                cpgsch (lht);           /* Set label character height */
                cpgmtxt("LV", -3.0, 0.15, 0.0, "(orig)");
                }
                                        /* Cotime is gold color, bold */
            log_scotime = log10 ((double)(codatum->datum->srch_cotime));
            if (log_scotime > (log_segmax + 0.3)) log_scotime = log_segmax+0.3;
            cpgsci (8); cpgqlw (&wid); cpgslw (5 * wid);
            cpgmove (log_scotime, 0.0);
            cpgdraw (log_scotime, snrmax*1.1);
            cpgslw (wid);
            if (codatum->labels)
                {
                cpgsch (lht);           /* Set label character height */
                cpgmtxt("RV", -3.0, 0.15, 1.0, "srch");
                }
            }
        else if (codatum->labels)
            {
            cpgsci (2);
            cpgqlw(&wid);cpgslw(2*wid);
            if (codatum->datum->srch_cotime == -1.0)
                snprintf(bad_fit, MAX_TXT-2, "Bad Fit (SNR)");
            else if (codatum->datum->srch_cotime == -2.0)
                snprintf(bad_fit, MAX_TXT-2, "Never fit SNR");
            else
                snprintf(bad_fit, MAX_TXT-2, "garbage SNR peak");
            cpgsch (lht);               /* Set label character height */
            cpgmtxt ("B", -1.5, 0.15, 0.0, bad_fit);
            cpgslw (wid);
            }

        cpgsci (1);                     /* Amplitude box, axes, &c in black */
        cpgsch (cht);                   /* Set main character height */
        cpgsvp (0.15, 0.85, 0.45, 0.8);
        cpgswin (log_segmin-0.3, log_segmax+0.3, 0.0, amplmax*1.1);
        cpgbox ("BCLST", 0.0, 0.0, "BCMST", 0.0, 0.0);
        int_to_time (codatum->datum->time_tag, &year, &day, &hour, &min, &sec);
                                        /* Top box gets the title */
        cpgsch (tht);                   /* Set title character height */
        snprintf(label, MAX_TXT, "%03d-%02d%02d%02d/%s.%c.%d.%s %2s",
                day, hour, min, sec,
                codatum->datum->baseline, codatum->datum->freq_code,
                codatum->datum->extent_no, codatum->datum->root_id,
                codatum->datum->polarization);
        cpgmtxt ("T", 0.5, 0.5, 0.5, label);
        cpgmtxt ("R", 2.2, 0.5, 0.5, "Amplitude");

        log_bpoint = log10 ((double)(codatum->breakpoint));
        fitampl[0] = fitampl[1] = codatum->plateau;
        fitampl[2] = codatum->plateau +
                     codatum->slope*(log_segmax - log_bpoint);
        fit_seglen[0] = log_segmin;
        fit_seglen[1] = log_bpoint;
        fit_seglen[2] = log_segmax;


        if (codatum->didfits & FITOPT_AMP_PS) {
            if (codatum->bestamp & FITOPT_AMP_PS) {cpgqlw(&wid);cpgslw(5*wid);}
            cpgsci(6);                 /* PLOT Amp PS in magenta */
            cpgline (npt, log_seglen, codatum->fitaps);
            if (codatum->bestamp & FITOPT_AMP_PS) {cpgslw(wid);}
        }
        cpgsci (11);                /* PLOT Amp Data in lt blue */
        if (codatum->didfits & FITOPT_AMP_PO) {
            if (codatum->bestamp & FITOPT_AMP_PO) {cpgqlw(&wid);cpgslw(5*wid);}
            cpgsci (8);                 /* PLOT Amp PO in orange */
            cpgline (npt, log_seglen, codatum->fitapo);
            if (codatum->bestamp & FITOPT_AMP_PO) {cpgslw(wid);}
        }
        if (codatum->didfits & FITOPT_AMP_SO) {
            if (codatum->bestamp & FITOPT_AMP_SO) {cpgqlw(&wid);cpgslw(5*wid);}
            cpgsci (12);                /* PLOT Amp SO in purple */
            cpgline (npt, log_seglen, codatum->fitaso);
            if (codatum->bestamp & FITOPT_AMP_SO) {cpgslw(wid);}
        }
        cpgsci (11);                /* PLOT Amp Data in lt blue */
        cpgpt (npt, log_seglen, codatum->ampl, 3);              /*symbols*/
        cpgerry (npt, log_seglen, amplow, amphigh, 0.5);        /*errbars*/

        if (codatum->datum->noloss_cotime > 0)
            {
            cpgsci (2);                 /* plateau-slope nominal */
            cpgline (3, fit_seglen, fitampl);
            cpgsci (3); cpgqlw (&wid); cpgslw (3 * wid); cpgsls(4);
            cpgmove (log_bpoint, 0.0);
            cpgdraw (log_bpoint, amplmax*1.1);  /* vert. breakpoint line */
            cpgslw (wid); cpgsls(1);
            if (codatum->labels)
                {
                cpgsch (lht);           /* Set label character height */
                cpgmtxt("LV", -3.0, 0.15, 0.0, "(br.pnt)");
                }

            log_ncotime = log10 ((double)(codatum->datum->noloss_cotime));
            if (log_ncotime > (log_segmax + 0.3)) log_ncotime = log_segmax+0.3;
                                        /* Cotime is green, bold */
            cpgsci (3); cpgqlw (&wid); cpgslw (5 * wid);
            cpgmove (log_ncotime, 0.0);
            cpgdraw (log_ncotime, amplmax*1.1); /* vert. cohloss line */
            cpgslw (wid); 
            if (codatum->labels)
                {
                cpgsch (lht);           /* Set label character height */
                cpgmtxt("RV", -3.0, 0.15, 1.0, "loss");
                }
            }
        else if (codatum->labels)
            { /* codatum->datum->noloss_cotime <= 0 means fit failed */
            cpgsci (2);
            cpgqlw(&wid);cpgslw(2*wid);
            /* be more clear on the failures */
            if (codatum->datum->noloss_cotime == -1.0)
                snprintf(bad_fit, MAX_TXT-2, "Bad Fit (Amplitude)");
            else if (codatum->datum->noloss_cotime == -2.0)
                snprintf(bad_fit, MAX_TXT-2, "Never fit Amplitude");
            else
                snprintf(bad_fit, MAX_TXT-2, "Bad coherence time");
            cpgsch (lht);               /* Set label character height */
            cpgmtxt ("B", -1.5, 0.1, 0.0, bad_fit);
            cpgslw (wid);
            }

        }

    return;
    }

/*
 * eof vim:nospell
 */
