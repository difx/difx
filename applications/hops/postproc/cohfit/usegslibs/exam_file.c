/*
 * Output a file of data formatted for handling by gnuplot
 * It is called after the fit and dumps out data independent of
 * the pgplot plot_codata() function.  Some information is
 * captured in the examdata object (per-file or globally).
 */
#include "cohfit.h"
#include "mk4_util.h"

/* for readability below */
#define FITHDRSTR "%5s %4s "\
    "%10s %8s  %10s %7s  %10s %10s %10s %10s %10s %10s\n"
#define FITDATSTR "%5.0f %4d "\
    "%10.4e %8.2e  %10.4e %7.1e  %10.4e %10.4e %10.4e %10.4e %10.4e %10.4e\n"
#define MINMAXGAME(C,S,N,X) do {\
        if(codatum->C[S] < N) N = codatum->C[S];\
        if(codatum->C[S] > X) X = codatum->C[S];\
    } while (0);
#define MINTRIMSNR(C,S) do {\
        if(codatum->C[S] <= 0.0) codatum->C[S] = 0.0;\
    } while (0);

void exam_file(cosumary *codatum, int bno, examdata *exdata)
{
    edatum  *edp = &(exdata->edata[bno]);
    FILE    *fpe;
    int year, day, hour, min, sec;
    char    *fitname;
    float   amin = 10000.0, amax = 0;
    float   smin = 1000000.0, smax = 0;

    /* no output if no examfile was written */
    if (!(edp->examfile)) return;
    fpe = fopen(edp->examfile, "w");
    edp->bno = bno;
    strncpy(edp->examfile, codatum->exampat, sizeof(edp->examfile)-1);
    msg("examfile #%d:  %s", 2, bno, edp->examfile);
    int_to_time (codatum->datum->time_tag, &year, &day, &hour, &min, &sec);
    /* in case there is any confusion */
    snprintf(edp->frlabel, MAX_TXT-1,
        "%04d-%03d-%02d%02d%02d/%s.%c.%d.%s pol %2s",
        1900+year, day, hour, min, sec,
            codatum->datum->baseline, codatum->datum->freq_code,
            codatum->datum->extent_no, codatum->datum->root_id,
            codatum->datum->polarization);
    fprintf(fpe, "#\n# file %d: %s\n# fringe %s\n#\n",
        bno, edp->examfile, edp->frlabel);
    fprintf(fpe, "# plateau = %g breakpoint = %g slope = %g loss = %.1f%%\n",
        edp->plateau = codatum->plateau,
        edp->breakpoint = codatum->breakpoint,
        edp->slope = codatum->slope,
        edp->cohereloss = codatum->cohereloss*100);

    /* save coherence times and some fit data */
    edp->ampl_cotime = codatum->ampl_cotime;
    edp->snr_cotime = codatum->snr_cotime;
    fprintf(fpe, "# amp/SNR coherence times  %.1f %.1f\n",
        edp->ampl_cotime, edp->snr_cotime);
    for (min = 0; min < NFITOPT; min++)
        edp->redchisq[min] = codatum->redchisq[min];
    for (day = 0; day < 3; day++) {
        if (day < sizeof(edp->pspar)) edp->pspar[day] = codatum->pspar[day];
        if (day < sizeof(edp->pserr)) edp->pserr[day] = codatum->pserr[day];
        if (day < sizeof(edp->popar)) edp->popar[day] = codatum->popar[day];
        if (day < sizeof(edp->poerr)) edp->poerr[day] = codatum->poerr[day];
        if (day < sizeof(edp->sopar)) edp->sopar[day] = codatum->sopar[day];
        if (day < sizeof(edp->soerr)) edp->soerr[day] = codatum->soerr[day];
    }
    fitname = as_fit_nm_ndx(codatum->bestamp, &(edp->bestampndx));
    fprintf(fpe, "# best ampl fit was: %s (red. chisq %g)\n#\n",
        fitname, edp->redchisq[edp->bestampndx]);
    fitname = as_fit_nm_ndx(codatum->bestsnr, &(edp->bestsnrndx));
    fprintf(fpe, "# best SNR fit was:  %s (red. chisq %g)\n#\n",
        fitname, edp->redchisq[edp->bestsnrndx]);

    /* dump out all the segment array values -- starting with a header */
    fprintf(fpe, FITHDRSTR,
        "#secs", "navg", "ampl", "amperr", "snr", "snrerr",
        "fitaps", "fitapo", "fitaso", "fitsnr", "fit2p8", "fit2p7");
    for (sec = 0; sec < codatum->nsegtime; sec++) {
        /* no point to min/max, but SNR must be > 0.0 */
        MINTRIMSNR(fitsnr, sec);
        fprintf(fpe, FITDATSTR,
            codatum->seglen[sec], codatum->navg[sec],
            codatum->ampl[sec], (codatum->snr[sec] > 0)
                ? codatum->ampl[sec] / codatum->snr[sec] : 0.0,
            codatum->snr[sec], 1.0,     /* one-sigma error bars */
            codatum->fitaps[sec], codatum->fitapo[sec], codatum->fitaso[sec],
            codatum->fitsnr[sec], codatum->fit2p8[sec], codatum->fit2p7[sec]);
        MINMAXGAME(ampl,   sec, amin, amax);
        MINMAXGAME(fitaps, sec, amin, amax);
        MINMAXGAME(fitapo, sec, amin, amax);
        MINMAXGAME(fitaso, sec, amin, amax);
        MINMAXGAME(snr,    sec, smin, smax);
#if HAVEGSL2P8
        MINMAXGAME(fit2p8, sec, smin, smax);
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
        MINMAXGAME(fit2p7, sec, smin, smax);
#endif /* HAVEGSL2P7 */
    }
    edp->lenmin = (double) codatum->seglen[0] * 0.90;
    if (edp->lenmin < 1) edp->lenmin = 1;
    edp->lenmax = (double) codatum->seglen[sec-1] * 1.10;
    if (edp->lenmax > MAX_COTIME) edp->lenmax = codatum->maxcotime;
    edp->snrmin = (smin - 1.0) * 0.90;
    if (edp->snrmin < 0.0) edp->snrmin = 0.0;
    edp->snrmax = (smax + 1.0) * 1.10;
    edp->ampmin = (amin * (1.0 - 1.0/smax)) * 0.90;
    if (edp->ampmin < 0.0) edp->ampmin = 0.0;
    edp->ampmax = (amax * (1.0 + 1.0/smax)) * 1.10;

    fprintf(fpe, "#\n# %12f < len < %12f\n"
                 "# %12f < amp < %12f\n# %12f < snr < %12f\n"
                 "#\n# eof vim:nospell\n#\n",
        edp->lenmin, edp->lenmax,
        edp->ampmin, edp->ampmax, edp->snrmin, edp->snrmax);
    fclose(fpe);
}

/*
 * eof vim:nospell
 */
