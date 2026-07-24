/*
 * Output a file of data formatted for handling by gnuplot
 * Per base-pol data is captured in exdata->edata[bno]
 * and we here finish up for a scan_boundary.  The gnuplot
 * commands are defined in the template file with defined
 * strings:
 *   GNUPLOT_PDFILE     -- pdfcairo setup
 *   GNUPLOT_PERBNO     -- per-file choices
 *   GNUPLOT_SETTINGS   -- standard configuration choices
 *   GNUPLOT_TWOPANEL*  -- setup for a SNR under AMP 2-panel plot
 *   GNUPLOT_CODA       -- and end of file marker
 * Each has an accompanying *_BASE define with the approximate
 * size of the string, should allocation be needed.  The first
 * three have %-items for printf().  The others are not configurable.
 *  fringe title
 *  data file
 *  xrange: min and max (%lf)
 *  amprange: min and max (%lf)
 *  snrrange: min and max (%lf)
 *  amp and snr coherence time estimates (%lf)
 *
 * The gnuplot(1) program itself is provided as a Makefile
 * #define if the program was found at configuration time.
 * If the program is missing, the files are still generated
 * (for some other processing) but no pdf is made.  (Technically,
 * if missing, it will be set to /bin/false, but that is a detail.)
 *
 * montage -tile 2x2 -geometry +2+2 -density 150 fff-dets-*pdf ff-dets.pdf
 * An alternative is to use pdflatex includegraphics to do the dance.
 */
#define _GNU_SOURCE
#include <stdlib.h>
#include "cohfit.h"
#include "exam_gnuplot.h"

/* We have only tested on gnuplot 4.6 and 6.0; the only significant
 * differences (for us) are dashtype and bold-enhancing.  This function
 * does quick tests to revise the defaults.  This could be an autoconf
 * test, but it won't be in HOPS4, so for the moment we internalize the
 * checks here.  The commands are constructed so that the grep result
 * answers the questions.  We have written the test in bash syntax and
 * that may not be the system default shell, so we have to be anal here,
 * presuming that both BASH and GNUPLOT have been defined. */
void set_gnuplot_opts(examdata *exdp)
{
#if defined(BASH) && defined(GNUPLOT)
    static char dashtype[] = "%s -c \"%s -e 'help dashtype'"
        " </dev/null |& grep 'dashtype.*pattern' >/dev/null 2>&1\"";
    static char boldface[] = "%s -c \"%s -e 'help enhanced'"
        " </dev/null |& grep '{/:Bold' >/dev/null 2>&1\"";
    char *cmd = malloc(MAX_TXT*10);
    int rc;
    if (!cmd) { perror("set_gnuplot_opts:malloc"); return; }
    rc = snprintf(cmd, MAX_TXT*10, dashtype, BASH, GNUPLOT);
    if (rc < MAX_TXT*10) {
        msg(cmd, 0);
        rc = system(cmd);
        exdp->gpdashtype = (rc == 0) ? 1 : 0;
        msg("Setting gpdashtype = %d", 0, exdp->gpdashtype);
    }
    rc = snprintf(cmd, MAX_TXT*10, boldface, BASH, GNUPLOT);
    if (rc < MAX_TXT*10) {
        msg(cmd, 0);
        rc = system(cmd);
        exdp->gpboldface = (rc == 0) ? 1 : 0;
        msg("Setting gpboldface = %d", 0, exdp->gpboldface);
    }
    memset(cmd, 0, MAX_TXT*10);
    free(cmd);
#else /* defined(BASH) && defined(GNUPLOT) */
#warning "Unable to configure for GNUPLOT options"
    msg("Unable to configure for GNUPLOT options", 3);
#endif /* defined(BASH) && defined(GNUPLOT) */
}

/* details for one such file */
void exam_sum(examdata *exdp, int bb, FILE *fps)
{
    edatum *edp = &exdp->edata[bb];
    fprintf(fps, "%s(file %d):\n"
        "fringe: %s\n"
        "plateau = %g breakpoint = %g slope = %g\n"
        "ampl_cotime = %.3f snr_cotime = %.3f\n",
        edp->examfile, exdp->pbno + bb,
        edp->frlabel,
        edp->plateau, edp->breakpoint, edp->slope,
        edp->ampl_cotime, edp->snr_cotime);
    if (edp->redchisq[FITOPT_NDX_PS] > 0)
      fprintf(fps, "ps plateau-slope reduced chisq = %.3f\n"
        "ps Amp(t) = A or A + B log10(t/C)\n"
        "ps A = %.5f +/- %.5f\n"
        "ps B = %.5f +/- %.5f\n"
        "ps C = %.5f +/- %.5f\n",
        edp->redchisq[FITOPT_NDX_PS],
        edp->pspar[0], edp->pserr[0],
        edp->pspar[1], edp->pserr[1],
        edp->pspar[2], edp->pserr[2]);
    if (edp->redchisq[FITOPT_NDX_PO] > 0)
      fprintf(fps, "po plateau-only reduced chisq = %.3f\n"
        "po Amp(t) = A\n"
        "po A = %.5f +/- %.5f\n",
        edp->redchisq[FITOPT_NDX_PO],
        edp->popar[0], edp->poerr[0]);
    if (edp->redchisq[FITOPT_NDX_SO] > 0)
      fprintf(fps, "slope-only reduced chisq = %.3f\n"
        "so Amp(t) = A + B log10(t/t[min]\n"
        "so A = %.5f +/- %.5f\n"
        "so B = %.5f +/- %.5f\n",
        edp->redchisq[FITOPT_NDX_SO],
        edp->sopar[0], edp->soerr[0],
        edp->sopar[1], edp->soerr[1]);
    fprintf(fps, "best amplitude fit %s\n\n", as_fit_ndx_nm(edp->bestampndx));
}

/* generate a summary file */
void exam_summary(examdata *exdp)
{
    int sumnmlen = exdp->elen + 32, nc, bb;
    char *sumname = malloc(sumnmlen), *pc, tbbn = exdp->pbno + exdp->nbno;
    char *pcwd;
    FILE *fps;

    if (!sumname) { perror("exam_summary:malloc"); return; }
    pc = strchr(exdp->epat, '%');     /* expect -%<integer>d */
    *--pc = 0;
    nc = snprintf(sumname, sumnmlen, "%s-%d.summary.txt", exdp->epat, tbbn);
    *pc = '-';                  /* restore it to be clean */
    if (nc >= sumnmlen) {
        msg("Unable to create summary file, need %d only have %d chars", 3,
            nc, sumnmlen);
        return;
    }
    fps = fopen(sumname, "w");
    pcwd = get_current_dir_name();
    if (pcwd) {
        if (strlen(pcwd) < 80) fprintf(fps, "workdir: %s\n", pcwd);
        else fprintf(fps, "workdir: %.80s...\n", pcwd);
        free(pcwd);
    }
    if (exdp->edit) fprintf(fps, "config controlled by %s\n", exdp->edit);
    fprintf(fps, "coherence loss = %g max coherence time = %g\n\n",
        exdp->closs, exdp->mxcotime);
    for (bb = 0; bb < exdp->nbno; bb++) exam_sum(exdp, bb, fps);
    fclose(fps);
}

/* another helper */
void exam_runit(char *cmd)
{
    int rc = system(cmd);
    if (rc) {
        msg("system(%s) failed (%d)", 3, cmd, rc);
        perror("exam_plot:system");
    } else {
        msg("system(%s) successful", 0, cmd);
    }
    free(cmd);
}

/* for readability below */
#define EXAM_RETURN(R,C,S) do {\
        msg("Unable to montage: need %d only have %d chars", 3, R, C);\
        msg("CMD so far: %s", 3, S);\
    return;} while(0)

/* this builds up the montage command, and as a last step, runs it */
void exam_montage(examdata *exdp, int bbn, char *pdf[bbn])
{
    int tbbn = exdp->pbno + exdp->nbno;
    int cmdlen = (bbn+1)*(exdp->elen + 8) + strlen(MONTAGE) + 64, nc = 0, bb;
    char *montyc = malloc(cmdlen), *mtpdfo = malloc(exdp->elen + 32), *pc;

    if (!montyc) { perror("exam_montage:malloc:1"); return; }
    if (!mtpdfo) { perror("exam_montage:malloc:2"); return; }
    pc = strchr(exdp->epat, '%');     /* expect -%<integer>d */
    *--pc = 0;
    nc = snprintf(mtpdfo, exdp->elen + 32,
        " %s-%d.montage.pdf", exdp->epat, tbbn);
    *pc = '-';                  /* restore it to be clean */
    if (nc >= exdp->elen + 32) EXAM_RETURN(nc, exdp->elen+32, mtpdfo);

    /* the basic montage command */
    nc = snprintf(montyc, cmdlen,
        "%s -tile %dx%d -geometry +2+2 -density %d ",
        MONTAGE, exdp->rnc[1], exdp->rnc[0], exdp->density);
    if (--nc >= cmdlen) EXAM_RETURN(nc, cmdlen, montyc);

    /* pile on the file names */
    for (bb = 0; bb < bbn; bb++) {
        nc += snprintf(montyc + nc, strlen(pdf[bb]) + 2, " %s ", pdf[bb]);
        if (--nc >= cmdlen) EXAM_RETURN(nc, cmdlen, montyc);
        msg("%s %d == %d?", 0, montyc, nc, strlen(montyc));
    }

    /* and finally add the output file */
    nc += snprintf(montyc + nc, exdp->elen + 16, "%s", mtpdfo);
    if (nc >= cmdlen) EXAM_RETURN(nc, cmdlen, montyc);
    msg("Montage command len %d < limit %d is:", 1, nc, cmdlen);
    exam_runit(montyc);
}

/* a helper that runs system() on the commandfile */
void exam_plot(char *cmds)
{
    int len = strlen(cmds) + strlen(GNUPLOT) + 16;
    char *syscmd = malloc(len);
    snprintf(syscmd, len-1, "%s %s", GNUPLOT, cmds);
    exam_runit(syscmd);
}

/* update limits with the per-plot values and set key locations */
void exam_custom(edatum *edp, double limits[6], char **ksnr, char **kamp)
{
    /* xrange: min and max (%lf) */
    limits[0] = edp->lenmin;
    limits[1] = edp->lenmax;
    /* amprange: min and max (%lf) */
    limits[2] = edp->ampmin;
    limits[3] = edp->ampmax;
    /* snrrange: min and max (%lf) */
    limits[4] = edp->snrmin;
    limits[5] = edp->snrmax;
    /* force bottom limits to be sensible */
    if (limits[2] < 0) limits[2] = 0.0;
    if (limits[4] < 0) limits[4] = 0.0;
    /* in the custom case it is perhaps unlikely to need KEYTOP */
    if (ksnr) *ksnr = KEYBOT;
    if (kamp) *kamp = KEYBOT;
}

/* helper function to establish global plot limits */
void exam_setlimits(examdata *exdp, double limits[6],
    const int nbb, char *ksnr[nbb], char *kamp[nbb])
{
    int bb;
    char *kybot = KEYBOT;
    char *kytop = KEYTOP;
    edatum *edp;
    double midamp, midsnr, gmamp, gmsnr;

    /* first establish limits */
    for (bb = 0; bb < nbb; bb++) {
        edp = &exdp->edata[bb];
        if (bb == 0) {              /* first edp establishes values */
            exam_custom(edp, limits, NULL, NULL);
        } else {                    /* otherwise expand as needed */
            if (edp->lenmin < limits[0]) limits[0] = edp->lenmin;
            if (edp->lenmax > limits[1]) limits[1] = edp->lenmax;
            if (edp->ampmin < limits[2]) limits[2] = edp->ampmin;
            if (edp->ampmax > limits[3]) limits[3] = edp->ampmax;
            if (edp->snrmin < limits[4]) limits[4] = edp->snrmin;
            if (edp->snrmax > limits[5]) limits[5] = edp->snrmax;
            kamp[bb] = KEYBOT;
            ksnr[bb] = KEYBOT;
        }
    }
    if (exdp->customlimits) return;

    gmamp = (limits[2] + limits[3]) / 2.0;
    gmsnr = (limits[4] + limits[5]) / 2.0;
    for (bb = 0; bb < nbb; bb++) {
        edp = &exdp->edata[bb];
        midamp = (edp->ampmin + edp->ampmax) / 2.0;
        midsnr = (edp->snrmin + edp->snrmax) / 2.0;
        ksnr[bb] = midsnr < gmsnr ? kytop : kybot;
        kamp[bb] = midamp < gmamp ? kytop : kybot;
    }
}

/* set the default line color, weight and dashtype */
void exam_default_styles(
    const int hdt, const int n, char *rgb[n], int lw[n], char *dt[n])
{
    /* default colors */
    rgb[0] = "ignored";
    rgb[1] = "medium-blue";     rgb[2] = "dark-violet";
    rgb[3] = "olive";           rgb[4] = "orange-red";
    rgb[5] = "navy";            rgb[6] = "light-red";
    rgb[7] = "forest-green";    rgb[8] = "dark-goldenrod";
    /* defaults for lw, dt, lt and title that have ps and cbs as winners */
    lw[0] = 0;
    lw[1] = lw[5] = lw[6] = lw[8] = 1;
    lw[2] = lw[3] = lw[4] = 2;  lw[7] = 3;
    if (hdt) {
        dt[0] = 0;
        dt[1] = dt[2] = dt[5] = dt[7] = "dt solid";
        dt[3] = "dt '.._'";     dt[4] = "dt '...'";
        dt[6] = "dt '---'";     dt[8] = "dt '- -'";
    } else {
        dt[0] = dt[1] = dt[2] = dt[3] =
        dt[4] = dt[5] = dt[6] = dt[7] = dt[8] = " ";
    }
}
    
/* swap format to indicate best amp fit */
char *exam_bestamp(const int hbf, char *ltit[NFITOPT], edatum *edp)
{
    static char plot_amp[GNUPLOT_TWOPANEL_PLOT_AMP_BASE];
    char *ampe = plot_amp;
    int ns = 0, na = GNUPLOT_TWOPANEL_PLOT_AMP_BASE;
    switch(edp->bestampndx) {
    case FITOPT_NDX_PS:
        ltit[FITOPT_NDX_PS] = (hbf)
            ? "lt 2 tit '{/:Bold plateau-slope}'"
            : "lt 2 tit 'plateau-slope -- best'";
        ltit[FITOPT_NDX_PO] = "lt 3 tit 'plateau-only'";
        ltit[FITOPT_NDX_SO] = "lt 4 tit 'slope-only'";
        break;
    case FITOPT_NDX_PO:
        ltit[FITOPT_NDX_PS] = "lt 3 tit 'plateau-slope'";
        ltit[FITOPT_NDX_PO] = (hbf)
            ? "lt 2 tit '{/:Bold plateau-only}'"
            : "lt 2 tit 'plateau-only -- best'";
        ltit[FITOPT_NDX_SO] = "lt 4 tit 'slope-only'";
        break;
    case FITOPT_NDX_SO:
        ltit[FITOPT_NDX_PS] = "lt 4 tit 'plateau-slope'";
        ltit[FITOPT_NDX_PO] = "lt 3 tit 'plateau-only'";
        ltit[FITOPT_NDX_SO] = (hbf)
            ? "lt 2 tit '{/:Bold slope-only}'"
            : "lt 2 tit 'slope-only -- best'";
        break;
    case NFITOPT:
        msg("Nothing fit, not model plotted", 3);
        break;
    default:
        msg("Dev. error in exam_bestamp() best = %d", 3, edp->bestampndx);
        exit(1);
    }

    ns = snprintf(ampe, na, "plot \\\n");
    if (ns >= na) return("plot 0.05\n");
    ampe = ampe + ns;     na -= ns;
    if (edp->redchisq[FITOPT_NDX_PS] > 0) {
        ns = snprintf(ampe, na, "  df u 1:7  w line %s, \\\n",
            ltit[FITOPT_NDX_PS]);
        if (ns >= na) return("plot 0.06\n");
        ampe = ampe + ns;     na -= ns;
    }
    if (edp->redchisq[FITOPT_NDX_PO] > 0) {
        ns = snprintf(ampe, na, "  df u 1:8  w line %s, \\\n",
            ltit[FITOPT_NDX_PO]);
        if (ns >= na) return("plot 0.07\n");
        ampe = ampe + ns;     na -= ns;
    }
    if (edp->redchisq[FITOPT_NDX_SO] > 0) {
        ns = snprintf(ampe, na, "  df u 1:9  w line %s, \\\n",
            ltit[FITOPT_NDX_SO]);
        if (ns >= na) return("plot 0.08\n");
        ampe = ampe + ns;     na -= ns;
    }
    ns = snprintf(ampe, na, 
        "  df u 1:3:4 w yerrorlines lt 1 tit 'Amp data'\n");
    if (ns >= na) return("plot 0.09\n");

    return(plot_amp);
}

/* swap format for best snr fit */
char *exam_bestsnr(const int hbf, char *ltit[NFITOPT], edatum *edp)
{
    static char plot_snr[GNUPLOT_TWOPANEL_PLOT_SNR_BASE];
    char *snre = plot_snr;
    int ns = 0, na = GNUPLOT_TWOPANEL_PLOT_SNR_BASE;
    ltit[FITOPT_NDX_3PT] = "lt 6 tit 'parabolic'";
    switch(edp->bestsnrndx) {
    case FITOPT_NDX_3PT:
        ltit[FITOPT_NDX_3PT] = "lt 6 tit 'parabolic'";
        ltit[FITOPT_NDX_2P8] = (hbf)
            ? "lt 7 tit '{/:Bold cubic spline (GSL2.8)}'"
            : "lt 7 tit 'cubic spline (GSL2.8) -- best'";
        ltit[FITOPT_NDX_2P7] = "lt 8 tit 'cubic spline (GSL2.7)'";
        break;
#if HAVEGSL2P8
    case FITOPT_NDX_2P8:
        ltit[FITOPT_NDX_3PT] = "lt 6 tit 'parabolic'";
        ltit[FITOPT_NDX_2P8] = (hbf)
            ? "lt 7 tit '{/:Bold cubic spline (GSL2.8)}'"
            : "lt 7 tit 'cubic spline (GSL2.8) -- best'";
        ltit[FITOPT_NDX_2P7] = "lt 8 tit 'cubic spline (GSL2.7)'";
        break;
#endif /* HAVEGSL2P8 */
#if HAVEGSL2P7
    case FITOPT_NDX_2P7:
        ltit[FITOPT_NDX_3PT] = "lt 6 tit 'parabolic'";
        ltit[FITOPT_NDX_2P8] = "lt 8 tit 'cubic spline (GSL2.8)'";
        ltit[FITOPT_NDX_2P7] = (hbf)
            ? "lt 7 tit '{/:Bold cubic spline (GSL2.7)}'"
            : "lt 7 tit 'cubic spline (GSL2.7) -- best'";
        break;
#endif /* HAVEGSL2P7 */
    case NFITOPT:
        msg("Nothing fit, not model plotted", 3);
        break;
    default:
        msg("Dev. error in exam_bestsnr() best = %d", 3, edp->bestsnrndx);
        exit(1);
    }

    ns = snprintf(snre, na, "plot \\\n");
    if (ns >= na) return("plot 0.00\n");
    snre = snre + ns;     na -= ns;
    if (edp->redchisq[FITOPT_NDX_3PT] > 0) {
        ns = snprintf(snre, na, "  df u 1:10 w line %s, \\\n",
            ltit[FITOPT_NDX_3PT]);
        if (ns >= na) return("plot 0.01\n");
        snre = snre + ns;     na -= ns;
    }
    if (edp->redchisq[FITOPT_NDX_2P8] > 0) {
        ns = snprintf(snre, na, "  df u 1:11 w line %s, \\\n",
            ltit[FITOPT_NDX_2P8]);
        if (ns >= na) return("plot 0.02\n");
        snre = snre + ns;     na -= ns;
    }
    if (edp->redchisq[FITOPT_NDX_2P7] > 0) {
        ns = snprintf(snre, na, "  df u 1:12 w line %s, \\\n",
            ltit[FITOPT_NDX_2P7]);
        if (ns >= na) return("plot 0.03\n");
        snre = snre + ns;     na -= ns;
    }
    ns = snprintf(snre, na,
        "  df u 1:5:6 w yerrorlines lt 5 tit 'SNR data'\n");
    if (ns >= na) return("plot 0.04\n");
    return(plot_snr);
}

/* populate the label and object details */
void exam_labobj(edatum *edp, labobj *snr, labobj *abp, labobj *ach,
    double lim[6], int ampistop, int snristop)
{
    /* for starters */
    snr->fc = "dark-red";       snr->dens = 0.1;
    abp->fc = "forest-green";   abp->dens = 0.1;
    ach->fc = "navy";           ach->dens = 0.1;

    /* rectangles from left,bottom to right,top */
    snr->top = edp->snrmax;
    snr->lft = edp->snr_cotime - 0.5;
    snr->rgt = edp->snr_cotime + 0.5;
    snr->bot = edp->snrmin;
    abp->top = edp->ampmax;
    abp->lft = edp->breakpoint - 0.5;
    abp->rgt = edp->breakpoint + 0.5;
    abp->bot = edp->ampmin;
    ach->top = edp->ampmax;
    ach->lft = edp->ampl_cotime - 0.5;
    ach->rgt = edp->ampl_cotime + 0.5;
    ach->bot = edp->ampmin;

    /* labels are near a rect. corner: bp is right just, others are left;
     * the y axis is upper or lower depending on where the data is in plot */
    {
        double pltamp = (lim[2] + lim[3]) / 2.0;
        double pltsnr = (lim[4] + lim[5]) / 2.0;
        double midamp = (edp->ampmin + edp->ampmax) / 2.0;
        double midsnr = (edp->snrmin + edp->snrmax) / 2.0;
        double fracti;
        snr->x = snr->rgt * 1.05;
        fracti = (midsnr < pltsnr)
               ? (snristop ? 0.5 : 0.9)
               : (snristop ? 0.5 : 0.1);
        snr->y = fracti * snr->top + (1.0-fracti) * snr->bot;

        abp->x = abp->lft / 1.05;
        fracti = (midamp < pltamp)
               ? (ampistop ? 0.5 : 0.9)
               : (ampistop ? 0.1 : 0.5);
        abp->y = fracti * abp->top + (1.0-fracti) * abp->bot;

        ach->x = ach->rgt * 1.05;
        fracti = (midamp < pltamp)
               ? (ampistop ? 0.5 : 0.9)
               : (ampistop ? 0.1 : 0.5);
        ach->y = fracti * ach->top + (1.0-fracti) * ach->bot;
    }
}

/* this creates .gnu and makes the pdfs */
void exam_gnuplot(examdata *exdp)
{
    int bb, bbn, lw[GNUPLOT_LINETYPES], amptop, snrtop;
    char *dt[GNUPLOT_LINETYPES], *rgb[GNUPLOT_LINETYPES];
    char *pdfbno[exdp->nbno], *gnucmds = NULL, *pltamp, *pltsnr;
    char *keysnr[exdp->nbno], *keyamp[exdp->nbno], *ltit[NFITOPT];
    double limits[6];
    edatum *edp;
    labobj snr, abp, ach;
    FILE *fpg;
    /* exit quietly if no data to work with */
    if (!exdp->epat || 0 == exdp->elen || 0 == exdp->nbno) return;
    msg("", 1);
    msg("exam_gnuplot on '%s' (%d) with %d files: %s %s", 1,
        exdp->epat, exdp->elen, exdp->nbno,
        exdp->gnuplot ? GNUPLOT : "disabled",
        exdp->montage ? MONTAGE : "disabled");

    /* defaults and the global limits for this scan group */
    exam_default_styles(exdp->gpdashtype, GNUPLOT_LINETYPES, rgb, lw, dt);
    exam_setlimits(exdp, limits, (const int)exdp->nbno, keysnr, keyamp);

    /* generate a gnu command file */
    for (bb = 0; bb < exdp->nbno; bb++) {
        edp = &exdp->edata[bb];             /* convenience pointer */
        gnucmds = malloc(strlen(edp->examfile) + 16);
        if (!gnucmds) { perror("examgnuplot:malloc:1"); return; }
        pdfbno[bb] = malloc(strlen(edp->examfile) + 16);
        if (!pdfbno[bb]) { perror("examgnuplot:malloc:2"); return; }
        sprintf(gnucmds, "%s.gnu", edp->examfile);
        sprintf(pdfbno[bb], "%s.pdf", edp->examfile);
        fpg = fopen(gnucmds, "w");
        /* see exam_gnuplot.h for argument dispositions */
        fprintf(fpg, GNUPLOT_PDFILE, 5.0, 6.0, "Sans", 14, pdfbno[bb]);
        if (exdp->customlimits)
            exam_custom(edp, limits, &keysnr[bb], &keyamp[bb]);
        fprintf(fpg, GNUPLOT_PERBNO,
            exdp->gpboldface, edp->frlabel, edp->frlabel, edp->examfile,
            limits[0], limits[1], limits[2], limits[3], limits[4], limits[5],
            edp->ampl_cotime, edp->snr_cotime);
        amptop = (keyamp[bb][KEYTBL] == 't') ? 1 : 0;
        snrtop = (keysnr[bb][KEYTBL] == 't') ? 1 : 0;
        exam_labobj(edp, &snr, &abp, &ach, limits, amptop, snrtop);
        fprintf(fpg, GNUPLOT_SETTINGS,
            lw[1], dt[1], rgb[1], lw[2], dt[2], rgb[2], lw[3], dt[3], rgb[3],
            lw[4], dt[4], rgb[4], lw[5], dt[5], rgb[5], lw[6], dt[6], rgb[6],
            lw[7], dt[7], rgb[7], lw[8], dt[8], rgb[8]);
        pltsnr = exam_bestsnr(exdp->gpboldface, ltit, edp);
        pltamp = exam_bestamp(exdp->gpboldface, ltit, edp);
        fprintf(fpg, GNUPLOT_TWOPANEL_HEAD_BLK,
            keysnr[bb], exdp->labels, edp->snr_cotime, snr.x, snr.y, snr.fc,
            snr.lft,snr.bot,snr.rgt,snr.top, snr.fc, snr.dens, snr.fc);
        fputs(pltsnr, fpg);     /* output SNR plot command */
        fprintf(fpg, GNUPLOT_TWOPANEL_MID_BLCK,
            keyamp[bb],
            ((edp->bestampndx == FITOPT_NDX_PS) ? exdp->labels : 0),
            edp->breakpoint, abp.x, abp.y, abp.fc,
            abp.lft,abp.bot,abp.rgt,abp.top, abp.fc, abp.dens, abp.fc,
            exdp->labels,
            edp->ampl_cotime, ach.x, ach.y, ach.fc, 
            ach.lft,ach.bot,ach.rgt,ach.top, ach.fc, ach.dens, ach.fc);
        fputs(pltamp, fpg);     /* output Amplitude plot command */
        fputs(GNUPLOT_CODA, fpg);
        fclose(fpg);
        /* now make the plot */
        if (exdp->gnuplot) exam_plot(gnucmds);
        free(gnucmds);
    }

    bbn = bb;
    /* create the montage and summary if we have permission and gnuplots */
    if (exdp->gnuplot && exdp->montage) exam_montage(exdp, bbn, pdfbno);
    exam_summary(exdp);

    for (bb = 0; bb < bbn; bb++)
        if (pdfbno[bb]) free(pdfbno[bb]);
}

/*
 * eof vim:nospell
 */
