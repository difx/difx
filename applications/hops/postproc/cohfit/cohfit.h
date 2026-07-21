/*
 * Header for cohfit
 *
 * Extensively hacked up April-May 2026 by GBC
 */
#define FALSE 0
#define TRUE  1

#include <stdio.h>

/* optind and optarg in unistd.h are used in main() and parse_cmdline() */
#include <unistd.h>
#include <gsl/gsl_errno.h>
#include "adata.h"
#include "msg.h"

#ifndef HAVEGSL2P8
#define HAVEGSL2P8 0
#endif /* HAVEGSL2P8 */
#ifndef HAVEGSL2P7
#define HAVEGSL2P7 0
#endif /* HAVEGSL2P7 */

/* note that in mk4_data.h #define MAXSTATIONS 32 */
#define MAX_BNO     120                 /* 20 stations */
#define MAX_NSEGLEN 100                 /* Fringex max -iarg MAXNSECS */
#define MAX_TXT      80                 /* pgplot labels */
#define MAX_ITERS   100                 /* max iterations */
#define NFITOPT       6                 /* number FITOPT bits */
#define MAX_COTIME  392.0               /* historical limit */
#define INIT_AVSP   500                 /* avg data storage, initially */
#define INCR_AVSP   100                 /* increment for more */

#ifdef requirefitolerances
/* some common constants needed for gsl_multifit_nlinear_driver() */
const double xtol = 1e-6;
const double gtol = 1e-6;
const double ftol = 0.0;
const size_t maxits = MAX_ITERS;
#endif /* requirefitolerances */

/* this is used for sorting the incoming data into proper order */
typedef struct avdata
    {
    int         order;
    int         lastorder;
    int         keyval;
    fringesum   fdata;
    } avg_data;
typedef struct avgdatastore
    {
    int         space;          /* space (re)allocated for avg_data */
    avg_data    *data;          /* pointer to avg_data so allocated */
    } avgspace;

/* this holds a scan-boundary group data through its processing */
typedef struct cosummary
    {
    /* inputs */
    fringesum   *datum;                 /* Data structure ptr for this scan */
    float       orig_srch;              /* copy of orig datum->src_cotime */
    float       ampl[MAX_NSEGLEN];      /* segment avg amplitudes */
    float       snr[MAX_NSEGLEN];       /* segment avg snr */
    float       seglen[MAX_NSEGLEN];    /* segment lengths (in seconds) */
    int         navg[MAX_NSEGLEN];      /* num avg'd segments per segment */
    /* outputs or options [-? arg] */
    float       fitsnr[MAX_NSEGLEN];    /* 3pt snr fit */
    float       fit2p8[MAX_NSEGLEN];    /* cubic spline (GSL2.8) snr fit */
    float       fit2p7[MAX_NSEGLEN];    /* cubic spline (GSL2.7) snr fit */
    float       fitaps[MAX_NSEGLEN];    /* ps amp fit result */
    float       fitapo[MAX_NSEGLEN];    /* po amp fit result */
    float       fitaso[MAX_NSEGLEN];    /* so amp fit result */
    float       plateau;                /* estimate of amplitude */
    float       breakpoint;             /* start of loss region */
    float       slope;                  /* decay rate in time */
    float       cohereloss;             /* [-s] 5% in the original */
    double      maxcotime;              /* [-c] coh. limit option */
    int         nsegtime;               /* how many segments == npt */
    int         fitmask;                /* [-f] allowed fits, see below */
    int         bestamp;                /* best amp fit, see below */
    int         bestsnr;                /* same for snr */
    int         didfits;                /* fit*[] &c. see below */
    int         iteratio[NFITOPT];      /* iterations on fits */
    float       redchisq[NFITOPT];      /* chisq/dof each fit */
    int         snr_peak[3];            /* seg indices for fit_snr() */
    int         cbs_p2p8[3];            /* seg indices for fit_cbs() */
    int         cbs_p2p7[3];            /* seg indices for fit_cbs() */
    double      pspar[3], pserr[3];     /* ps fit parameters, errors */
    double      popar[1], poerr[1];     /* po fit parameters, errors */
    double      sopar[2], soerr[2];     /* so fit parameters, errors */
    double      ampl_cotime;            /* ampl noloss coherence time */
    double      snr_cotime;             /* snr noloss coherence time */
    /* external reporting */
    char        *exampat;               /* [-e] filename base */
    int         examlen;                /* [-e] length of the base */
    int         labels;                 /* [-l] show/skip labels */
    } cosumary;

/* per-base-pol and accumulated knowledge for [-e] option */
typedef struct edatum {
    char        frlabel[MAX_TXT];       /* date/type2.stamp pol */
    double      lenmin, lenmax;         /* extrema of segment lengths */
    double      ampmax, ampmin;         /* extrema of amplitude */
    double      snrmax, snrmin;         /* extrema of SNR */
    double      plateau, breakpoint, slope, cohereloss;
    double      ampl_cotime, snr_cotime;
    double      redchisq[NFITOPT];
    double      pspar[3], pserr[3];
    double      popar[1], poerr[1];
    double      sopar[2], soerr[2];
    int         bestampndx, bestsnrndx;
    char        *examfile;              /* copy of output file */
    int         bno;                    /* ordinal of the file */
} edatum;

/* this is the top-level holder of command-line options and it also holds
 * the group of output data in the range [pbno..nbno) for the -e proceeing.
 * all char* items are allocated */
typedef struct examdata {
    /* processing options set via [-g] option, static for entire execution */
    int         gnuplot, montage;       /* defaults 1,1 */
    int         customlimits;           /* per plot, default 0 */
    int         density;                /* for montage, default 150 */
    int         rnc[2];                 /* row,col from [-g] option */
    int         labels;                 /* [-l] show/skip labels */
    int         fitmask;                /* [-f] allowed fits, see below */
    float       closs;                  /* [-s] coherence loss */
    double      mxcotime;               /* [-c] max coh time */
    char        *pdfile;                /* [-d] pdf file if /pdf given */
    char        *devp;                  /* [-d] device name */
    char        *epat;                  /* [-e] file base without pattern */
    int         elen;                   /* and its length is computed */
    char        *edit;                  /* [-g] plot edit file */
    int         gslegacy;               /* use legacy gsl_multifit_nlin.h */
    int         gpdashtype;             /* gnuplot supports dashtype */
    int         gpboldface;             /* gnuplot supports {:/Bold} */
    /* scan-boundary group data, cleared each scan-boundary */
    int         nbno;                   /* number of base-pols */
    int         pbno;                   /* previous total of such */
    edatum      edata[MAX_BNO];         /* per-file allocations */
} examdata;

/* values use in cosumary.fitmask and cosumary.bestamp above
 *   fitmask is what fits should be tried (i.e. code exercised)
 *   didfits is what fits were actually done (i.e output populated)
 *   bestamp is what fits was selected as best (i.e. final answer)
 *   bestsnr is what fits was selected as best (i.e. final answer)
 * these values must be consistent with [-f] option.  Various data
 * about the fits is also captured: model paramters, iterations and
 * reduced chisq.
 */
#define FITOPT_NOFITS   0x00    /* do no fits */
#define FITOPT_AMP_PS   0x01    /* do plateau-slope fit */
#define FITOPT_NDX_PS   0       /* < NFITOPT */
#define FITOPT_AMP_PO   0x02    /* do plateau-only fit */
#define FITOPT_NDX_PO   1       /* < NFITOPT */
#define FITOPT_AMP_SO   0x04    /* do slope-only fit */
#define FITOPT_NDX_SO   2       /* < NFITOPT */
#define FITOPT_SNR_3PT  0x10    /* do 3-point SNR fit at max */
#define FITOPT_NDX_3PT  3       /* < NFITOPT */
#define FITOPT_SNR_2P8  0x20    /* do GSL 2.8 cubic spline fit */
#define FITOPT_NDX_2P8  4       /* < NFITOPT */
#define FITOPT_SNR_2P7  0x40    /* do GSL 2.7 cubic spline fit */
#define FITOPT_NDX_2P7  5       /* < NFITOPT */
#define FITOPT_AMP_ALL  (FITOPT_AMP_PS|FITOPT_AMP_PO|FITOPT_AMP_SO)
#define FITOPT_SNR_ALL  (FITOPT_SNR_3PT|FITOPT_SNR_2P8|FITOPT_SNR_2P7)
#define FITOPT_ALLFITS  (FITOPT_AMP_ALL|FITOPT_SNR_ALL)

/* Fit conclusions that propagate to cofit output (fit_codata.c):
 *  never fit amplitude         => codatum->datum->noloss_cotime = -2.0;
 *   fit_ampl() != 0            => codatum->datum->noloss_cotime = -1.0;
 *   or noloss_cotime < 1.0     => codatum->datum->noloss_cotime =  1.0;
 *  never fit the SNR           => codatum->datum->srch_cotime   = -2.0;
 *  normalize_snr() != 0        => codatum->datum->srch_cotime   = -1.0;
 *  fit_snr <= 0                => codatum->datum->srch_cotime   = -1.0;
 * Negatives result in removal from output, otherwise a line is generated.
 * Note that if the cotime is the first or last segment time it means that
 * we don't really have a robust result.
 */

/* routines that move data around */
extern int normalize_snr(cosumary *, int );
extern void sorter(avg_data *, int);
extern int parse_cmdline(int, char **, FILE **, examdata *);
extern int read_data(avgspace *, char *, int *, int *);

extern int sort_data(avg_data *, int);
extern void plot_codata(cosumary *);
extern int write_codata(cosumary *, FILE *);
extern void clear_codata(cosumary *);

/* routines associated with [-e] and [-g] handling */
extern void exam_file(cosumary *, int, examdata *);
extern void exam_gnuplot(examdata *);
extern int exam_edit(char *, examdata *);
extern int get_gslegacy_default(void);
extern void set_gnuplot_opts(examdata *);

/* routines that are support fitting */
extern char *as_fit_nm_ndx(int, int*);
extern char *as_fit_ndx_nm(int);
extern int fit_ampl(cosumary *, int, int);
extern int fit_msnr(cosumary *, int);
extern void fit_codata(cosumary *, examdata *);

/* for non-abort GSL error handling */
extern gsl_error_handler_t err_handler;
extern char *get_err_handler_report();
extern void clear_err_handler_report(void);

/*
 * eof vim:nospell
 */
