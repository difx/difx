#define MAXSEG 5120
                                        /* Modes for clear_fxp() */
#define ALL    0
#define FILES  1
#define ACCUMS 2
                                        /* Modes for fringex itself */
#define CMODE      0x01
#define OMODE      0x02
#define QMODE      0x04
#define SEARCH     0x08
#define NOLOSS     0x10
#define BINARYMODE 0x20
#define SRCHPOS    0x40

#define PI 3.141592654

#include "mk4_data.h"
#include "adata.h"

#if BIGGER
// should become MAX_CHAN_PP from mk4_sizes.h ?
#define NFX_SB_64   64	    /* type 205 */
#define NFX_SB_32   32	    /* type 203 */
//#define NFX_FCHAN   16
#define NFX_FCHAN   64
#else /* BIGGER */
#define NFX_SB_64   16
#define NFX_SB_32   16
#define NFX_FCHAN   16
#endif /* BIGGER */

struct fxparam
    {
                                        /* Command line parameters */
    int         mode;
    int         no_amp_corr;
    double      raoff;
    double      decoff;
    double      rateoff;
    double      delayoff;
    double      userfreq;
    double      nsecs;
    int         account;
                                        /* Calculated parameters */
    double      fchan[NFX_FCHAN];
    double      ffit_reffreq;
    double      reffreq;
    double      reftime;
    double      dprate;
    double      acc_period;
    double      srch_cotime;
    double      noloss_cotime;
    double      delay;
    double      rate;
    double      bandwidth;
    double      amp_corr_fact;
                                        /* Binary file images */
    struct mk4_fringe *fringe;
    struct mk4_sdata *sdata[2];         /* ptrs to ref,rem station data */

                                        /* Interim segment sums */
    int         nsegs;
    double      tstart;
    double      numaccp;
    double      segstart;
    double      rsum[MAXSEG];
    double      isum[MAXSEG];
    double      segsec[MAXSEG];
    double      segcount[MAXSEG];
    double      seglen[MAXSEG];
                                        /* Receptacle for results */
    fringesum   adata;
    };

                                        /* Structure for looping functions */
#define MAXNSECS  50
#define MAXRATES  500
#define MAXDELAYS 500

struct loops
    {
    int         nnsec;
    double      nsecs[MAXNSECS];
    int         nrates;
    double      rates[MAXRATES];
    int         ndelays;
    double      delays[MAXDELAYS];
    };

extern int msglev;

extern void model (double, struct fxparam *, double *);

