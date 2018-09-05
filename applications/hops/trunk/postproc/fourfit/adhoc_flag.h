/*
 * Machinery to support adhoc_flag() capability.
 *
 * Everything hides behind one function, adhoc_flag()
 * which should be invoked only when adhoc_flag_file[s] are
 * defined.  The #define'd statement is for efficiency if not.
 */

/* struct type_param defined in param_struct.h */
extern void adhoc_flag(struct type_param *pp,
    int datum_flag, int fr, int ap, int *uflag, int *lflag);

#include "hops_config.h"
/* this should be defined in hops_config.h */
#ifndef HAVE_HSEARCH_R
#define HAVE_HSEARCH_R 0
#endif /* HAVE_HSEARCH_R */

/* disable the capability for testing */
#ifndef USE_ADHOC_FLAG
#define USE_ADHOC_FLAG HAVE_HSEARCH_R
#endif /* USE_ADHOC_FLAG */
#if USE_ADHOC_FLAG
#warning "Ad Hoc Flagging enabled"
#define ADHOC_FLAG(PP, DF, FR, AP, PU, PL) do { \
    static int once = 1;                        \
    if (once) {                                 \
        msg("AHF(%s)",1,(PP)->ah_flag_files[0]);\
        msg("AHF(%s)",1,(PP)->ah_flag_files[1]);\
        once = 0;                               \
    }                                           \
    if ((PP)->ah_flag_files[0][0] ||            \
        (PP)->ah_flag_files[1][0])              \
        adhoc_flag(PP, DF, FR, AP, PU, PL);     \
    else                                        \
        *(PU) = *(PL) = DF;                     \
    } while(0)
#else /* USE_ADHOC_FLAG */
#warning "Ad Hoc Flagging disabled"
#define ADHOC_FLAG(PP, DF, FR, AP, PU, PL) do { \
    *(PU) = *(PL) = DF; } while(0)
#endif /* USE_ADHOC_FLAG */

/* implementation details, not needed by caller */
#ifndef ADHOC_FLAG_IMPLEMENTATION
#define ADHOC_FLAG_IMPLEMENTATION 0
#endif /* ADHOC_FLAG_IMPLEMENTATION */
#if ADHOC_FLAG_IMPLEMENTATION
/* These might all be computable */
#define MAX_NUM_FLAG_FILES  256
#define MAX_LEN_FLAG_LINE   256
#define NUM_FLAG_LINES      256
#define MAX_NUM_FREQS       64
#define AHCOMMENT           '*'
typedef struct ah_ff_cache_entry {
    double tfirst, tfinal;  /* first and last times of the table */
    int ntimes;             /* number of entries in table */
    int lindex;             /* last index accessed */
    int nbytes;             /* size of char data per table line */
    double *times;          /* pointer to times in the table */
    char *table;            /* pointer to char data of table */
    char *file;             /* file providing the table */
} AHFFC_Entry;
#else /* ADHOC_FLAG_IMPLEMENTATION */
#endif /* ADHOC_FLAG_IMPLEMENTATION */

/*
 * eof
 */
