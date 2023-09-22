#ifndef ADHOC_FLAG_H_
#define ADHOC_FLAG_H_

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
/*#warning "Ad Hoc Flagging enabled"*/
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
/*#warning "Ad Hoc Flagging disabled"*/
#define ADHOC_FLAG(PP, DF, FR, AP, PU, PL) do { \
    *(PU) = *(PL) = DF; } while(0)
#endif /* USE_ADHOC_FLAG */

/*
 * eof
 */

#endif
