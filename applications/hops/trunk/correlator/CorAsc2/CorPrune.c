/* CorPrune.c
 *
 * Rip-off from CorAsc2 that reads and writes a file limiting
 * the number of type 120's to the number specified as an argument.
 * This exists to prune long integrations to a short starter.
 *                                       gbc 2021.8.31
 * As this is a very quick hack only minimal changes are made.
 * ----------------- start ignoring things ----------------------
 *                                       tac  2009.1.7   */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>     /* For strrchr(), etc. */
#include <math.h>       /* For atan2() and hypot() */
#include <ctype.h>      /* for isprint() */
#include "type_000.h"       /* In $INC */
#include "type_100.h"
#include "type_101.h"
#include "type_120.h"
#include "T1.h"         /* For type 141, etc. */
#include "type_200.h"
#include "type_201.h"
#include "type_202.h"
#include "type_203.h"
#include "type_204.h"
#include "type_205.h"
#include "type_206.h"
#include "type_207.h"
#include "type_208.h"
#include "type_210.h"
#include "type_212.h"
#include "type_220.h"
#include "type_221.h"
#include "type_222.h"
#include "type_230.h"
#include "type_300.h"
#include "type_301.h"
#include "type_302.h"
#include "type_303.h"
#include "type_304.h"
#include "type_306.h"
#include "type_307.h"
#include "type_308.h"
#include "type_309.h"
#include "tapeDB.h"

#define flip_short(a)   (flip ? r_short_reverse(a)  : a)
#define flip_unsig(a)   (flip ? r_unsig_reverse(a)  : a)
#define flip_int(a)     (flip ? r_int_reverse(a)    : a)
#define flip_long(a)    (flip ? r_long_reverse(a)   : a)
#define flip_float(a)   (flip ? r_float_reverse(a)  : a)
#define flip_double(a)  (flip ? r_double_reverse(a) : a)

#define MBUFF 100000        /* Length of buff[] */
#define SYSCLK 32.0e6       /* Hz */
//#define ERRMSG " ERROR: \007"
#define ERRMSG " ERROR: "
#define DBGMSG " DEBUG: "

int t2s (double arg, char *ps); /* Below */
char *me;           /* My name */
int msglev = 2;         /* For extra debuggery */
char buff[MBUFF];       /* The input buffer */
int mbuff;          /* Number of data read into buff[] */

short int word = 0x0001;
char *byte = (char*)&word;

int main (int argc, char *argv[])
    {               /* CorAsc2 */


    static char *rnames[] = {   /* Names of each record type */
    "000", "100", "101", "120", "130", "131", "141", "142", "143", "144",
    "150", "200", "201", "202", "203", "204", "205", "206", "207", "208",
    "210", "212", "220", "221", "222", "230",
    "300", "301", "302", "303", "304", "306", "307", "308", "309"
    };
                                    // change NNAMES to match above array
    #define NNAMES 35
    int countt[NNAMES];                 /* Count number of times each record was found */

    char flip = *byte;

    int i, j, k, len, n, nt;
    int nbuff, nlags, nap, tread, tlast;
    int t222_sspad, t222_cfpad;
    double a_sin, a_cos, a_amp, a_ph, b_sin, b_cos, b_amp, b_ph, tval;
    char line[80];          /* Scratch */
    T1_RecHdr *t1 = (T1_RecHdr *) buff;
    struct type_000 *t000 = (struct type_000 *) buff;
    struct type_100 *t100 = (struct type_100 *) buff;
    struct type_101 *t101 = (struct type_101 *) buff;
    struct type_120 *t120 = (struct type_120 *) buff;
    struct counts_per_lag *pcpl = t120->ld.cpl;
    struct counts_global *pcg = &t120->ld.cg;
    struct auto_per_lag *papl = t120->ld.apl;
    struct auto_global *pag = &t120->ld.ag;
    struct spectral *psp = t120->ld.spec;
    T1_R130 *t130 = (T1_R130 *) buff;
    T1_R131 *t131 = (T1_R131 *) buff;
    T1_R141 *t141 = (T1_R141 *) buff;
    T1_R142 *t142 = (T1_R142 *) buff;
    T1_R143 *t143 = (T1_R143 *) buff;
    T1_R144 *t144 = (T1_R144 *) buff;
    T1_R150 *t150 = (T1_R150 *) buff;
    struct type_200 *t200 = (struct type_200 *) buff;
    struct type_201 *t201 = (struct type_201 *) buff;
    struct type_202 *t202 = (struct type_202 *) buff;
    struct type_203 *t203 = (struct type_203 *) buff;
    /* type 203_v0 */
    /* type 203_v1 */
    struct ch_struct *pchs = t203->channels;
    struct type_204 *t204 = (struct type_204 *) buff;
    struct type_205 *t205 = (struct type_205 *) buff;
    /* type_205_v0 */
    /* type_205_v1 */
    struct type_206_v0 *t206_0 = (struct type_206_v0 *) buff;
    struct type_206_v1 *t206_1 = (struct type_206_v1 *) buff;
    struct type_206_v2 *t206_2 = (struct type_206_v2 *) buff;
    /* !!! type_206_v2 */
    struct sidebands *pr_0[] = { t206_0->accepted,
    t206_0->reason1, t206_0->reason2, t206_0->reason3, t206_0->reason4,
    t206_0->reason5, t206_0->reason6, t206_0->reason7, t206_0->reason8
    };
    struct sidebands *pr_1[] = { t206_1->accepted,
    t206_1->reason1, t206_1->reason2, t206_1->reason3, t206_1->reason4,
    t206_1->reason5, t206_1->reason6, t206_1->reason7, t206_1->reason8
    };
    struct sidebands *pr_2[] = { t206_2->accepted,
    t206_2->reason1, t206_2->reason2, t206_2->reason3, t206_2->reason4,
    t206_2->reason5, t206_2->reason6, t206_2->reason7, t206_2->reason8
    };
    struct type_207_v0 *t207_0 = (struct type_207_v0 *) buff;
    struct type_207_v1 *t207_1 = (struct type_207_v1 *) buff;
    struct type_207_v2 *t207_2 = (struct type_207_v2 *) buff;
    /* !!! type_207_v2 */
    struct sbandf *psbf_0[] = {
    t207_0->ref_pcamp, t207_0->rem_pcamp,
    t207_0->ref_pcphase, t207_0->rem_pcphase,
    t207_0->ref_pcfreq, t207_0->rem_pcfreq
    };
    struct sbandf *psbf_1[] = {
    t207_1->ref_pcamp, t207_1->rem_pcamp,
    t207_1->ref_pcphase, t207_1->rem_pcphase,
    t207_1->ref_pcoffset, t207_1->rem_pcoffset,
    t207_1->ref_pcfreq, t207_1->rem_pcfreq
    };
    struct sbandf *psbf_2[] = {
    t207_2->ref_pcamp, t207_2->rem_pcamp,
    t207_2->ref_pcphase, t207_2->rem_pcphase,
    t207_2->ref_pcoffset, t207_2->rem_pcoffset,
    t207_2->ref_pcfreq, t207_2->rem_pcfreq
    };
    char *pcname_0[] = {
    "ref_pcamp[]", "rem_pcamp[]",
    "ref_pcphase[]", "rem_pcphase[]",
    "ref_pcfreq[]", "rem_pcfreq[]"
    };
    char *pcname_1[] = {
    "ref_pcamp[]", "rem_pcamp[]",
    "ref_pcphase[]", "rem_pcphase[]",
    "ref_pcoffset[]", "rem_pcoffset[]",
    "ref_pcfreq[]", "rem_pcfreq[]"
    };
    char *pcname_2[] = {
    "ref_pcamp[]", "rem_pcamp[]",
    "ref_pcphase[]", "rem_pcphase[]",
    "ref_pcoffset[]", "rem_pcoffset[]",
    "ref_pcfreq[]", "rem_pcfreq[]"
    };
    struct type_208_v0 *t208 = (struct type_208_v0 *) buff;
    struct type_208_v1 *t208_1 = (struct type_208_v1 *) buff;
    struct type_210 *t210 = (struct type_210 *) buff;
    /* !!! type_210_v0 */
    /* !!! type_210_v1 */
    struct type_212_v0 *t212 = (struct type_212_v0 *) buff;
    struct type_212_v1 *t212_1 = (struct type_212_v1 *) buff;
    struct type_220 *t220 = (struct type_220 *) buff;   /* ?? */
    struct type_221 *t221 = (struct type_221 *) buff;
    struct type_222 *t222 = (struct type_222 *) buff;
    struct type_230 *t230 = (struct type_230 *) buff;
    struct type_300 *t300 = (struct type_300 *) buff;
    struct type_301 *t301 = (struct type_301 *) buff;
    struct type_302 *t302 = (struct type_302 *) buff;
    struct type_303 *t303 = (struct type_303 *) buff;
    struct type_304 *t304 = (struct type_304 *) buff;
    struct type_306 *t306 = (struct type_306 *) buff;
    struct type_307 *t307 = (struct type_307 *) buff;
    struct type_308 *t308 = (struct type_308 *) buff;
    struct type_309_v0 *t309_v0 = (struct type_309_v0 *) buff;
    struct type_309 *t309 = (struct type_309 *) buff;
    struct tapeDB *ttdb = (struct tapeDB *) buff;
                                    // prototypes
        short r_short_reverse (short);
        unsigned short r_unsig_reverse (unsigned short);
        int r_int_reverse (int);
        long r_long_reverse (long);
        float r_float_reverse (float);
        double r_double_reverse (double);

    int maxt120;
    int isat120;
    int wcount;

      /* *** Initialize *** */
    tread = 0;
    me = (me = strrchr (argv[0], '/')) == NULL ? argv[0] : me + 1;  /* My name */
    for (i = 0; i < NNAMES; i++)    /* Each rname[] */
        countt[i] = 0;          /* Initialize count */
      /* ** Command line OK? ** */
    if (argc != 2)
        {
        (void) fprintf (stderr,
            "%s Typical usage:\n"
            "    %s  xxx [...] <  corrfileinput  > corfileoutput\n"
            "    where xxx is the number of type 120s to retain\n"
            , me, me);
        return (1);         /* Unsatisfactory return */
        }
    maxt120 = atoi(argv[1]);
    (void) fprintf (stderr, "Truncating input to %d t120 records\n", maxt120);
      /* *** Loop until break *** */
    while (1)
        {
        isat120 = 0;
        (void) fflush (stdout); /* Necessary? */
        tlast = tread;
        /* ** First read just header ** */
        nbuff = sizeof (T1_RecHdr);
        if ((k = fread ((void *) buff, 1, nbuff, stdin)) != nbuff)
            {           /* Errors */
            if (!feof(stdin)) (void) fprintf (stderr,
                "%s%s fread() on stdin returned %d at %dB\n",
                me, ERRMSG, k, tlast);
            break;
            }           /* Presumably end of file */
        mbuff = k;          /* Bytes read (more later) */
        tread += k;
        if (msglev < 1)     /* Debuggery? */
            (void) fprintf (stderr,
                "%s%s Read header type %.3s at %dB\n",
                me, DBGMSG, t1->recId, tlast);
        /* ** Type-000 record? ** */
        if (strncmp (t1->recId, "000", 3) == 0)
            {
            /* Calculate the length of the additional read */
            nbuff = sizeof (struct type_000) - k;
            countt[0]++;        /* Increment record count */
            }
        /* ** Type-100 record? ** */
        else if (strncmp (t1->recId, "100", 3) == 0)
            {
            nbuff = sizeof (struct type_100) - k;
            countt[1]++;        /* Increment record count */
            }
        /* ** Type-101 record? ** */
        else if (strncmp (t1->recId, "101", 3) == 0)
            {
            /* Special case variable-length record */
            nbuff = sizeof (struct type_101) - k;
            n = flip_short(t101->nblocks);
            nbuff += (n - 1) * sizeof (int);
            countt[2]++;        /* Increment record count */
            }
        /* ** Type-120 record? ** */
        else if (strncmp (t1->recId, "120", 3) == 0)
            {
            isat120 = 1;
            /* * Yes.  Calculate the length of the additional read * */
            /* Special case variable-length record */
            nbuff = sizeof (struct type_120) - sizeof (union lag_data) - k;
            nlags = flip_short(t120->nlags);
            /* (We've already read enough of a 120 to see type and nlags) */
            if (t120->type == COUNTS_PER_LAG)   /* 1 */
                nbuff += nlags * sizeof (struct counts_per_lag);
            else if (t120->type == COUNTS_GLOBAL)   /* 2 */
                nbuff += sizeof (struct counts_global) +
                (nlags - 1) * sizeof (struct lag_tag);
            else if (t120->type == AUTO_GLOBAL) /* 3 */
                nbuff += sizeof (struct auto_global) + (nlags - 1) * sizeof (int);
            else if (t120->type == AUTO_PER_LAG)    /* 4 */
                nbuff += nlags * sizeof (struct auto_per_lag);
            else if (t120->type == SPECTRAL)    /* 5 */
                nbuff += nlags * sizeof (struct spectral);
            else
                {       /* No other type-120 types */
                (void) fprintf (stderr,
                    "%s%s type-120 %d at %dB?\n",
                    me, ERRMSG, t120->type, tlast);
                return (-20);   /* Error */
                }
            countt[3]++;        /* Increment record count */
            }
        /* ** Type-130 record? ** */
        else if (strncmp (t1->recId, "130", 3) == 0)
            {
            nbuff = sizeof (T1_R130) - k;
            countt[4]++;        /* Increment record count */
            }
        /* ** Type-131 record? ** */
        else if (strncmp (t1->recId, "131", 3) == 0)
            {
            nbuff = sizeof (T1_R131) - k;
            countt[5]++;        /* Increment record count */
            }
        /* ** Type-141 record? ** */
        else if (strncmp (t1->recId, "141", 3) == 0)
            {
            nbuff = sizeof (T1_R141) - k;
            countt[6]++;        /* Increment record count */
            }
        /* ** Type-142 record? ** */
        else if (strncmp (t1->recId, "142", 3) == 0)
            {
            nbuff = sizeof (T1_R142) - k;
            countt[7]++;        /* Increment record count */
            }
        /* ** Type-143 record? ** */
        else if (strncmp (t1->recId, "143", 3) == 0)
            {
            nbuff = sizeof (T1_R143) - k;
            countt[8]++;        /* Increment record count */
            }
        /* ** Type-144 record? ** */
        else if (strncmp (t1->recId, "144", 3) == 0)
            {
            nbuff = sizeof (T1_R144) - k;
            countt[9]++;        /* Increment record count */
            }
        /* ** Type-150 record? ** */
        else if (strncmp (t1->recId, "150", 3) == 0)
            {
            nbuff = sizeof (T1_R150) - k;
            countt[10]++;       /* Increment record count */
            }
        /* ** Type-200 record? ** */
        else if (strncmp (t1->recId, "200", 3) == 0)
            {
            nbuff = sizeof (struct type_200) - k;
            countt[11]++;       /* Increment record count */
            }
        /* ** Type-201 record? ** */
        else if (strncmp (t1->recId, "201", 3) == 0)
            {
            nbuff = sizeof (struct type_201) - k;
            countt[12]++;       /* Increment record count */
            }
        /* ** Type-202 record? ** */
        else if (strncmp (t1->recId, "202", 3) == 0)
            {
            nbuff = sizeof (struct type_202) - k;
            countt[13]++;       /* Increment record count */
            }
        /* ** Type-203 record? ** */
        else if (strncmp (t1->recId, "203", 3) == 0)
            {
            nbuff = (strncmp (t203->version_no, "00", 2) == 0)
                  ? sizeof (struct type_203_v0) - k
                  : sizeof (struct type_203_v1) - k;
            countt[14]++;       /* Increment record count */
            }
        /* ** Type-204 record? ** */
        else if (strncmp (t1->recId, "204", 3) == 0)
            {
            nbuff = sizeof (struct type_204) - k;
            countt[15]++;       /* Increment record count */
            }
        /* ** Type-205 record? ** */
        else if (strncmp (t1->recId, "205", 3) == 0)
            {
            nbuff = (strncmp (t205->version_no, "00", 2) == 0)
                  ? sizeof (struct type_205_v0) - k
                  : sizeof (struct type_205_v1) - k;
            countt[16]++;       /* Increment record count */
            }
        /* ** Type-206 record? ** */
        else if (strncmp (t1->recId, "206", 3) == 0)
            {
            if (strncmp (t1->recVer, "00", 2) == 0)
                nbuff = sizeof (struct type_206_v0) - k;
            else if (strncmp (t1->recVer, "01", 2) == 0)
                nbuff = sizeof (struct type_206_v1) - k;
            else            /* Assume version 02 */
                nbuff = sizeof (struct type_206_v2) - k;
            countt[17]++;       /* Increment record count */
            }
        /* ** Type-207 record? ** */
        else if (strncmp (t1->recId, "207", 3) == 0)
            {
            if (strncmp (t1->recVer, "00", 2) == 0)
                nbuff = sizeof (struct type_207_v0) - k;
            else if (strncmp (t1->recVer, "01", 2) == 0)
                nbuff = sizeof (struct type_207_v1) - k;
            else            /* Assume version 02 */
                nbuff = sizeof (struct type_207_v2) - k;
            countt[18]++;       /* Increment record count */
            }
        /* ** Type-208 record? ** */
        else if (strncmp (t1->recId, "208", 3) == 0)
        {
        if (strncmp (t1->recVer, "00", 2) == 0) /* Version 00? */
            nbuff = sizeof (struct type_208_v0) - k;
        else            /* Assume version 01 */
            nbuff = sizeof (struct type_208_v1) - k;
        countt[19]++;       /* Increment record count */
        }
        /* ** Type-210 record? ** */
        else if (strncmp (t1->recId, "210", 3) == 0)
            {
            nbuff = sizeof (struct type_210) - k;
            countt[20]++;       /* Increment record count */
            }
        /* ** Type-212 record? ** */
        else if (strncmp (t1->recId, "212", 3) == 0)
            {
            /* Special case variable-length record and two versions */
            if (strncmp (t1->recVer, "00", 2) == 0) /* Version 00? */
                nbuff = sizeof (struct type_212_v0) +   /* Yes */
                (flip_short(t212->nap) - 1) * sizeof (struct phasor) - k;
            else
                {           /* Assume version 01 */
                nap = flip_short(t212->nap) + flip_short(t212->nap) % 2;    /* CJL says padded */
                nbuff = sizeof (struct type_212_v1) +
                (nap - 1) * sizeof (struct newphasor) - k;
                }
            countt[21]++;       /* Increment record count */
            }
        /* ** Type-220 record? ** */
        else if (strncmp (t1->recId, "220", 3) == 0)
            {
            /* Special case variable-length record */
            len = (int) flip_short(t220->width) * flip_short(t220->height); /* Size of fplot[][] */
            nbuff = sizeof (struct type_220) - sizeof (char **) + len - k;
            countt[22]++;       /* Increment record count */
            }
        /* ** Type-221 record? ** */
        else if (strncmp (t1->recId, "221", 3) == 0)
            {
            /* Special case variable-length record */
            if (msglev < 1)     /* Debuggery? */
                (void) fprintf (stderr,
                    "%s%s ps_length = %d at %dB\n",
                    me,DBGMSG,flip_int(t221->ps_length),tlast);
            nbuff = sizeof (struct type_221) - sizeof (char) + flip_int(t221->ps_length) - k;
            if (flip_short(t221->padded) && nbuff % 8 > 0)  /* Pad to 8 bytes? */
                nbuff += 8 - nbuff % 8; /* Yes */
            countt[23]++;       /* Increment record count */
            }
        /* ** Type-222 record? ** */
        else if (strncmp (t1->recId, "222", 3) == 0)
            {
            /* Special case variable-length record */
            t222_sspad = ( ( flip_int(t222->setstring_length) + 7 ) & ~7) + 8;
            t222_cfpad = ( ( flip_int(t222->cf_length) + 7 ) & ~7) + 8;
            if (msglev < 1)     /* Debuggery? */
            {
                (void) fprintf (stderr,
                    "%s%s cf_length = %d at %dB\n",
                    me,DBGMSG,flip_int(t222->cf_length),tlast);
                (void) fprintf (stderr,
                    "%s%s cf_pad = %d at %dB\n",
                    me,DBGMSG,t222_cfpad,tlast);
                (void) fprintf (stderr,
                    "%s%s set_string_length = %d at %dB\n",
                    me,DBGMSG,flip_int(t222->setstring_length),tlast);
                (void) fprintf (stderr,
                    "%s%s set_string_pad = %d at %dB\n",
                    me,DBGMSG,t222_sspad,tlast);
                (void) fprintf (stderr,
                    "%s%s cf_hash = %u at %dB\n",
                    me,DBGMSG,flip_int(t222->control_hash),tlast);
                (void) fprintf (stderr,
                    "%s%s set_string_hash = %u at %dB\n",
                    me,DBGMSG,flip_int(t222->setstring_hash),tlast);
            }
            nbuff = sizeof(struct type_222) + t222_sspad + t222_cfpad - k; 
            // if (flip_short(t222->padded) && nbuff % 8 > 0)  /* Pad to 8 bytes? */
            //     nbuff += 8 - nbuff % 8; /* Yes */
            countt[24]++;       /* Increment record count */
            }
        /* ** Type-230 record? ** */
        else if (strncmp (t1->recId, "230", 3) == 0)
            {
            /* Special case variable-length record */
            nbuff = sizeof (struct type_230) + (flip_short(t230->nspec_pts) - 1) * sizeof (complex) - k;
            countt[25]++;       /* Increment record count */
            }
        /* ** Type-300 record? ** */
        else if (strncmp (t1->recId, "300", 3) == 0)
            {
            nbuff = sizeof (struct type_300) - k;
            countt[26]++;       /* Increment record count */
            }
        /* ** Type-301 record? ** */
        else if (strncmp (t1->recId, "301", 3) == 0)
            {
            nbuff = sizeof (struct type_301) - k;
            countt[27]++;       /* Increment record count */
            }
        /* ** Type-302 record? ** */
        else if (strncmp (t1->recId, "302", 3) == 0)
            {
            nbuff = sizeof (struct type_302) - k;
            countt[28]++;       /* Increment record count */
            }
        /* ** Type-303 record? ** */
        else if (strncmp (t1->recId, "303", 3) == 0)
            {
            nbuff = sizeof (struct type_303) - k;
            countt[29]++;       /* Increment record count */
            }
        /* ** Type-304 record? ** */
        else if (strncmp (t1->recId, "304", 3) == 0)
            {
            nbuff = sizeof (struct type_304) - k;
            countt[30]++;       /* Increment record count */
            }
        /* ** Type-306 record? ** */
        else if (strncmp (t1->recId, "306", 3) == 0)
            {
            nbuff = sizeof (struct type_306) - k;
            countt[31]++;       /* Increment record count */
            }
        /* ** Type-307 record? ** */
        else if (strncmp (t1->recId, "307", 3) == 0)
            {
            nbuff = sizeof (struct type_307) - k;
            countt[32]++;       /* Increment record count */
            }
        /* ** Type-308 record? ** */
        else if (strncmp (t1->recId, "308", 3) == 0)
            {
            nbuff = sizeof (struct type_308) - k;
            countt[33]++;       /* Increment record count */
            }
        /* ** Type-309 record? ** */
        else if (strncmp (t1->recId, "309", 3) == 0)
            {
            nbuff = (strncmp (t309->version_no, "00", 2) == 0)
                  ? sizeof (struct type_309_v0) - k
                  : sizeof (struct type_309_v1) - k;
            countt[34]++;       /* Increment record count */
            }
        /* ** tapeDB record? ** */
        else if (strncasecmp (t1->recId, "TDB", 3) == 0)
            nbuff = sizeof (struct tapeDB) - k;
        /* ** We can't yet handle any other record types ** */
        else
            {           /* Unknown recId */
            (void) fprintf (stderr,
                "%s%s Unknown record type \"%.3s\" at %dB\n",
                me, ERRMSG, t1->recId, tlast);
            return (-21);       /* Error */
            }
        /* ** Check:  Is buff[] big enough? ** */
        if (mbuff + nbuff > MBUFF)
            {           /* Would buffer overflow? */
            (void) fprintf (stderr,
                "%s%s Buffer overflow record type"
                " %.3s  mbuff+nbuff = %d at %dB\n",
                me, ERRMSG, t1->recId, mbuff + nbuff, tlast);
            return (-22);       /* Fatal error */
            }
        /* ** Read that much more data ** */
        if ((k = fread ((void *) (buff + mbuff), 1, nbuff, stdin)) != nbuff)
            {
            (void) fprintf (stderr,
                "%s%s fread() on stdin returned %d at %dB\n",
                me, ERRMSG, k, tlast);
            break;
            }           /* Presumably end of file */
        mbuff += k;         /* OK.  Total number of bytes read */
        tread += k;
        /* ----------------------------------------------------------- */
        /* at this point instead of printing stuff, we just write it   */
        /* unless it is a t120 and we have reached the stated limit    */
        /* ----------------------------------------------------------- */
        if (isat120 == 1)
            {
            if (maxt120 == 0) continue;
            if (maxt120 == 1) (void)fprintf(stderr, "Max reached...truncating t120s here\n");
            if (maxt120 > 0) maxt120--;
            ++wcount;
            }
        /* notify user of partial writes */
        if ((k = fwrite ((void *) buff, 1, mbuff, stdout)) != mbuff)
            {
            (void) fprintf (stderr,
                "%s fwrite() partial write of %d != %d\n", k, mbuff);
            break;
            }
        }               /* End of while loop until break */
      /* Probably end of file */

      /* *** Print number of each record type that we read *** */
    for (i = 0; i < NNAMES; i++)    /* Each record type */
        if (countt[i] > 0)      /* Read any of this type? */
            (void) fprintf (stderr, " Read %5d type %s record%c \n", countt[i], rnames[i],
               countt[i] > 1 ? 's' : ' ');
    (void) fprintf (stderr, "Wrote %5d type 120 records\n%s End \n", wcount, me);
    return (0);         /* Probably normal end */
    }               /* End of main = CorAsc2 */



int t2s (           /* Convert a time to a printable string */
    /* This is NOT the same as t2s() in CJL's library */
    double arg,     /* The time in seconds to be converted (assumed +) */
    char *ps)
    {               /* Pointer to a string for the answer.  This
                     * string will be 13 bytes long (including \0) and
                     * must be dimensioned large enough in the calling
                     * program. */
      /* (t2s() is a simplified version of d2s() from, for example, Sho.c) */
    double argp;            /* For seconds roundoff */
    int dd, hh, mm, ss;     /* Days, hours, minutes, seconds  */

    if (arg < 0.0 || arg > 31622400.0)
        {               /* Range OK? */
        /* (That's the maximum number of seconds in a year) */
        if (msglev < 1)     /* Nope.  Debuggery? */
            (void) fprintf (stderr,
                "%s%s t2s() %.3e seconds is illegal\n",
                me, ERRMSG, arg);
        (void) strcpy (ps, " ERROR! ");
        return (-1);        /* Error */
        }
    argp = arg + 0.5;       /* Round off; assumed arg positive */
    dd = argp / 86400.0;        /* Days */
    hh = (argp - dd * 86400.0) / 3600.0;    /* Hours */
    mm = (argp - dd * 86400.0 - hh * 3600.0) / 60.0;    /* Minutes */
    ss = argp - dd * 86400.0 - hh * 3600.0 - mm * 60.0 + 0.5;   /* Seconds */
    (void) sprintf (ps, "%03d-%02d:%02d:%02d", 1 + dd, hh, mm, ss);
    return (0);         /* Normal end */
    }               /* End of t2s() */

/* swabr.c - byte reversing procedures to be used for conversion
 * between Big-Endian and Small-Endian formats
 *
 * Created by David E. Flynt, 8/8/94
 * modified by rjc, 2005.9.2, to optimize for speed, by
 * eliminating loops. Probably should be implemented as macro's someday.
 *
 * modified by tac, 2009.1.7, from swab.c, to return values instead of
 * altering in place.
 */

/* short_reverse: reverse the order of bytes in a short integer */
short r_short_reverse (short i)
    {
    char *p1 = (char *)&i, *p2, temp;
    p2 = p1 + 1;

    temp = *p1;
    *p1  = *p2;
    *p2  = temp;

    return i;
    }

/* unsig_reverse: reverse the order of bytes in a unsigned short integer */
unsigned short r_unsig_reverse (unsigned short i)
    {
    char *p1 = (char *)&i, *p2, temp;
    p2 = p1 + 1;

    temp = *p1;
    *p1  = *p2;
    *p2  = temp;

    return i;
    }

/* int_reverse: reverse the order of bytes in an integer */
int r_int_reverse (int j)
    {
    char *p = (char *)&j, temp;     /* p points to the first byte of j */

    temp = p[0];
    p[0] = p[3];
    p[3] = temp;

    temp = p[1];
    p[1] = p[2];
    p[2] = temp;

    return j;
    }

/* long_reverse: reverse the order of bytes in a 64-bit integer */
long r_long_reverse (long j)
    {
    char *p = (char *)&j, temp;     /* p points to the first byte of j */

    temp = p[0];
    p[0] = p[7];
    p[7] = temp;

    temp = p[1];
    p[1] = p[6];
    p[6] = temp;

    temp = p[2];
    p[2] = p[5];
    p[5] = temp;

    temp = p[3];
    p[3] = p[4];
    p[4] = temp;

    return j;
    }

/* float_reverse: reverse the order of bytes in a float */
float r_float_reverse (float j)
    {
    char *p = (char *)&j, temp;     /* p points to the first byte of j */

    temp = p[0];
    p[0] = p[3];
    p[3] = temp;

    temp = p[1];
    p[1] = p[2];
    p[2] = temp;

    return j;
    }

/* double_reverse: reverse the order of bytes in a double */
double r_double_reverse (double j)
    {
    char *p = (char *)&j, temp;     /* p points to the first byte of j */

    temp = p[0];
    p[0] = p[7];
    p[7] = temp;

    temp = p[1];
    p[1] = p[6];
    p[6] = temp;

    temp = p[2];
    p[2] = p[5];
    p[5] = temp;

    temp = p[3];
    p[3] = p[4];
    p[4] = temp;

    return j;
    }

/*
 * eof
 */
