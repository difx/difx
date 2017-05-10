/* CorAsc2.c
 *
 * $Id$
 *
 * Call me CorAsc2.  I read correlator data files from stdin and
 * debug print more-or-less everything therein.  See also CorAsc.c.
 * Typical usage:
 *     CorAsc2  xxx  <  $TMP/cortest  |  more
 * where xxx is the record type to be debug printed, and $TMP/cortest
 * is an example of a correlator data file.  Replace xxx with any
 * one of:  000, 100, 101, 120, 130, 131, 141, 142, 143, 144, 150,
 * 200, 201, 202, 203, 204, 205, 206, 207, 208, 210, 212, 220, 221, 230,
 * 300, 301, 302, 303, 304, 306, 307, 308, 309, or TDB.
 * Revised:  I can now print multiple file types, that is I accept
 * multiple xxx on the command line, and I count and print the number
 * of each record type that I read.
 * I am a rough unsophisticated test program.
 * Revised:  2002 June 5, JAB
 * revise to support both big and little Endian architectures
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
int msglev = 1;         /* For extra debuggery */
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

      /* *** Initialize *** */
    tread = 0;
    me = (me = strrchr (argv[0], '/')) == NULL ? argv[0] : me + 1;  /* My name */
    for (i = 0; i < NNAMES; i++)    /* Each rname[] */
        countt[i] = 0;          /* Initialize count */
      /* ** Command line OK? ** */
    j = (argc == 1) ? 0 : 1;     // avoid segfault for 1 arg
    if (!strcmp(argv[j], "dbg")) msglev = 0;
    for (k = j; k < argc; k++)
        if (strlen (argv[k]) != 3)
        {
        (void) fprintf (stderr,
            "%s Typical usage:\n"
            "    %s  xxx [...] <  $TMP/cortest  |  more\n"
            "    where xxx is the record type to be printed\n"
            "    (that is one of 000, 100, 101, 120, 130, etc.),\n"
            "    and $TMP/cortest is an example of a correlator\n"
            "    data file to be read.  Multiple types can be\n"
            "    requested, and the first one can be \"dbg\" to\n"
            "    turn on additional debugging or \"xxx\" to just\n"
            "    get the summary of record counts.\n"
            , me, me);
        return (1);         /* Unsatisfactory return */
        }
      /* *** Loop until break *** */
    while (1)
        {
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
        if (msglev < 1)     /* Debuggery? */
            (void) fprintf (stderr,
                "%s%s Read record type %.3s at %dB\n",
                me, DBGMSG, t1->recId, tlast);
        /* *** Here we have a complete record; should we print it? *** */
        for (i = 1; i < argc; i++)  /* Each command-line argument */
            if (strncasecmp (argv[i], t1->recId, 3) == 0)   /* Print this one? */
                break;          /* Yes */
        if (i >= argc)      /* Print this one? */
            continue;           /* Nope */
        /* ** Yes print, but what kind is it? ** */
        /* * Type-000 record? * */
        if (strncmp (t1->recId, "000", 3) == 0)
            {           /* Type 000? */
            (void) printf ("%s type_000 record_id = %.3s ", me, t000->record_id);
            (void) printf (" version_no = %.2s \n", t000->version_no);
            (void) printf (" date = %.16s ", t000->date);
            (void) printf (" name = %.40s \n", t000->name);
            continue;
            }           /* End of if type 000 */
        /* ** Type-1xx records are in correlator files ** */
        /* * Type-100 record? * */
        if (strncmp (t1->recId, "100", 3) == 0)
            {           /* Type 100? */
            /* General data description */
            (void) printf ("%s type_100 record_id = %.3s ", me, t100->record_id);
            (void) printf (" version_no = %.2s \n", t100->version_no);
            (void) printf (" procdate = %d %03d %02d%02d%05.2f ",
                       flip_short(t100->procdate.year), flip_short(t100->procdate.day),
                       flip_short(t100->procdate.hour), flip_short(t100->procdate.minute),
                       flip_float(t100->procdate.second));
            (void) printf (" baseline = %.2s \n", t100->baseline);
            (void) printf (" rootname = %s ", t100->rootname);  /* Max 34 */
            (void) printf (" qcode = %.2s ", t100->qcode);
            (void) printf (" pct_done = %.2f \n", flip_float(t100->pct_done));
            (void) printf (" start = %d %03d %02d%02d%05.2f ",
                       flip_short(t100->start.year), flip_short(t100->start.day), flip_short(t100->start.hour),
                       flip_short(t100->start.minute), flip_float(t100->start.second));
            (void) printf (" stop = %d %03d %02d%02d%05.2f \n",
                       flip_short(t100->stop.year), flip_short(t100->stop.day), flip_short(t100->stop.hour),
                       flip_short(t100->stop.minute), flip_float(t100->stop.second));
            (void) printf (" ndrec = %d ", flip_int(t100->ndrec));
            (void) printf (" nindex = %d ", flip_int(t100->nindex));
            (void) printf (" nlags = %d ", flip_short(t100->nlags));
            (void) printf (" nblocks = %d \n", flip_short(t100->nblocks));
            continue;
            }           /* End of if type 100 */
        /* * Type-101 record? * */
        if (strncmp (t1->recId, "101", 3) == 0)
            {           /* Type 101? */
            /* Index number parameters */
            (void) printf ("%s type_101 record_id = %.3s ", me, t101->record_id);
            (void) printf (" version_no = %.2s \n", t101->version_no);
            (void) printf (" status = %#x ", t101->status);
            (void) printf (" nblocks = %d ", flip_short(t101->nblocks));
            (void) printf (" index = %d ", flip_short(t101->index));
            (void) printf (" primary = %d \n", flip_short(t101->primary));
            (void) printf (" ref_chan_id = %.8s ", t101->ref_chan_id);
            (void) printf (" rem_chan_id = %.8s ", t101->rem_chan_id);
            (void) printf (" corr_board = %d ", flip_short(t101->corr_board));
            (void) printf (" corr_slot = %d \n", flip_short(t101->corr_slot));
            (void) printf (" ref_chan = %d ", flip_short(t101->ref_chan));
            (void) printf (" rem_chan = %d ", flip_short(t101->rem_chan));
            (void) printf (" post_mortem = %#x \n", flip_int(t101->post_mortem));
            for (i = 0; i < flip_short(t101->nblocks); i++)
                {           /* Each block */
                (void) printf (" blocks[%d] = 0x%8.8x ", i, flip_int(t101->blocks[i]));
                if (i % 3 == 2)
                    (void) printf ("\n");
                }
            if (flip_short(t101->nblocks) % 3 != 0)
                (void) printf ("\n");
            continue;
            }           /* End of if type 101 */
        /* * Type-120 record? * */
        if (strncmp (t1->recId, "120", 3) == 0)
            {           /* Type 120? */
            /* Sorted lag data */
            (void) printf ("%s got type 120 \n", me);
            (void) printf (" record_id = %.3s ", t120->record_id);
            (void) printf ("version_no = %.2s ", t120->version_no);
            (void) printf ("type = %d \n", t120->type);
            nlags = flip_short(t120->nlags);
            (void) printf (" nlags = %d ", nlags);
            (void) printf ("baseline = %.2s ", t120->baseline);
            (void) printf ("rootcode = %.6s ", t120->rootcode);
            (void) printf ("index = %d ", flip_int(t120->index));
            (void) printf ("ap = %d \n", flip_int(t120->ap));
            (void) printf (" weight = %8.4f ", flip_float(t120->fw.weight));
            (void) printf ("status = %#8.8x ", flip_int(t120->status));
            /* (void) printf("bitshift = %f ", t120->bitshift); */
            (void) printf ("fr_delay = %d ", flip_int(t120->fr_delay));
            /* (void) printf("fbit = %f \n", t120->fbit); */
            (void) printf ("delay_rate = %d \n", flip_int(t120->delay_rate));
            if (t120->type == COUNTS_GLOBAL)
                {
                (void) printf (" cg.cosbits = 0x%8.8x ", flip_int(pcg->cosbits));
                (void) printf ("cg.sinbits = 0x%8.8x \n", flip_int(pcg->sinbits));
                }
            else if (t120->type == AUTO_GLOBAL)
                (void) printf (" ag.cosbits = 0x%8.8x \n", flip_int(pag->cosbits));
            for (j = 0; j < nlags; j++)
                {           /* Through union lag_data */
                (void) printf (" j =%3d ", j);
                if (t120->type == COUNTS_PER_LAG)
                    {
                    (void) printf (" coscor = 0x%8.8x %9d ",
                               flip_int(pcpl[j].coscor), flip_int(pcpl[j].coscor));
                    (void) printf (" cosbits = 0x%8.8x %9d \n",
                               flip_int(pcpl[j].cosbits), flip_int(pcpl[j].cosbits));
                    (void) printf ("         sincor = 0x%8.8x %9d ",
                               flip_int(pcpl[j].sincor), flip_int(pcpl[j].sincor));
                    (void) printf (" sinbits = 0x%8.8x %9d \n",
                               flip_int(pcpl[j].sinbits), flip_int(pcpl[j].sinbits));
                    }
                else if (t120->type == COUNTS_GLOBAL)
                    {
                    (void) printf (" lags[].coscor = 0x%8.8x %9d ",
                               flip_int(pcg->lags[j].coscor), flip_int(pcg->lags[j].coscor));
                    (void) printf (" lags[].sincor = 0x%8.8x %9d \n",
                               flip_int(pcg->lags[j].sincor), flip_int(pcg->lags[j].sincor));
                    }
                else if (t120->type == AUTO_PER_LAG)
                    {
                    (void) printf (" cosbits = 0x%8.8x %9d ",
                               flip_int(papl[j].cosbits), flip_int(papl[j].cosbits));
                    (void) printf (" coscor = 0x%8.8x %9d \n",
                               flip_int(papl[j].coscor), flip_int(papl[j].coscor));
                    }
                else if (t120->type == AUTO_GLOBAL)
                    (void) printf (" coscor[] = 0x%8.8x %9d \n",
                       flip_int(pag->coscor[j]), flip_int(pag->coscor[j]));
                else if (t120->type == SPECTRAL)
                    {
                    (void) printf (" real %9.6f  imag %9.6f\n",
                               flip_float (psp[j].re), flip_float (psp[j].im));
                    }
                }           /* End of for j lags */
            continue;
            }           /* End of if type 120 */
            /* * Heading of type 13x, 14x, or 15x? * */
        if (strncmp (t1->recId, "13", 2) == 0 ||    /* Type 13x */
          strncmp (t1->recId, "14", 2) == 0 ||  /* or type 14x */
          strncmp (t1->recId, "15", 2) == 0)
            {           /* or type 15x? */
            /* T1 Record Header */
            // (void) printf ("%s got type %.3s \n", me, t1->recId);
            (void) printf (" recId = %.3s ", t1->recId);
            (void) printf (" recVer = %.2s ", t1->recVer);
            (void) printf (" recFmt = %#x ", t1->recFmt);
            (void) printf (" cFOffset = %d \n", flip_short(t1->cFOffset));
            /* (Or number of correlator frames for type 130) */
            (void) printf (" baseline = %.2s ", t1->baseline);
            (void) printf (" rootcode = %.6s ", t1->rootcode);
            (void) printf (" index = %d ", flip_int(t1->index));
            (void) printf (" ap = %d \n", flip_int(t1->ap));
            }           /* End of if type 13x, 14x, or 15x */
        /* * Type-130 record? * */
        if (strncmp (t1->recId, "130", 3) == 0)
            {           /* Type 130? */
            /* AP error counts */
            (void) printf (" enabled = %#x ", flip_int(t130->enabled));
            (void) printf (" occurred = %#x \n", flip_int(t130->occurred));
            (void) printf (" xSuId = %d ", flip_int(t130->xSuId));
            (void) printf (" xChnId = %d ", flip_int(t130->xChnId));
            (void) printf (" xCFNum = %d ", flip_int(t130->xCFNum));
            (void) printf (" xChkSum = %d ", flip_int(t130->xChkSum));
            (void) printf (" xInBdLink = %d ", flip_int(t130->xInBdLink));
            (void) printf (" xInBdSync = %d \n", flip_int(t130->xInBdSync));
            (void) printf (" ySuId = %d ", flip_int(t130->ySuId));
            (void) printf (" yChnId = %d ", flip_int(t130->yChnId));
            (void) printf (" yCFNum = %d ", flip_int(t130->yCFNum));
            (void) printf (" yChkSum = %d ", flip_int(t130->yChkSum));
            (void) printf (" yInBdLink = %d ", flip_int(t130->yInBdLink));
            (void) printf (" yInBdSync = %d \n", flip_int(t130->yInBdSync));
            (void) printf (" headTapPastEnd = %d ", flip_int(t130->headTapPastEnd));
            (void) printf (" headTap3Carry = %d ", flip_int(t130->headTap3Carry));
            (void) printf (" tailTapPastEnd = %d ", flip_int(t130->tailTapPastEnd));
            (void) printf (" tailTap3Carry = %d \n", flip_int(t130->tailTap3Carry));
            continue;
            }           /* End of if type 130 */
        /* * Type-131 record? * */
        if (strncmp (t1->recId, "131", 3) == 0)
            {           /* Type 131? */
            /* Link Status */
            for (j = 0; j < 64; j++)
                {           /* Each linkStatus[] */
                (void) printf (" %#9x ", flip_int(t131->linkStatus[j]));
                if (j % 4 == 3)
                    (void) printf (" \n");
                }
            }           /* End of if type 131 */
        /* * Type-141 record? * */
        if (strncmp (t1->recId, "141", 3) == 0)
            {           /* Type 141? */
            /* Correlator Frame Header */
            (void) printf (" suId = %d ", flip_int(t141->suId));
            (void) printf (" chnId = %d ", flip_int(t141->chnId));
            (void) printf (" cFNum = %#x ", flip_int(t141->cFNum));
            (void) printf (" dlyErr = %#x \n", flip_int(t141->dlyErr));
            (void) printf (" dlyErrRate = %#x ", flip_int(t141->dlyErrRate));
            (void) printf (" phase = %#x ", flip_int(t141->phase));
            (void) printf (" phaseRate = %#x \n", flip_int(t141->phaseRate));
            (void) printf (" phaseAcc = %#x ", flip_int(t141->phaseAcc));
            (void) printf (" phaseLogIncPeriod = %d ", flip_int(t141->phaseLogIncPeriod));
            (void) printf (" phaseKAccSegLen = %#x \n", flip_int(t141->phaseKAccSegLen));
            (void) printf (" sideband = %d ", flip_int(t141->sideband));
            (void) printf (" oversamplingFactor = %d ", flip_int(t141->oversamplingFactor));
            (void) printf (" chkSum = %#x ", flip_int(t141->chkSum));
            (void) printf (" flags = %#8.8x \n", flip_int(t141->flags));
            continue;
            }           /* End of if type 141 */
        /* * Type-142 record? * */
        if (strncmp (t1->recId, "142", 3) == 0)
            {           /* Type 142? */
            /* Correlator Frame Dynamic Parameters */
            (void) printf (" phaseAdj = %#x ", flip_int(t142->phaseAdj));
            (void) printf (" phaseIncClkDiv = %#x ", flip_int(t142->phaseIncClkDiv));
            (void) printf (" phaseRateIncCnt = %#x \n", flip_int(t142->phaseRateIncCnt));
            (void) printf (" phase = %#x ", flip_int(t142->phase));
            (void) printf (" phaseRate = %#x ", flip_int(t142->phaseRate));
            (void) printf (" phaseAcc = %#x \n", flip_int(t142->phaseAcc));
            (void) printf (" xDly = %#x ", flip_int(t142->xDly));
            (void) printf (" xDlyRate = %#x ", flip_int(t142->xDlyRate));
            (void) printf (" yDly = %#x ", flip_int(t142->yDly));
            (void) printf (" yDlyRate = %#x \n", flip_int(t142->yDlyRate));
            (void) printf (" bDly = %#x ", flip_int(t142->bDly));
            (void) printf (" bDlyRate = %#x ", flip_int(t142->bDlyRate));
            (void) printf (" tapPos = %d \n", flip_int(t142->tapPos));
            (void) printf (" xDlyRateSign = %d ", flip_int(t142->xDlyRateSign));
            (void) printf (" yDlyRateSign = %d ", flip_int(t142->yDlyRateSign));
            (void) printf (" bDlyRateSign = %d ", flip_int(t142->bDlyRateSign));
            (void) printf (" udr = %d \n", flip_int(t142->udr));
            continue;
            }           /* End of if type 142 */
        /* * Type-143 record? * */
        if (strncmp (t1->recId, "143", 3) == 0)
            {           /* Type 143? */
            /* Correlator Frame Dynamic Parameter Residues */
            (void) printf (" phaseAdj = %#x ", flip_int(t143->phaseAdj));
            (void) printf (" phaseIncClkDiv = %#x ", flip_int(t143->phaseIncClkDiv));
            (void) printf (" phaseRateIncCntFinal = %#x \n",
                       flip_int(t143->phaseRateIncCntFinal));
            (void) printf (" phaseRateFinal = %#x ", flip_int(t143->phaseRateFinal));
            (void) printf (" phaseFinal = %#x ", flip_int(t143->phaseFinal));
            (void) printf (" phaseInitial = %#x \n", flip_int(t143->phaseInitial));
            (void) printf (" xDlyFinal = %#x ", flip_int(t143->xDlyFinal));
            (void) printf (" xDlyInitial = %#x ", flip_int(t143->xDlyInitial));
            (void) printf (" yDlyFinal = %#x ", flip_int(t143->yDlyFinal));
            (void) printf (" yDlyInitial = %#x \n", flip_int(t143->yDlyInitial));
            (void) printf (" bDlyFinal = %#x ", flip_int(t143->bDlyFinal));
            (void) printf (" bDlyInitial = %#x ", flip_int(t143->bDlyInitial));
            (void) printf (" tapPosFinal = %d ", flip_int(t143->tapPosFinal));
            (void) printf (" tapErr = %d ", flip_int(t143->tapErr));
            (void) printf (" udr = %d \n", flip_int(t143->udr));
            continue;
            }           /* End of if type 143 */
        /* * Type-144 record? * */
        if (strncmp (t1->recId, "144", 3) == 0)
            {           /* Type 144? */
            /* Correlator Frame Channel Errors */
            (void) printf (" suIdEx = %#x ", flip_int(t144->suIdEx));
            (void) printf (" suIdRx = %#x ", flip_int(t144->suIdRx));
            (void) printf (" chnIdEx = %#x ", flip_int(t144->chnIdEx));
            (void) printf (" chnIdRx = %#x ", flip_int(t144->chnIdRx));
            (void) printf (" cFNumEx = %#x ", flip_int(t144->cFNumEx));
            (void) printf (" cFNumRx = %#x ", flip_int(t144->cFNumRx));
            (void) printf (" chkSumEx = %#x ", flip_int(t144->chkSumEx));
            (void) printf (" chkSumRx = %#x \n", flip_int(t144->chkSumRx));
            continue;
            }           /* End of if type 144 */
        /* * Type-150 record? * */
        if (strncmp (t1->recId, "150", 3) == 0)
            {           /* Type 150? */
            /* Correlator End Task */
            (void) printf (" qcode[] = %d %d \n", t150->qcode[0], t150->qcode[1]);
            /* ?? To be revised ?? */
            continue;
            }           /* End of if type 150 */
        /* * Header of type-2xx or type-3xx records? * */
        if (strncmp (t1->recId, "2", 1) == 0 || /* Type 2xx */
          strncmp (t1->recId, "3", 1) == 0)
            {           /* or type 3xx? */
            /* Type-2xx or type-3xx record header */
            // (void) printf ("%s got type %.3s \n", me, t1->recId);
            (void) printf (" record_id = %.3s ", t1->recId);
            (void) printf (" version_no = %.2s ", t1->recVer);
            }           /* End of if type 2xx or 3xx */
        /* ** Type-2xx records are in fringe files ** */
        /* * Type-200 record? * */
        if (strncmp (t1->recId, "200", 3) == 0)
            {           /* Type 200? */
            /* General information */
            (void) printf (" sortware_rev = ");
            for (i = 0; i < 10; i++)    /* Each entry */
                (void) printf ("%d ", flip_short(t200->software_rev[i]));
            (void) printf (" \n");
            (void) printf (" expt_no = %d ", flip_int(t200->expt_no));
            (void) printf (" exper_name = %s ", t200->exper_name);
            (void) printf (" scan_name = %s ", t200->scan_name);
            (void) printf (" correlator = %s \n", t200->correlator);
            (void) printf (" scantime = %d %03d %02d%02d%05.2f ",
                       flip_short(t200->scantime.year), flip_short(t200->scantime.day),
                       flip_short(t200->scantime.hour), flip_short(t200->scantime.minute),
                       flip_float(t200->scantime.second));
            (void) printf (" start_offset = %d ", flip_int(t200->start_offset));
            (void) printf (" stop_offset = %d \n", flip_int(t200->stop_offset));
            (void) printf (" corr_date = %d %03d %02d%02d%05.2f ",
                       flip_short(t200->corr_date.year), flip_short(t200->corr_date.day),
                       flip_short(t200->corr_date.hour), flip_short(t200->corr_date.minute),
                       flip_float(t200->corr_date.second));
            (void) printf (" fourfit_date = %d %03d %02d%02d%05.2f \n",
                       flip_short(t200->fourfit_date.year), flip_short(t200->fourfit_date.day),
                       flip_short(t200->fourfit_date.hour), flip_short(t200->fourfit_date.minute),
                       flip_float(t200->fourfit_date.second));
            (void) printf (" frt = %d %03d %02d%02d%05.2f \n", flip_short(t200->frt.year),
                       flip_short(t200->frt.day), flip_short(t200->frt.hour), flip_short(t200->frt.minute),
                       flip_float(t200->frt.second));
            continue;
            }           /* End of if type 200 */
        /* * Type-201 record? * */
        if (strncmp (t1->recId, "201", 3) == 0)
            {           /* Type 201? */
            /* Source information */
            (void) printf (" source = %s \n", t201->source);
            (void) printf (" coord = %02d:%02d:%05.2f %02d:%02d:%04.1f ",
                       flip_short(t201->coord.ra_hrs), flip_short(t201->coord.ra_mins),
                       flip_float(t201->coord.ra_secs), flip_short(t201->coord.dec_degs),
                       flip_short(t201->coord.dec_mins), flip_float(t201->coord.dec_secs));
            (void) printf (" epoch = %d \n", flip_short(t201->epoch));
            (void) printf (" coord_date = %d %03d %02d%02d%05.2f ",
                       flip_short(t201->coord_date.year), flip_short(t201->coord_date.day),
                       flip_short(t201->coord_date.hour), flip_short(t201->coord_date.minute),
                       flip_float(t201->coord_date.second));
            (void) printf (" rarate = %.3e ", flip_double(t201->ra_rate));
            (void) printf (" dec_rate = %.3e \n", flip_double(t201->dec_rate));
            (void) printf (" pulsar_phase = ");
            for (i = 0; i < 4; i++) /* Each polynomial coefficient */
                (void) printf ("%.3e ", flip_double(t201->pulsar_phase[i]));
            (void) printf (" \n");
            (void) printf (" pulsar_epoch = %.7e ", flip_double(t201->pulsar_epoch));
            (void) printf (" dispersion = %.3e \n", flip_double(t201->dispersion));
            continue;
            }           /* End of if type 201 */
        /* * Type-202 record? * */
        if (strncmp (t1->recId, "202", 3) == 0)
            {           /* Type 202? */
            /* Baseline information */
            (void) printf (" baseline = %.2s \n", t202->baseline);
            (void) printf (" ref_intl_id = %.2s ", t202->ref_intl_id);
            (void) printf (" rem_intl_id = %.2s \n", t202->rem_intl_id);
            (void) printf (" ref_name = %.8s ", t202->ref_name);
            (void) printf (" rem_name = %.8s \n", t202->rem_name);
            (void) printf (" ref_tape = %.8s ", t202->ref_tape);
            (void) printf (" rem_tape = %.8s ", t202->rem_tape);
            (void) printf (" nlags = %d \n", flip_short(t202->nlags));
            (void) printf (" ref_xpos = %.3f ", flip_double(t202->ref_xpos));
            (void) printf (" rem_xpos = %.3f \n", flip_double(t202->rem_xpos));
            (void) printf (" ref_ypos = %.3f ", flip_double(t202->ref_ypos));
            (void) printf (" rem_ypos = %.3f \n", flip_double(t202->rem_ypos));
            (void) printf (" ref_zpos = %.3f ", flip_double(t202->ref_zpos));
            (void) printf (" rem_zpos = %.3f \n", flip_double(t202->rem_zpos));
            (void) printf (" u = %.4e ", flip_double(t202->u));
            (void) printf (" v = %.4e ", flip_double(t202->v));
            (void) printf (" uf = %.4e ", flip_double(t202->uf));
            (void) printf (" vf = %.4e \n", flip_double(t202->vf));
            (void) printf (" ref_clock = %.3f ", flip_float(t202->ref_clock));
            (void) printf (" rem_clock = %.3f \n", flip_float(t202->rem_clock));
            (void) printf (" ref_clockrate = %.3e ", flip_float(t202->ref_clockrate));
            (void) printf (" rem_clockrate = %.3e \n", flip_float(t202->rem_clockrate));
            (void) printf (" ref_idelay = %.3e ", flip_float(t202->ref_idelay));
            (void) printf (" rem_idelay = %.3e \n", flip_float(t202->rem_idelay));
            (void) printf (" ref_zdelay = %.3e ", flip_float(t202->ref_zdelay));
            (void) printf (" rem_zdelay = %.3e \n", flip_float(t202->rem_zdelay));
            (void) printf (" ref_elev = %.3f ", flip_float(t202->ref_elev));
            (void) printf (" rem_elev = %.3f ", flip_float(t202->rem_elev));
            (void) printf (" ref_az = %.3f ", flip_float(t202->ref_az));
            (void) printf (" rem_az = %.3f \n", flip_float(t202->rem_az));
            continue;
            }           /* End of if type 202 */
        /* * Type-203 record? * */
        if (strncmp (t1->recId, "203", 3) == 0)
            {           /* Type 203? */
            /* Channel information */
            (void) printf (" \n");
            for (i = 0; i < 32; i++)
                {           /* Each channel */
                if (flip_short(pchs[i].index) < 0)  /* Skip not-in-use channels */
                    continue;
                (void) printf (" index = %d ", flip_short(pchs[i].index));
                (void) printf (" sample_rate = %d ", flip_short(pchs[i].sample_rate));
                (void) printf (" refsb = %c ", pchs[i].refsb);
                (void) printf (" remsb = %c ", pchs[i].remsb);
                (void) printf (" refpol = %c ", pchs[i].refpol);
                (void) printf (" rempol = %c \n", pchs[i].rempol);
                (void) printf ("   ref_freq = %.12e ", flip_double(pchs[i].ref_freq));
                (void) printf (" rem_freq = %.12e \n", flip_double(pchs[i].rem_freq));
                (void) printf ("   ref_chan_id = %.8s ", pchs[i].ref_chan_id);
                (void) printf (" rem_chan_id = %.8s \n", pchs[i].rem_chan_id);
                }
            continue;
            }           /* End of if type 203 */
        /* * Type-204 record? * */
        if (strncmp (t1->recId, "204", 3) == 0)
            {           /* Type 204? */
            /* Execution setup */
            (void) printf (" ff_version = ");
            for (i = 0; i < 2; i++) /* Each revision level */
            (void) printf ("%d ", flip_short(t204->ff_version[i]));
            (void) printf (" platform = %.8s \n", t204->platform);
            (void) printf (" control_file = %.96s \n", t204->control_file);
            (void) printf (" ffcf_date = %d %03d %02d%02d%05.2f \n",
                       flip_short(t204->ffcf_date.year), flip_short(t204->ffcf_date.day),
                       flip_short(t204->ffcf_date.hour), flip_short(t204->ffcf_date.minute),
                       flip_float(t204->ffcf_date.second));
            (void) printf (" override = %.128s \n", t204->override);
            continue;
            }           /* End of if type 204 */
        /* * Type-205 record? * */
        if (strncmp (t1->recId, "205", 3) == 0)
            {           /* Type 205? */
            /* Fourfit setup */
            (void) printf (" utc_central = %d %03d %02d%02d%05.2f \n",
                       flip_short(t205->utc_central.year), flip_short(t205->utc_central.day),
                       flip_short(t205->utc_central.hour), flip_short(t205->utc_central.minute),
                       flip_float(t205->utc_central.second));
            (void) printf (" offset = %.3f ", flip_float(t205->offset));
            (void) printf (" ffmode = %.8s \n", t205->ffmode);
            (void) printf (" search = ");
            for (i = 0; i < 6; i++) /* Rate search windows */
            (void) printf ("%.3e ", flip_float(t205->search[i]));
            (void) printf (" \n");
            (void) printf (" filter = ");
            for (i = 0; i < 8; i++)
                {           /* Filter thresholds */
                if (i > 0 && i % 4 == 0)    /* Newline? */
                    (void) printf (" \n  ");    /* Yes */
                (void) printf ("%.2e ", flip_float(t205->filter[i]));
                }
            (void) printf (" \n");
            (void) printf (" start = %d %03d %02d%02d%05.2f ",
                       flip_short(t205->start.year), flip_short(t205->start.day), flip_short(t205->start.hour),
                       flip_short(t205->start.minute), flip_float(t205->start.second));
            (void) printf (" stop = %d %03d %02d%02d%05.2f ",
                       flip_short(t205->stop.year), flip_short(t205->stop.day), flip_short(t205->stop.hour),
                       flip_short(t205->stop.minute), flip_float(t205->stop.second));
            (void) printf (" ref_freq = %.2f \n", flip_double(t205->ref_freq));
            for (i = 0; i < 16; i++)
                {           /* Each fourfit channel */
                if (t205->ffit_chan[i].ffit_chan_id == ' ') /* In use? */
                    continue;       /* Nope */
                (void) printf (" ffit_chan_id = %c ",
                       t205->ffit_chan[i].ffit_chan_id);
                (void) printf (" channels[] = ");
                for (j = 0; j < 4; j++)
                    {       /* Each channel index */
                    if (flip_short(t205->ffit_chan[i].channels[j]) < 0) /* In use? */
                        continue;   /* Nope */
                    (void) printf ("%d ", flip_short(t205->ffit_chan[i].channels[j]));
                    }
                (void) printf (" \n");
                }
            continue;
            }           /* End of if type 205 */
            /* * Type-206 record? * */
        if (strncmp (t1->recId, "206", 3) == 0)
            {           /* Type 206? */
            /* Data filtering */
            (void) printf (" \n");
            if (strncmp (t1->recVer, "00", 2) == 0)
                {           /* Version 00? */
                (void) printf (" start = %d %03d %02d%02d%05.2f ",  /* Yes */
                       flip_short(t206_0->start.year), flip_short(t206_0->start.day),
                       flip_short(t206_0->start.hour), flip_short(t206_0->start.minute),
                       flip_float(t206_0->start.second));
                (void) printf (" first_ap = %d ", flip_short(t206_0->first_ap));
                (void) printf (" last_ap = %d \n", flip_short(t206_0->last_ap));
                (void) printf (" accepted[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each accepted[] */
                    if (i % 4 == 0) /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_0[0][i].lsb), flip_short(pr_0[0][i].usb));
                    }
                (void) printf (" \n");
                (void) printf (" intg_time = %.1f ", flip_float(t206_0->intg_time));
                (void) printf (" accept_ratio = %.1f ", flip_float(t206_0->accept_ratio));
                (void) printf (" discard = %.1f \n", flip_float(t206_0->discard));
                for (j = 1; j < 9; j++)
                    {       /* Each reason number */
                    (void) printf (" reason%d[] = ", j);
                    for (i = 0; i < 16; i++)
                        {       /* Each reasonj[] */
                        if (i % 4 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_0[j][i].lsb), flip_short(pr_0[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ratesize = %d ", flip_short(t206_0->ratesize));
                (void) printf (" mbdsize = %d ", flip_short(t206_0->mbdsize));
                (void) printf (" sbdsize = %d \n", flip_short(t206_0->sbdsize));
                }
                else if (strncmp (t1->recVer, "01", 2) == 0)
                {           /* version 01 */
                (void) printf (" start = %d %03d %02d%02d%05.2f ",
                       flip_short(t206_1->start.year), flip_short(t206_1->start.day),
                       flip_short(t206_1->start.hour), flip_short(t206_1->start.minute),
                       flip_float(t206_1->start.second));
                (void) printf (" first_ap = %d ", flip_short(t206_1->first_ap));
                (void) printf (" last_ap = %d \n", flip_short(t206_1->last_ap));
                (void) printf (" accepted[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each accepted[] */
                    if (i % 4 == 0) /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_1[0][i].lsb), flip_short(pr_1[0][i].usb));
                    }
                (void) printf (" \n");
                (void) printf (" weights[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each weights[] */
                    if (i % 4 == 0) /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" lsb = %.2e usb = %.2e ",
                               flip_double(t206_1->weights[i].lsb),
                               flip_double(t206_1->weights[i].usb));
                    }
                (void) printf (" \n");
                (void) printf (" intg_time = %.1f ", flip_float(t206_1->intg_time));
                (void) printf (" accept_ratio = %.1f ", flip_float(t206_1->accept_ratio));
                (void) printf (" discard = %.1f \n", flip_float(t206_1->discard));
                for (j = 1; j < 9; j++)
                    {       /* Each reason number */
                    (void) printf (" reason%d[] = ", j);
                    for (i = 0; i < 16; i++)
                        {       /* Each reasonj[] */
                        if (i % 4 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_1[j][i].lsb), flip_short(pr_1[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ratesize = %d ", flip_short(t206_1->ratesize));
                (void) printf (" mbdsize = %d ", flip_short(t206_1->mbdsize));
                (void) printf (" sbdsize = %d \n", flip_short(t206_1->sbdsize));
                }
                else
                {           /* Assume version 02 */
                (void) printf (" start = %d %03d %02d%02d%05.2f ",
                       flip_short(t206_2->start.year), flip_short(t206_2->start.day),
                       flip_short(t206_2->start.hour), flip_short(t206_2->start.minute),
                       flip_float(t206_2->start.second));
                (void) printf (" first_ap = %d ", flip_short(t206_2->first_ap));
                (void) printf (" last_ap = %d \n", flip_short(t206_2->last_ap));
                (void) printf (" accepted[] = ");
                for (i = 0; i < 64; i++)
                    {       /* Each accepted[] */
                    if (i % 4 == 0) /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_2[0][i].lsb), flip_short(pr_2[0][i].usb));
                    }
                (void) printf (" \n");
                (void) printf (" weights[] = ");
                for (i = 0; i < 64; i++)
                    {       /* Each weights[] */
                    if (i % 4 == 0) /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" lsb = %.2e usb = %.2e ",
                               flip_double(t206_2->weights[i].lsb),
                               flip_double(t206_2->weights[i].usb));
                    }
                (void) printf (" \n");
                (void) printf (" intg_time = %.1f ", flip_float(t206_2->intg_time));
                (void) printf (" accept_ratio = %.1f ", flip_float(t206_2->accept_ratio));
                (void) printf (" discard = %.1f \n", flip_float(t206_2->discard));
                for (j = 1; j < 9; j++)
                    {       /* Each reason number */
                    (void) printf (" reason%d[] = ", j);
                    for (i = 0; i < 64; i++)
                        {       /* Each reasonj[] */
                        if (i % 4 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %d usb = %d ",
                               flip_short(pr_2[j][i].lsb), flip_short(pr_2[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ratesize = %d ", flip_short(t206_2->ratesize));
                (void) printf (" mbdsize = %d ", flip_short(t206_2->mbdsize));
                (void) printf (" sbdsize = %d \n", flip_short(t206_2->sbdsize));
                }
            continue;
            }           /* End of if type 206 */
        /* * Type-207 record? * */
        if (strncmp (t1->recId, "207", 3) == 0)
            {           /* Type 207? */
            /* Phasecal and error rate */
            if (strncmp (t1->recVer, "00", 2) == 0)
                {           /* Version 00? */
                (void) printf (" \n");  /* Yes */
                for (j = 0; j < 6; j++)
                    {       /* Each pcname_0[] */
                    (void) printf (" %s = ", pcname_0[j]);
                    for (i = 0; i < 16; i++)
                        {       /* Each pcname_0[j][] */
                        if (i % 2 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %.2e usb = %.2e ",
                               flip_float(psbf_0[j][i].lsb), flip_float(psbf_0[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ref_pcrate = %.2e ", flip_float(t207_0->ref_pcrate));
                (void) printf (" rem_pcrate = %.2e \n", flip_float(t207_0->rem_pcrate));
                (void) printf (" ref_errate[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each ref_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_0->ref_errate[i]));
                    }
                (void) printf (" \n");
                (void) printf (" rem_errate[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each rem_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_0->rem_errate[i]));
                    }
                }
            else if(strncmp (t1->recVer, "01", 2) == 0)
                {           /* version 01 */
                (void) printf (" pcal_mode = %d \n", flip_int(t207_1->pcal_mode));
                for (j = 0; j < 8; j++)
                    {       /* Each pcname_1[] */
                    (void) printf (" %s = ", pcname_1[j]);
                    for (i = 0; i < 16; i++)
                        {       /* Each pcname_1[j][] */
                        if (i % 2 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %.2e usb = %.2e ",
                               flip_float(psbf_1[j][i].lsb), flip_float(psbf_1[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ref_pcrate = %.2e ", flip_float(t207_1->ref_pcrate));
                (void) printf (" rem_pcrate = %.2e \n", flip_float(t207_1->rem_pcrate));
                (void) printf (" ref_errate[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each ref_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_1->ref_errate[i]));
                    }
                (void) printf (" \n");
                (void) printf (" rem_errate[] = ");
                for (i = 0; i < 16; i++)
                    {       /* Each rem_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_1->rem_errate[i]));
                    }
                }
            else
                {           /* Assume version 02 */
                (void) printf (" pcal_mode = %d \n", flip_int(t207_2->pcal_mode));
                for (j = 0; j < 8; j++)
                    {       /* Each pcname_1[] */
                    (void) printf (" %s = ", pcname_2[j]);
                    for (i = 0; i < 64; i++)
                        {       /* Each pcname_1[j][] */
                        if (i % 2 == 0) /* New line? */
                            (void) printf (" \n "); /* Yes */
                        (void) printf (" lsb = %.2e usb = %.2e ",
                               flip_float(psbf_2[j][i].lsb), flip_float(psbf_2[j][i].usb));
                        }
                    (void) printf (" \n");
                    }
                (void) printf (" ref_pcrate = %.2e ", flip_float(t207_2->ref_pcrate));
                (void) printf (" rem_pcrate = %.2e \n", flip_float(t207_2->rem_pcrate));
                (void) printf (" ref_errate[] = ");
                for (i = 0; i < 64; i++)
                    {       /* Each ref_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_2->ref_errate[i]));
                    }
                (void) printf (" \n");
                (void) printf (" rem_errate[] = ");
                for (i = 0; i < 64; i++)
                    {       /* Each rem_errate[] */
                    if (i > 0 && i % 8 == 0)    /* New line? */
                        (void) printf (" \n "); /* Yes */
                    (void) printf (" %.2f ", flip_float(t207_2->rem_errate[i]));
                    }
                }

            (void) printf (" \n");
            continue;
            }           /* End of if type 207 */
        /* * Type-208 record? * */
        if (strncmp (t1->recId, "208", 3) == 0)
            {           /* Type 208? */
            /* Solution parameters */
            (void) printf (" quality = %c ", t208->quality);
            (void) printf (" errcode = %c ", t208->errcode);
            (void) printf (" tape_qcode = %.6s \n", t208->tape_qcode);
            (void) printf (" adelay  =      %.12e \n", flip_double(t208->adelay));
            (void) printf (" arate   =      %.12e \n", flip_double(t208->arate));
            (void) printf (" aaccel  =      %.12e \n", flip_double(t208->aaccel));
            (void) printf (" tot_mbd  =     %.12e \n", flip_double(t208->tot_mbd));
            (void) printf (" tot_sbd  =     %.12e \n", flip_double(t208->tot_sbd));
            (void) printf (" tot_rate  =    %.12e \n", flip_double(t208->tot_rate));
            (void) printf (" tot_mbd_ref =  %.12e \n", flip_double(t208->tot_mbd_ref));
            (void) printf (" tot_sbd_ref =  %.12e \n", flip_double(t208->tot_sbd_ref));
            (void) printf (" tot_rate_ref = %.12e \n", flip_double(t208->tot_rate_ref));
            (void) printf (" resid_mbd = %.6e ", flip_float(t208->resid_mbd));
            (void) printf (" resid_sbd = %.6e ", flip_float(t208->resid_sbd));
            (void) printf (" resid_rate = %.6e \n", flip_float(t208->resid_rate));
            (void) printf (" mbd_error = %.2e ", flip_float(t208->mbd_error));
            (void) printf (" sbd_error = %.2e ", flip_float(t208->sbd_error));
            (void) printf (" rate_error = %.2e \n", flip_float(t208->rate_error));
            (void) printf (" ambiguity = %.2e ", flip_float(t208->ambiguity));
            (void) printf (" amplitude = %.2e \n", flip_float(t208->amplitude));
            (void) printf (" inc_seg_ampl = %.2e ", flip_float(t208->inc_seg_ampl));
            (void) printf (" inc_chan_ampl = %.2e \n", flip_float(t208->inc_chan_ampl));
            (void) printf (" snr = %.2e ", flip_float(t208->snr));
            (void) printf (" prob_false = %.2e \n", flip_float(t208->prob_false));
            (void) printf (" totphase = %.2e ", flip_float(t208->totphase));
            if (strncmp (t208->version_no, "00", 2) == 0)   /* Version 00? */
                (void) printf (" resphase = %.2e \n", flip_float(t208->resphase));  /* Yes */
            else
                {           /* Assume version 01 */
                (void) printf (" totphase_ref = %.2e ", flip_float(t208_1->totphase_ref));
                (void) printf (" resphase = %.2e \n", flip_float(t208_1->resphase));
                (void) printf (" tec_error = %.2e \n", flip_float(t208_1->tec_error));
                }
            continue;
            }           /* End of if type 208 */
        /* * Type-210 record? * */
        if (strncmp (t1->recId, "210", 3) == 0)
            {           /* Type 210? */
            /* Residual fringe amplitude and phase */
            (void) printf (" \n");
            for (i = 0; i < 64; i++)
                {           /* Each channel (?) */
                (void) printf (" %2d ampl = %.3e ", i, flip_float(t210->amp_phas[i].ampl));
                (void) printf (" phase = %.1f \n", flip_float(t210->amp_phas[i].phase));
                }
            continue;
            }           /* End of if type 210 */
        /* * Type-212 record? * */
        if (strncmp (t1->recId, "212", 3) == 0)
            {           /* Type 212? */
            /* Phasor data ?? */
            (void) printf (" nap = %d \n", flip_short(t212->nap));
            (void) printf (" first_ap = %d ", flip_short(t212->first_ap));
            (void) printf (" channel = %d ", flip_short(t212->channel));
            (void) printf (" sbd_chan = %d \n", flip_short(t212->sbd_chan));
            (void) printf (" Index   amp         phase     weight \n");
            if (strncmp (t1->recVer, "00", 2) == 0) /* Version 00? */
                for (i = 0; i < flip_short(t212->nap); i++) /* Yes, each data[] */
                    (void) printf ("%3d %12.3e %9.4f \n",
                       i, flip_float(t212->data[i].amp), flip_float(t212->data[i].phase));
            else            /* Assume version 01 */
                for (i = 0; i < flip_short(t212_1->nap); i++)   /* Each data[] */
                    (void) printf ("%3d %12.3e %9.4f %8.3f \n",
                       i, flip_float(t212_1->data[i].amp), flip_float(t212_1->data[i].phase),
                       flip_float(t212_1->data[i].weight));
            }           /* End of if type 212 */
        /* * Type-220 record? * */
        if (strncmp (t1->recId, "220", 3) == 0)
            {           /* Type 220? */
            /* Fringe plot */
            (void) printf (" width = %d ", flip_short(t220->width));
            (void) printf (" height = %d \n", flip_short(t220->height));
            continue;
            }           /* End of if type 220 */
        /* * Type-221 record? * */
        if (strncmp (t1->recId, "221", 3) == 0)
            {           /* Type 221? */
            /* Postscript plot */
            (void) printf (" ps_length = %d \n", flip_int(t221->ps_length));
            continue;
            }           /* End of if type 221 */
        /* * Type-222 record? * */
        if (strncmp (t1->recId, "222", 3) == 0)
            {           /* Type 222? */
            /* control file record */
            (void) printf (" set_string_length = %d \n", flip_int(t222->setstring_length));
            (void) printf (" cf_length = %d \n", flip_int(t222->cf_length));
            (void) printf (" set_string_hash = %u \n", flip_int(t222->setstring_hash));
            (void) printf (" control_hash = %u \n", flip_int(t222->control_hash));
            t222_sspad = ( ( flip_int(t222->setstring_length) + 7 ) & ~7) + 8;
            t222_cfpad = ( ( flip_int(t222->cf_length) + 7 ) & ~7) + 8;            
            (void) printf (" set_string contents = %s \n", t222->control_contents);
            (void) printf (" control_file contents = %s \n", &(t222->control_contents[t222_sspad]) );
            continue;
            }           /* End of if type 221 */
        /* * Type-230 record? * */
        if (strncmp (t1->recId, "230", 3) == 0)
            {           /* Type 230? */
            /* Complex spectral data ?? */
            (void) printf (" nspec_pts = %d \n", flip_short(t230->nspec_pts));
            (void) printf (" frq = %d ", flip_int(t230->frq));
            (void) printf (" ap = %d ", flip_int(t230->ap));
            (void) printf (" lsbweight = %e ", flip_float(t230->lsbweight));
            (void) printf (" usbweight = %e \n", flip_float(t230->usbweight));
            (void) printf (" Index    re          im \n");
            for (i = 0; i < flip_short(t230->nspec_pts); i++)   /* Each xpower[] */
                (void) printf ("%3d %12.3e %12.3e \n",
                       i, flip_double(creal(t230->xpower[i])),
                       flip_double(cimag(t230->xpower[i])));
            }           /* End of if type 230 */
        /* ** Type-3xx records are in station-data files ** */
        /* * Type-300 record? * */
        if (strncmp (t1->recId, "300", 3) == 0)
            {           /* Type 300? */
            /* Station ID and model parameters */
            (void) printf (" SU_number = %d ", t300->SU_number);
            (void) printf (" id = %c ", t300->id);
            (void) printf (" intl_id = %.2s ", t300->intl_id);
            (void) printf (" name = %s \n", t300->name);
            (void) printf (" model_start = %d %03d %02d%02d%05.2f ",
                       flip_short(t300->model_start.year), flip_short(t300->model_start.day),
                       flip_short(t300->model_start.hour), flip_short(t300->model_start.minute),
                       flip_float(t300->model_start.second));
            (void) printf (" model_interval = %.2f ", flip_float(t300->model_interval));
            (void) printf (" nsplines = %d \n", flip_short(t300->nsplines));
            continue;
            }           /* End of if type 300 */
        /* * Type-301 record? * */
        if (strncmp (t1->recId, "301", 3) == 0)
            {           /* Type 301? */
            /* Delay polynomial coefficients */
            // short_reverse(&(t301->interval));
            (void) printf (" interval = %d ", flip_short(t301->interval));
            (void) printf (" chan_id = %s \n", t301->chan_id);
            (void) printf (" delay_spline[] = ");
            for (i = 0; i < 6; i++)
                {           /* Each delay_spline[] */
                if (i == 2)     /* New line before 3rd */
                    (void) printf (" \n   ");
                if (i == 0)     /* First two get special formats */
                    (void) printf (" %.17f ", flip_double(t301->delay_spline[0]));
                else if (i == 1)
                    (void) printf (" %.15f ", flip_double(t301->delay_spline[1]));
                else        /* Normal */
                    (void) printf (" %.8e ", flip_double(t301->delay_spline[i]));
                }
            (void) printf (" \n");
            continue;
            }           /* End of if type 301 */
        /* * Type-302 record? * */
        if (strncmp (t1->recId, "302", 3) == 0)
            {           /* Type 302? */
            /* Phase polynomial coefficients */
            (void) printf (" interval = %d ", flip_short(t302->interval));
            (void) printf (" chan_id = %s \n", t302->chan_id);
            (void) printf (" phase_spline[] = ");
            for (i = 0; i < 6; i++)
                {           /* Each phase_spline[] */
                if (i == 2)     /* New line before 3rd */
                    (void) printf (" \n   ");
                if (i == 0)     /* First two get special formats */
                    (void) printf (" %.17f ", flip_double(t302->phase_spline[0]));
                else if (i == 1)
                    (void) printf (" %.15f ", flip_double(t302->phase_spline[1]));
                else        /* Normal */
                    (void) printf (" %.3e ", flip_double(t302->phase_spline[i]));
                }
            (void) printf (" \n");
            continue;
            }           /* End of if type 302 */
        /* * Type-303 record? * */
        if (strncmp (t1->recId, "303", 3) == 0)
            {           /* Type 303? */
            /* Az, el, pa, u, v, w */
            printf (" interval = %d ", flip_short(t303->interval));
            printf (" chan_id = %s \n", t303->chan_id);
            printf ("azimuth %g %g %g %g %g %g\n", flip_double(t303->azimuth[0]),
                   flip_double(t303->azimuth[1]), flip_double(t303->azimuth[2]),
                   flip_double(t303->azimuth[3]), flip_double(t303->azimuth[4]),
                   flip_double(t303->azimuth[5]));
            printf ("elevation %g %g %g %g %g %g\n", flip_double(t303->elevation[0]),
                   flip_double(t303->elevation[1]), flip_double(t303->elevation[2]),
                   flip_double(t303->elevation[3]), flip_double(t303->elevation[4]),
                   flip_double(t303->elevation[5]));
            printf ("parallactic angle %g %g %g %g %g %g\n", flip_double(t303->parallactic_angle[0]),
                   flip_double(t303->parallactic_angle[1]), flip_double(t303->parallactic_angle[2]),
                   flip_double(t303->parallactic_angle[3]), flip_double(t303->parallactic_angle[4]),
                   flip_double(t303->parallactic_angle[5]));
            printf ("u %g %g %g %g %g %g\n", flip_double(t303->u[0]),
                   flip_double(t303->u[1]), flip_double(t303->u[2]),
                   flip_double(t303->u[3]), flip_double(t303->u[4]),
                   flip_double(t303->u[5]));
            printf ("v %g %g %g %g %g %g\n", flip_double(t303->v[0]),
                   flip_double(t303->v[1]), flip_double(t303->v[2]),
                   flip_double(t303->v[3]), flip_double(t303->v[4]),
                   flip_double(t303->v[5]));
            printf ("w %g %g %g %g %g %g\n", flip_double(t303->w[0]),
                   flip_double(t303->w[1]), flip_double(t303->w[2]),
                   flip_double(t303->w[3]), flip_double(t303->w[4]),
                   flip_double(t303->w[5]));
            printf ("\n");
            continue;
            }           /* End of if type 303 */
        /* * Type-304 record? * */
        if (strncmp (t1->recId, "304", 3) == 0)
            {           /* Type 304? */
            /* "Cooked" track error statistics */
            (void) printf (" time = %d %03d %02d%02d%05.2f ", flip_short(t304->time.year),
                       flip_short(t304->time.day), flip_short(t304->time.hour), flip_short(t304->time.minute),
                       flip_float(t304->time.second));
            (void) printf (" duration = %.2f \n", flip_float(t304->duration));
            for (i = 0; i < 64; i++)
                {           /* Each trackstats[] */
                (void) printf (" %2d ", i);
                (void) printf (" error_count = %d ",
                       flip_int(t304->trackstats[i].error_count));
                (void) printf (" frames = %d ", flip_int(t304->trackstats[i].frames));
                (void) printf (" bad_frames = %d ",
                       flip_int(t304->trackstats[i].bad_frames));
                (void) printf (" slip_sync = %d ", flip_int(t304->trackstats[i].slip_sync));
                (void) printf (" missing_sync = %d ",
                       flip_int(t304->trackstats[i].missing_sync));
                (void) printf (" crc_error = %d \n",
                       flip_int(t304->trackstats[i].crc_error));
                }
            continue;
            }           /* End of if type 304 */
        /* * Type-306 record? * */
        if (strncmp (t1->recId, "306", 3) == 0)
            {           /* Type 306? */
            /* "Cooked" state counts */
            (void) printf (" time = %d %03d %02d%02d%05.2f \n", flip_short(t306->time.year),
                       flip_short(t306->time.day), flip_short(t306->time.hour), flip_short(t306->time.minute),
                       flip_float(t306->time.second));
            (void) printf (" duration = %e \n", flip_float(t306->duration));
            for (i = 0; i < 16; i++)
                {           /* Each stcount[] */
                (void) printf (" %2d ", i);
                (void) printf (" chan_id = %s ", t306->stcount[i].chan_id);
                (void) printf (" bigpos = %d ", flip_int(t306->stcount[i].bigpos));
                (void) printf (" pos = %d ", flip_int(t306->stcount[i].pos));
                (void) printf (" neg = %d ", flip_int(t306->stcount[i].neg));
                (void) printf (" bigneg = %d \n", flip_int(t306->stcount[i].bigneg));
                }
            continue;
            }           /* End of if type 306 */
        /* * Type-307 record? * */
        if (strncmp (t1->recId, "307", 3) == 0)
            {           /* Type 307? */
            /* "Raw" phase-cal counts */
            (void) printf (" su = %d \n", flip_int(t307->su));
            (void) printf (" tot = %.6e ", flip_double(t307->tot));
            (void) t2s (flip_double(t307->tot) / SYSCLK, line); /* TOT to ASCII */
            (void) printf (" %s \n", line);
            (void) printf (" rot = %.6e ", flip_double(t307->rot));
            (void) t2s (flip_double(t307->rot) / SYSCLK, line); /* ROT to ASCII */
            (void) printf (" %s \n", line);
            (void) printf (" accum_period = %.3f ", flip_double(t307->accum_period));
            (void) printf (" frame_count = %d \n", flip_int(t307->frame_count));
            for (i = 0; i < 16; i++)
                {           /* Each channel */
                (void) printf (" counts[%d]  val_count = %d \n",
                       i, flip_int(t307->counts[i].val_count));
                (void) printf ("  counts[] = ");
                for (j = 0; j < 8; j++)
                    {       /* Each counter, LSB first */
                    (void) printf (" %d ", flip_int(t307->counts[i].count[j]));
                    if (j % 4 == 3)
                        (void) printf (" \n");
                    if (j == 3)
                        (void) printf ("             ");
                    }
                }
            /* Now for something extra special:  Calculate "cooked" values.
             * But since we do not read svex, we use default coefficients. */
            (void) printf ("Cooked phase cals (for 2x4) from this type 307: \n");
            for (i = 0; i < 16; i++)
                {           /* Each channel */
                (void) printf (" %2d ", i);
                a_sin =
                    (0.83 * flip_int(t307->counts[i].count[0]) +
                     1.67 * flip_int(t307->counts[i].count[1])) / flip_int(t307->counts[i].val_count) -
                    1.25;
                (void) printf (" a_sin = %6.3f ", a_sin);
                a_cos =
                    (0.83 * flip_int(t307->counts[i].count[2]) +
                     1.67 * flip_int(t307->counts[i].count[3])) / flip_int(t307->counts[i].val_count) -
                    1.25;
                (void) printf (" a_cos = %6.3f ", a_cos);
                a_amp = hypot (a_sin, a_cos);
                (void) printf (" a_amp = %6.3f ", a_amp);
                a_ph = 180.0 * atan2 (a_sin, -a_cos) / M_PI;    /* ?? N.B. fudged sign ?? */
                (void) printf (" a_ph = %6.1f \n", a_ph);
                (void) printf ("    ");
                b_sin =
                (0.83 * flip_int(t307->counts[i].count[4]) +
                 1.67 * flip_int(t307->counts[i].count[5])) / flip_int(t307->counts[i].val_count) -
                1.25;
                (void) printf (" b_sin = %6.3f ", b_sin);
                b_cos =
                (0.83 * flip_int(t307->counts[i].count[6]) +
                 1.67 * flip_int(t307->counts[i].count[7])) / flip_int(t307->counts[i].val_count) -
                1.25;
                (void) printf (" b_cos = %6.3f ", b_cos);
                b_amp = hypot (b_sin, b_cos);
                (void) printf (" b_amp = %6.3f ", b_amp);
                b_ph = 180.0 * atan2 (b_sin, -b_cos) / M_PI;    /* ?? N.B. fudged sign ?? */
                (void) printf (" b_ph = %6.1f \n", b_ph);
                }
            continue;
            }           /* End of if type 307 */
        /* * Type-308 record? * */
        if (strncmp (t1->recId, "308", 3) == 0)
            {           /* Type 308? */
            /* "Cooked" phase-cal values */
            (void) printf (" time = %d %03d %02d%02d%05.2f ", flip_short(t308->time.year),
                       flip_short(t308->time.day), flip_short(t308->time.hour), flip_short(t308->time.minute),
                       flip_float(t308->time.second));
            (void) printf (" duration = %.2f \n", flip_float(t308->duration));
            for (i = 0; i < 32; i++)
                {           /* Each pcal[] */
                (void) printf (" %2d ", i);
                (void) printf (" chan_id = %.8s ", t308->pcal[i].chan_id);
                (void) printf (" frequency = %7.0f ", flip_float(t308->pcal[i].frequency));
                (void) printf (" real = %9.5f ", flip_float(t308->pcal[i].real));
                (void) printf (" imag = %9.5f \n", flip_float(t308->pcal[i].imaginary));
                }
            continue;
            }           /* End of if type 308 */
        // Type-309 record?
        if (strncmp (t1->recId, "309", 3) == 0)
            if (strncmp (t309->version_no, "00", 2) == 0)
                {                   // Mk5B phase cal - version 00
                printf ("su %d   ntones %d   ",
                    flip_int(t309_v0->su), flip_int(t309_v0->ntones));
                nt = flip_int(t309_v0->ntones);
                nt = 4 * ((nt - 1) / 4 + 1);
                                    // convert ROT to ASCII
                t2s (flip_double(t309_v0->rot) / SYSCLK, line);
                printf ("rot %s   ", line);

                printf ("acc_period %6.3lf \n",
                flip_double(t309_v0->acc_period));

                printf ("tone freqs (Hz):");
                for (i = 0; i < nt; i++)
                    {
                    if (i % 4 == 0)
                        printf ("\n");
                    tval = flip_double(t309_v0->chan[i].freq);
                    if (fabs(tval) < 1e9)
                        printf ("%17.3lf  ", tval);
                    else
                        printf ("%17s  ", "huge");
                    }
                printf ("\n");
                for (i = 0; i < nt; i++)
                    {
                    if (t309_v0->chan[i].chan_name[0] == 0 ||
                        !isprint(t309_v0->chan[i].chan_name[0]))
                                        continue;   // channel unused, skip it
                    printf ("chan_name[%d] = %.8s\n", i, t309_v0->chan[i].chan_name);
                    for (j = 0; j < nt; j++)
                        {
                        printf ("%8x %8x  ",
                            flip_int(t309_v0->chan[i].acc[j][0]),
                            flip_int(t309_v0->chan[i].acc[j][1]));
                        if (j % 4 == 3)
                            printf ("\n");
                        }
                    printf ("\n");
                    }
                }
            else
                {                   // Mk5B phase cal - version 01
                printf ("su %d   ntones %d   ",
                    flip_int(t309->su), flip_int(t309->ntones));
                nt = flip_int(t309->ntones);
                nt = 4 * ((nt - 1) / 4 + 1);
                                    // convert ROT to ASCII
                t2s (flip_double(t309->rot) / SYSCLK, line);
                printf ("rot %s   ", line);

                printf ("acc_period %6.3lf \n",
                flip_double(t309->acc_period));

                printf ("tone freqs (Hz):");
                for (i = 0; i < nt; i++)
                    {
                    if (i % 4 == 0)
                        printf ("\n");
                    tval = flip_double(t309->chan[i].freq);
                    if (fabs(tval) < 1e9)
                        printf ("%17.3lf  ", tval);
                    else
                        printf ("%17s  ", "huge");
                    }
                printf ("\n");
                for (i = 0; i < 64; i++)
                    {
                    if (t309->chan[i].chan_name[0] == 0 ||
                        !isprint(t309->chan[i].chan_name[0]))
                                        continue;   // channel unused, skip it
                    printf ("chan_name[%d] = %.8s\n", i, t309->chan[i].chan_name);
                    for (j = 0; j < nt; j++)
                        {
                        if (!getenv("CORASC2_PCAL"))
                        { // previous print format
                        printf ("%8x %8x  ",
                            flip_int(t309->chan[i].acc[j][0]),
                            flip_int(t309->chan[i].acc[j][1]));
                        }
                        else
                        { // replicate pcal_interp.c code
                        double u,v,re,im;
                        u = flip_int(t309->chan[i].acc[j][0]);
                        v = flip_int(t309->chan[i].acc[j][1]);
#define TWO31 2147483648.0
#define TWO32 4294967296.0
                        u = (u < TWO31) ? u : u - TWO32;
                        v = (v < TWO31) ? v : v - TWO32;
                        re = u * 1e-6 / (-128 * flip_double(t309->acc_period));
                        im = v * 1e-6 / (-128 * flip_double(t309->acc_period));
                        printf ("%7.4f %+9.4f  ",
                            sqrt(re*re+im*im), atan2(im, re)*180/M_PI);
                        }
                        if (j % 4 == 3)
                            printf ("\n");
                        }
                    printf ("\n");
                    }
                }
        /* * tapeDB record? * */
        if (strncasecmp (t1->recId, "TDB", 3) == 0)
            {           /* tapeDB? */
            /* Tape database record */
            (void) printf ("%s got type TDB ", me);
            (void) printf (" record_id = %.3s ", ttdb->record_id);
            (void) printf (" version_no = %.2s \n", ttdb->version_no);
            if (strncmp (ttdb->version_no, "00", 2) != 0)
                {           /* Version with sus[]? */
                (void) printf (" sus[] = ");    /* Yes */
                for (i = 0; i < 5; i++)
                    {       /* Each sus[] */
                    if (ttdb->sus[i] == 0xff)   /* End? */
                        break;      /* Yes */
                    (void) printf ("%x ", ttdb->sus[i]);
                    }
                }
            (void) printf (" vsn = %.8s ", ttdb->vsn);
            (void) printf (" station = %c ", ttdb->station);
            (void) printf (" site_id = %.3s \n", ttdb->site_id);
            (void) printf (" exper_num = %d ", flip_short(ttdb->exper_num));
            (void) printf (" exper_name = %s ", ttdb->exper_name);
            (void) printf (" start_time = %d %.3d \n", flip_short(ttdb->start_time.year),
                       flip_short(ttdb->start_time.day));
            for (i = 0; i < MAX_PASS; i++)
                {           /* Each pass */
                if (flip_int(ttdb->head[i]) > 3)    /* Data? */
                    continue;       /* Nope, to next i */
                for (k = 1; k >= 0; k--)
                    {       /* Each direction, F or R */
                    (void)
                    printf (" index %2d  %c  head %d  headpos %5.0f microns \n",
                        i, k == 0 ? 'R' : 'F', flip_int(ttdb->head[i]),
                        flip_int(ttdb->headpos[i]) / 10.0);
                    for (j = 0; j < MAX_INDEX; j++)
                        {       /* Each index (footage/500) */
                        if (flip_int(ttdb->pass[i][j].head_pos_offset[k]) == 16383) /* OK? */
                            (void) printf ("  -- ");    /* Nope */
                        else    /* Probably valid data */
                            (void) printf ("%5.0f",
                               (short) flip_int(ttdb->pass[i][j].
                               head_pos_offset[k]) / 10.0);
                        /* (Because it's a short in int's clothing) */
                        if (j % 13 == 12)   /* New line? */
                            (void) printf (" \n");  /* Yes */
                        }       /* End of for j each index */
                    (void) printf (" \n");
                    }       /* End of for k each direction */
                }           /* End of for i each pass */
            }           /* End of if tapeDB record */
        /* We can't (yet) do any other record types */
        }               /* End of while loop until break */
      /* Probably end of file */
      /* *** Print number of each record type that we read *** */
    for (i = 0; i < NNAMES; i++)    /* Each record type */
        if (countt[i] > 0)      /* Read any of this type? */
            (void) printf (" Read %4d type %s record%c \n", countt[i], rnames[i],
               countt[i] > 1 ? 's' : ' ');
    (void) printf ("%s End \n", me);
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
