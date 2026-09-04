#ifndef FSTRUCT_DONE
#define FSTRUCT_DONE

/* 180 dual-pol baselines breaks this */
#define MAXFILES 500000
/* an old restriction for filenum to be 1-9999 */
#define MAXFNDIGITS 4
#define LEGALPOLCHARS   "HVLRXY"

/* these flags are defined, but do not seem to ever be used */
#define BADSTRING 0x001
#define BADFORM   0x002
#define BADBASE   0x004
#define BADFNUM   0x008
#define BADFREQ   0x010
#define BADSRC    0x020
#define BADROOT   0x040
#define BADSTAT   0x080
#define BADMALLOC 0x100

#ifndef FALSE
#define TRUE 1
#define FALSE 0
#endif

/* defaults to indicate a cleared structure are in () below */

/* type 0 is root file SOURCE.stamp    (2 parts)
 * type 1 is corel, AB.nn.abcdef       (3 parts)
 * type 2 is fringe, AB.X.nn.abcdef    (4 parts)
 *      2 is fringe, AB.X.nn-pp.abcdef (4 parts) new with 3.26
 * type 3 is sdata, A.nn.abcdef        (3 parts)
 * type 4 is log.<sourcename>?         (2 parts) */

typedef struct
    {
    int order;		    /* For sorting operations, (-1) */
    char *name;		    /* Full name of file (NULL) */
    short type;		    /* File type according to name 0...4 (see above; -1) */
    char source[32];	/* Source name (root files only;  0) */
    char baseline[3];	/* Baseline id (fringe and corel files only; 0) */
    char station;	    /* Type 3 (pcal) files only (' ') */
    char freq_code;	    /* Frequency id code (fringe files only) (' ') */
    int filenum;	    /* File number in fileset (-1)*/
    char rootcode[7];	/* root id code 'abcdef' (0) */
    short done;		    /* Has this file has been done (see below; FALSE) */
    int intparm[4];	    /* Auxiliary info if needed (see below; 0) */
    float floatparm[4];	/* Auxiliary info if needed (see below; 0.0) */
    /* new features */
    int namealloc;      /* strlen(name)+1 when name malloc'd */
    char poln[3];       /* Polarization info ('  ') */
    } fstruct;

/* The 'done' field is used in sort_names and alist.c
 * fringex uses intparm for fdata.srch_cotime and fdata.noloss_cotime
 *   and   uses floatparm for fdata.delay_rate and fdata.mbdelay
 * poln is populated if the fringe number includes the polarization */

/* namealloc was introduced to allow name to be freed when fset is used;
 * however it is difficult to prevent an invalid free, thus any use of
 * these functions must use prep_fstruct() when it is known that the
 * fstruct may contain garbage--this happens if the variable is on the
 * stack or malloc'd.  It is not necessary if the struct was previously
 * in previous calls as it will certainly not contain garbage.
 *
 * This applies to filesets as well, see fileset.h */

/* sub/utils */
extern void   prep_fstruct(fstruct *);
extern int    check_name (char *, fstruct *);
extern int    extract_filenames (char *, int, fstruct **, int *, int *, int *);
extern int    get_filelist (int, char **, int, fstruct **);

#endif
