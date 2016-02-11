/************************************************************************/
/*                                                                      */
/* These are the definitions of memory representations of Mk4 data      */
/* files.  The IO library will read the file into memory, then arrange  */
/* for the information to find its way into these structures.  The      */
/* structure elements are all pointers to data record structures.  The  */
/* pointer values can be filled in by setting them to the address of    */
/* the appropriate segment of the raw memory image.  If the disk format */
/* of the record and the definition of the C structure have become      */
/* different, due to format evolution, the IO library must identify     */
/* the version number of the disk format, allocate memory for the       */
/* structure, and copy required data fields into the structure.         */
/* The status of the format and structure compatibility is kept in the  */
/* header file mk4_version.h, which is used by the IO library.          */
/*                                                                      */
/* The "allocated" array of pointers, and the "nalloc" variable, are my */
/* brute force way of eliminating memory allocation blues.  Since it is */
/* a ghastly error to free memory multiple times, or to keep on         */
/* allocating memory without freeing it up again (very time-consuming   */
/* bugs to track down), here I aim to keep a master list of allocated   */
/* pointers.  Every malloc or related call places a copy of the pointer */
/* in the "allocated" array, and increments "nalloc".  When it's time   */
/* to free memory, you just free the first "nalloc" members of the      */
/* "allocated" array.  This should be fairly bulletproof, except that   */
/* realloc() calls could mess you up.  For each pointer, allocation     */
/* should be done in one place only, then left well alone.  Memory that */
/* is allocated outside the routines which fill these structures is     */
/* somebody else's problem.                                             */
/*                                                                      */
/* Created August 14 1995 by CJL                                        */
/*                                                                      */
/************************************************************************/
#ifndef MK4_DATA
#define MK4_DATA

#include "mk4_sizes.h"

#define MAXSTATIONS 32
#define MAXIND 8192
#define MAXAP 8192                      /* debug!! make larger, but with
                                           more clever code in plot_data.h 
                                           rjc 99.8.9                */
#define MAXLAG 8192

#define MAXMAX 8192                     /* set this to max(MAXAP,MAXLAG) */

/* #define MAXAP 4096 */
/* #define MAXLAG 1024 */
/* #define MAXMAX 4096 */

                                        /* MAKE SURE that the following
                                         * constant is the product of the
                                         * variables above, i.e.
                                         * = (MAXAP * MAXLAG) */
/* #define MAX_APXLAG 262144 */
/* #define MAX_APXLAG 1048576 */
/* #define MAX_APXLAG 2097152 */
// #define MAX_APXLAG 4194304
#define MAX_APXLAG MAXAP * MAXLAG
#define MAXSPLINES 64                   /* Spline polynomial intervals */
#define MAXSTATPER 3600                 /* Statistics acc periods  */

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#include "mk4_records.h"


struct mk4_corel
    {
    void *allocated[MAXIND + 4];                /* Ignore type 120 recs */
    int nalloc;
    char *file_image;
    struct type_000 *id;
    struct type_100 *t100;
    int index_space;
    struct index_tag
        {
        struct type_101 *t101;
        int ap_space;
        struct type_120 **t120;
        } *index;
    };

struct mk4_fringe
    {
    void *allocated[MAXFREQ + 15];
    int nalloc;
    char *file_image;
    struct type_000 *id;
    struct type_200 *t200;
    struct type_201 *t201;
    struct type_202 *t202;
    struct type_203 *t203;
    struct type_204 *t204;
    struct type_205 *t205;
    struct type_206 *t206;
    struct type_207 *t207;
    struct type_208 *t208;
    struct type_210 *t210;
/*     struct type_211 *t211; */
    int n212;
    struct type_212 *t212[MAXFREQ];
    struct type_220 *t220;
    struct type_221 *t221;
    int n230;
    struct type_230 *t230[MAXFREQ * MAXAP];
    };

struct mk4_sdata
    {
    void *allocated[2*MAXSPLINES*MAXFREQ + 7*MAXSTATPER + 3];
    int nalloc;
    char *file_image;
    struct type_000 *id;
    struct type_300 *t300;
    struct
        {
        char chan_id[32];
        struct type_301 *t301[MAXSPLINES];
        struct type_302 *t302[MAXSPLINES];
        struct type_303 *t303[MAXSPLINES];
        } model[MAXFREQ];
    int n304;
    int n305;
    int n306;
    int n307;
    int n308;
    int n309;
    struct type_304 *t304[MAXSTATPER];
    struct type_305 *t305[MAXSTATPER];
    struct type_306 *t306[MAXSTATPER];
    struct type_307 *t307[MAXSTATPER];
    struct type_308 *t308[MAXSTATPER];
    struct type_309 *t309[MAXSTATPER];
    };

struct mk4_log
    {
    char *file_image;
    struct type_000 *id;
    };
#endif
