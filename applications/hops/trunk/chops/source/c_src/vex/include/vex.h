#ifndef VEX_H
#define VEX_H

#define MAXCOMMENTS 8192
#define MAXQUOTES 2048
#define MAXBLOCKS 256
#define MAXDEFS 512
#define MAXREFS 256
#define MAXSCANS 8192
#define STATEMENT_SIZE 8192
#define MAX_NAMESIZE 32
#define MAX_PVALSIZE 128
#define MAXNVAL 256
#define MAXBLKSTMT 100
#define MAXSTMT 50
                                        /* Binary structs for various flavors */
                                        /* of vex files/sections */
#include "mk4_typedefs.h"
#include "ovex.h"
#include "cvex.h"
#include "evex.h"
#include "ivex.h"
#include "svex.h"
#include "lvex.h"
/* #include "lvex.h" */
                                        /* Struct to hold them all */
struct vex
    {
    int vextypes;                       /* Which sects are filled in (bitmap) */
    char filename[256];                 /* Disk-based vex file of origin */
    struct scan_struct *ovex;           /* Observe vex written by sched progs */
    struct Cvex_Config *cvex;           /* Correlator setup vex */
    struct evex_struct *evex;           /* Task-specific experiment vex */
    struct ivex_struct *ivex;           /* Correlator system initialization vex */
    struct svex_struct *svex;           /* Station unit setup vex */
    struct lvex_struct *lvex;           /* Log vex */
    char   *ovex_src;                   /* Filtered ascii vex source, based */
    char   *cvex_src;                   /* on the subset of the file that */
    char   *evex_src;                   /* was parsed.  If these pointers are */
    char   *svex_src;                   /* null, such filtered source was not */
    char   *ivex_src;                   /* requested */
    char   *lvex_src;
    };

#define  OVEX 1<<0
#define  CVEX 1<<1
#define  SVEX 1<<2
#define  IVEX 1<<3
#define  EVEX 1<<4
#define  LVEX 1<<5
#define  WANT_OUTPUT 1<<8
                                        /* Vex versions, to be OR'ed with OVEX */
                                        /* EVEX and others as needed */
#define V_1_0 100 << 10
#define V_1_5 150 << 10
#define V_ALL 998 << 10
#define V_BAD 999 << 10


/**************************************************************/
/*                                                            */
/* The rest of this file consists of definitions and structs  */
/* to support the low-level operation of the parser library   */
/*                                                            */
/**************************************************************/


struct comment
    {
                                        /* Addresses define extrema of comment */
    char  *start;
    char  *end;
    };

struct quote
    {
                                        /* Addresses define extrema of quote */
    char  *start;
    char  *end;
    };

struct ref
    {
                                        /* Strings define contents of ref statement */
    int  stno;
    char filename[256];
    char blockname[MAX_NAMESIZE];
    char keyword[MAX_NAMESIZE];
    int nargs;
    char *args[MAXNVAL];
    };

struct def
    {
                                        /* Store name and location, and */
                                        /* parse contents only when needed */
    int start;
    int end;
    char name[MAX_NAMESIZE];
    };

struct scan
    {
                                        /* A def by any other name */
    int start;
    int end;
    char name[MAX_NAMESIZE];
    };

struct block
    {
    char        name[MAX_NAMESIZE];
    int         stno;                   /* statement number of $BLOCK */
    int         end;                    /* Last statement in block */
    int         ndef;                   /* For primitive $BLOCKS */
    struct def  deflist[MAXDEFS];
    };

struct statement
    {
    char *start;
    char *end;
    char *str;
    };

struct insert
    {
    int after;
    char *str;
    };
                                        /* List of valid statement names */
                                        /* by block name (block_params.c) */
struct blk_stmt
    {
    char name[32];
    int  nstmt;
    char stmt[MAXSTMT][32];
    };
                                        /* Used for scan_info() */
struct def_list
    {
    int         blockno;
    int         defno;
    };

#define FALSE 0
#define TRUE 1

#define MAN TRUE
#define OPT FALSE
                                        /* Value type */
#define VAL_REAL   1
#define VAL_CHAR   2
#define VAL_INT    3
#define VAL_EPOCH  4
#define VAL_RA     5
#define VAL_DEC    6
#define VAL_LINK   7
#define VAL_NONE   8
                                        /* Value units */

#define UNIT_TIME          1
#define UNIT_FREQ          2
#define UNIT_SAMPLERATE    3
#define UNIT_LENGTH        4
#define UNIT_ANGLE         5
#define UNIT_FLUX          6
#define UNIT_BITDENSITY    7
#define UNIT_ANGLE_TIME    51
#define UNIT_LENGTH_TIME   41
#define UNIT_NONE          80
#define UNIT_OPTIONAL      100

struct value
    {
    char        mandatory;
    char        type;
    char        units;
    char        range[50];
    };

struct pval_format
    {
    char        param_name[128];
    int         typever;
    char        block[MAX_NAMESIZE];
    int         nval;
    int         ngroup;
    struct value *values;
    };

struct ra
    {
    short       ra_hrs;
    short       ra_mins;
    float       ra_secs;
    };

struct dec
    {
    short       dec_degs;
    short       dec_mins;
    float       dec_secs;
    };

struct data_value
    {
    int type;
    int units;
    union
        {
        char    strval[MAX_PVALSIZE];
        double  realval;
        int     intval;
        struct date epochval;
        struct sky_coord raval;
        struct sky_coord decval;
        char    linkval[MAX_NAMESIZE];
        } data;
    };

struct param_val
    {
    char name[MAX_NAMESIZE];
    int  nval;
    char *val[MAXNVAL];
    struct data_value dval[MAXNVAL];
    };

#endif
