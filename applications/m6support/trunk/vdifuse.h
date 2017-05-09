/*
 * $Id: vdifuse.h 4248 2017-02-28 22:37:57Z gbc $
 *
 * This file provides support for the fuse interface
 */

#ifndef vdifuse_h
#define vdifuse_h

#include <dirent.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/stat.h>

/* internal release number */
#ifndef VDIFUSE_VERSION
#define VDIFUSE_VERSION 0.0
#endif /* VDIFUSE_VERSION */

#define VDIFUSE_MAX_PATH 256
#define VDIFUSE_SEARCH_MAX 9999
#define VDIFUSE_ONE_SEC_NS (int64_t)1000000000
#define VDIFUSE_MAX_HIER 5
#define VDIFUSE_MIN_FILE_SIZE 60000
/* need 8x4, for sg2; 48 for SGInfo to fit, and this rounds out to 1024 */
#define VDIFUSE_MAX_SEQI (56)
#define VDIFUSE_NUM_VOIDS (VDIFUSE_MAX_SEQI*sizeof(uint32_t)/sizeof(void*))
#define VDIFUSE_TOPDIR_SIZE 16

/* ns timing on files -- one of these appears to be set in stdio.h */
#if !(defined(_BSD_SOURCE) || defined(_SVID_SOURCE) || defined(_POSIX_C_SOURCE))
#error "none of _BSD_SOURCE, _SVID_SOURCE or _POSIX_C_SOURCE was defined"
#endif

/* top-level subdirs of primary mount */
#define VDIFUSE_FRAGMENTS "/fragments"
#define VDIFUSE_SEQUENCES "/sequences"
extern char fragments_topdir[VDIFUSE_TOPDIR_SIZE];
extern char sequences_topdir[VDIFUSE_TOPDIR_SIZE];
extern int fragments_topdir_len;
extern int sequences_topdir_len;

/* set when -d for fuse is */
extern int vdifuse_debug;

/* normally stdout */
extern FILE *vdflog;

/* set when calling fuse_main() makes sense */
extern int vdifuse_enable;
#define VDIFUSE_ENABLE_SKIP 0
#define VDIFUSE_ENABLE_DOIT 1
#define VDIFUSE_ENABLE_HELP 2

/* protect the cache against accidental erasure */
extern int vdifuse_protect;

/* allow the mount point to be forcibly reused */
extern int vdifuse_reuse;

/* parse command-line options and trigger pre-fuse_main() work */
extern int vdifsup_opts(int *argc, char **argv[]);

/* inserted cache creation time into mtime/ctime/atime of stbuf */
extern void set_creation_time(struct stat *stbuf);

/* create the VDIF metadata cache */
extern int vdifuse_create_metadata(
    char *cache, int vdifuse_ndirs, char **vdifuse_dirs);

/* (report and) check the VDIF metadata cache */
extern int vdifuse_report_metadata(
    char *cache, struct stat *sb);
extern int vdifuse_report_filelist(
    char *cache, struct stat *sb);

/* just use it at a particular mount point */
extern int vdifuse_access_metadata(
    char *cache, struct stat *sb, struct stat *mp);

/* allow tuning of some internal options */
extern int vdifuse_options(char *arg);

/* for convenience when we refer to these */
typedef struct vdifuse_pars {
    float       vdifuse_vers;       /* creator version */
    /* options on cache creation: */
    uint32_t    prefix_bytes;       /* in each vdif file */
    uint32_t    offset_bytes;       /* in each packet to hdr */
    uint32_t    searchwindow;       /* ## slop in these */
    uint32_t    max_pkts_gap;       /* ## between fragments */
    float       max_secs_gap;       /* ## between fragments */
    uint32_t    how_rigorous;       /* fragments were tested */
    uint32_t    noduplicates;       /* filenames assumed uniq */
    uint32_t    seqhierarchy;       /* construct seq. subdirs */
    uint32_t    writeblocker;       /* write blocking buf.size */
    uint32_t    pkts_per_sec;       /* nominal packet rate */
    float       dropfraction;       /* ## rate of dropped packets */
    uint32_t    catchbuserrs;       /* activate bus error handler */
    uint32_t    station_mask;       /* for sg_signature() use */
    uint32_t    reserved_two;       /* reserved for future use */
    uint32_t    reserved_tre;       /* reserved for future use */
    uint32_t    reserved_fur;       /* reserved for future use */
    /* topdir names for fragments and sequences */
    char        frag_top_dir[VDIFUSE_TOPDIR_SIZE];
    char        seqs_top_dir[VDIFUSE_TOPDIR_SIZE];
    /* discovered information */
    uint32_t    maxfrcounter;       /* largest frame count seen */
    uint32_t    est_pkt_rate;       /* estimated rate */
    struct timeval  creation;       /* time of cache creation */
} VDIFUSEpars;

/*
 * Need to store entries as follows:
 *  PARAMS:    (vpars) general cache parameters
 *  FRAGMENT:  (vfuse) non struct stat per-file data (for fragments)
 *  DIRECTORY: (vfuse) subdirectory information
 *  SEQUENCE:  (vseqi) storage for sequences, follows DIRECTORY
 *  ANCILLARY: (voids) ancillary data (##sgv2), follows FRAGMENT
 *  INVALID:   (voids) empty entries in cache to ignore
 * 
 * ## items are not fully implemented
 */
typedef struct vdifuse_entry {
    uint32_t    index;                      /* index in array of entries */
    uint32_t    etype;                      /* see VDIFUSE_ENTRY_* above */

    /* need a union over additional stuff */
    uint64_t    vsig;                       /* vdif signature of frag/seq */
    uint32_t    entry_pps;                  /* packets per sec of frag/seq */
    uint32_t    stype;                      /* s(ub)type frag/seq/anc */
    uint32_t    cindex;                     /* continuation index */
    uint32_t    ccount;                     /* continuation count */

    union vdifuse_union {
        struct stat vfuse;                  /* stat buf provided to fuse */
        struct vdifuse_pars vpars;          /* internal cache parameters */
        uint32_t vseqi[VDIFUSE_MAX_SEQI];   /* indices of the sequence */
        void *voids[VDIFUSE_NUM_VOIDS];     /* ancillary data (sgv2) */
    } u;

    char        path[VDIFUSE_MAX_PATH];     /* absolute real path to entry */
    char        fuse[VDIFUSE_MAX_PATH];     /* absolute fuse path to entry */
    char        hier[VDIFUSE_MAX_PATH];     /* hierarchy placement of frag */
} VDIFUSEntry;

/* descriptive functions -- nonzero return if entry is defective */
extern int describe_struct(void);
extern int describe_params(VDIFUSEntry *vp);
extern int describe_fragment(VDIFUSEntry *vp);
extern int describe_sequence(VDIFUSEntry *vp);
extern int describe_directory(VDIFUSEntry *vp);
extern int describe_ancillary(VDIFUSEntry *vp);
/* a switch into the above, ignoring checking */
extern void describe_cache_entry(VDIFUSEntry *vp);

/* getattr support: return 0 if path is a fragment or sequence */
extern int vdifuse_fragment(const char *path, struct stat *stbuf);
extern int vdifuse_sequence(const char *path, struct stat *stbuf);
#define VDIFUSE_TOPDIR_PARAMS   0
#define VDIFUSE_TOPDIR_ROOT_DIR 1
#define VDIFUSE_TOPDIR_FRAGMENT 2
#define VDIFUSE_TOPDIR_SEQUENCE 3
extern void vdifuse_topdir(int which, struct stat *stbuf);

/* readdir support: provide index (name and stbuf) of next fragment */
extern int get_vdifuse_fragment(int index, char **name, struct stat **stbuf);
extern int get_vdifuse_sequence(int index, char **name, struct stat **stbuf);

/* for ffi_info.errors */
#define VDIFUSE_FFIERROR_NONE   0
#define VDIFUSE_FFIERROR_EOF    1
#define VDIFUSE_FFIERROR_READ   2
#define VDIFUSE_FFIERROR_RPATH  3

/* open, release and read support */
typedef struct ffi_info {
    uint64_t        fh;     /* fuse_file_info.fh */
    int             flags;  /* fuse_file_info.flags */
    size_t          size;   /* last requested read size */
    off_t           offset; /* last requested read offset */
    uint32_t        sindex; /* sequence entry index */
    uint32_t        stype;  /* subtype of sequence */
    int             numb;   /* of sequence members */
    struct timeval  topen;  /* when file was opened */
    uint64_t        totrb;  /* total bytes read */
    int             errors; /* for support on errors */
    void            *sdata; /* private per-subtype state data */
    void            *sfrag; /* private per-subtype per-fragment data */
    void            *cntxt; /* private per-file signal context data */
} FFInfo;
extern int vorrfd, realfd;  /* reserved fd in info cache */
extern int vorr_init(void);
extern int vorr_open(const char *fusepath, FFInfo *ffi);
extern int vorr_release(const char *fusepath, FFInfo *ffi);
extern int vorr_read(const char *fusepath, char *buf, FFInfo *ffi);

/* support for the sequence hierarchy */
extern int get_sequence_subdir(const char *path);
extern int get_vdifuse_subdir(int index, char **name, struct stat **sb);

/* external access to the cache for (careful) manipulations */
VDIFUSEntry *current_cache_start(void);
uint32_t current_cache_entries(void);
/* this one returns a pointer to a free entry at the end */
VDIFUSEntry *create_cache_entry(void);
/* this one returns a pointer to the parameters structure */
VDIFUSEpars *current_cache_pars(void);

void describe_cache_entry(VDIFUSEntry *vp);

/* defined values for the etype in a VMDCentry */
#define VDIFUSE_ENTRY_INVALID   0   /* allows for holes in the array */
#define VDIFUSE_ENTRY_FRAGMENT  1   /* reference to single vdif file */
#define VDIFUSE_ENTRY_SEQUENCE  2   /* fragments makes complete file */
#define VDIFUSE_ENTRY_DIRECTORY 3   /* a directory within fuse mount */
#define VDIFUSE_ENTRY_PARAMS    4   /* parameters used to find files */
#define VDIFUSE_ENTRY_ANCILLARY 5   /* ancillary data with any above */

/* defined values for the stype in a VMDCentry */
#define VDIFUSE_STYPE_NULL      0   /* (Fragment) unused */
#define VDIFUSE_STYPE_VDIF      1   /* (Fragment) unblocked VDIF */
#define VDIFUSE_STYPE_SGV2      2   /* (Fragment) SGver2 blocked VDIF */
#define VDIFUSE_STYPE_INFO      3   /* (Ancillary) for SGver2 access */
#define VDIFUSE_STYPE_SDIR      4   /* (Ancillary) subdir of subdir */
#define VDIFUSE_STYPE_PART      5   /* (Ancillary) of sequence */
#define VDIFUSE_STYPE_NDEF      6   /* not (yet) defined */
//#define VDIFUSE_STYPE_BLKV    7   /* ## blocked VDIF */

/* perform any closeout operations */
extern int vdifuse_finish(void);

/* fragment survey evaluations */
#define VDIFUSE_RIGOR_NOCHECK   0x000001 /* consider everything valid */
#define VDIFUSE_RIGOR_SUFFIX    0x000010 /* <whateveritmightbe>.vdif */
#define VDIFUSE_RIGOR_ENAME     0x000100 /* <exp>_<sc>_<scan><ignore>.vdif */
#define VDIFUSE_RIGOR_MAGIC     0x001000 /* magic number in file (sg2) */
#define VDIFUSE_RIGOR_MINSIZE   0x010000 /* stat the file for min size */
#define VDIFUSE_RIGOR_REGEX     0x100000 /* regex exclude/include used */
/* for printing what we have */
#define VDIFUSE_RIGOR_HPRINTF    "%06x"
#define VDIFUSE_RIGOR_PRINTF    "[%06x]"
extern int vdif_rigor_frag(char *path, VDIFUSEpars *pars);

/* populate vfuse entry for a valid file */
extern int create_fragment_vfuse(VDIFUSEntry *vc, int index,
    VDIFUSEpars *pars, int rigor);

/* these are in vdifseq.c, but that maybe can be split */

/* parsing of canonical names */
extern int create_fragtree(void);

/* support for sequence creation */
extern int create_sequences(void);

/* sg access wrappers */
extern uint64_t sg_signature(uint32_t *vh);

/* this is mostly for development */
extern int sg_info_size(void);

/* attach ancillary access data fragments */
extern void attach_sgv2_anc(uint32_t index);
/* provide a description thereof */
extern int describe_ancillary_sgv2(VDIFUSEntry *vc);
extern char *describe_stype(int stype);

/* work out size of sgv2 sequences */
extern int finalize_sgv2_sequence(VDIFUSEntry *vc);
/* vorr support */
extern int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_sgv2_seq(FFInfo *ffi);
extern int read_sgv2_seq(char *buf, FFInfo *ffi);

/* work out size of flat sequences */
extern int finalize_vdif_sequence(VDIFUSEntry *vc);
/* vorr support */
extern int open_flat_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_flat_seq(FFInfo *ffi);
extern int read_flat_seq(char *buf, FFInfo *ffi);

/* random utility */
extern double secs_since(struct timeval *when);

/* regex utilties */
extern void regexhelp(FILE *fp);
extern void regexfinal(void);
extern int regexclude(char *patt);
extern int reginclude(char *patt);
extern int regexcludefile(char *file);
extern int regincludefile(char *file);
extern int regexcheck(char *path);

/* trace utilities */
extern void vdifuse_mktrace(char *c, char *m);
extern void vdifuse_rmtrace(int rv);
extern void vdifuse_trace(char *fmt, ...);
extern void vdifuse_flush_trace(void);
extern void vdifuse_bread(char *fmt, ...);
extern void vdifuse_flush_bread(void);
#define S1(X) #X
#define S2(X) S1(X)
#define VDT(FMT)  (__FILE__ ":" S2(__LINE__) " " FMT)

#endif /* vdifuse_h */

/*
 * eof
 */
