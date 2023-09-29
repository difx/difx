/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifuse.h 5810 2023-04-04 22:24:59Z gbc $
 *
 * This file provides support for the fuse interface
 * ## items are not fully implemented
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
#error "VDIFUSE_VERSION needs to be supplied by Makefile"
#endif /* VDIFUSE_VERSION */

/* acceptable are [VDIFUSE_VERSION-VDIFUSE_VERANGE .. VDIFUSE_VERSION] */
#define VDIFUSE_VERANGE 0.02

#define VDIFUSE_MAX_PATH 256
#define VDIFUSE_SEARCH_MAX 9999     /* max packet size */
#define VDIFUSE_ONE_SEC_NS (int64_t)1000000000
#define VDIFUSE_MAX_HIER 5
#define VDIFUSE_MIN_FILE_SIZE 60000

/*
 * Mark6 has 4 slots x 8 disks or 32 fragments--will need SGInfo on each--and
 * SGInfo is 224B (currently, see sg_access.h); 56*(4/8) = 28 which x8 is 224
 *
 * sg_sizes.c is a check function to verify these numbers.
 */
#define VDIFUSE_MAX_SEQI (56)
#define VDIFUSE_NUM_VOIDS (VDIFUSE_MAX_SEQI*sizeof(uint32_t)/sizeof(void*))
#define VDIFUSE_TOPDIR_SIZE 16

/* vthread support */
#define VDIFUSE_MAX_VTHREADS 16
/* need a legal value to flag garbage */
#define VTHREAD_BOGUS        0xFFFF
/* upper bits of vthread entry */
#define VTHREAD_FLAG_IGNORE  0x00000
#define VTHREAD_FLAG_SINGLE  0x10000
#define VTHREAD_FLAG_MIXED   0x20000

/* ns timing on files -- one of these appears to be set in stdio.h */
#if !(defined(_BSD_SOURCE) || defined(_SVID_SOURCE) || defined(_POSIX_C_SOURCE))
#error "none of _BSD_SOURCE, _SVID_SOURCE or _POSIX_C_SOURCE was defined"
#endif

/* top-level subdirs of primary mount */
#define VDIFUSE_FRAGMENTS "/fragments"
#define VDIFUSE_SEQUENCES "/sequences"
#define VDIFUSE_VPROCDIR  "/vdifproc"
#define VDIFUSE_VTHREADS  "/vthreads"
extern char fragments_topdir[VDIFUSE_TOPDIR_SIZE];
extern char sequences_topdir[VDIFUSE_TOPDIR_SIZE];
extern char vthreads_topdir[VDIFUSE_TOPDIR_SIZE];
extern char vdifproc_topdir[VDIFUSE_TOPDIR_SIZE];
extern size_t fragments_topdir_len;
extern size_t sequences_topdir_len;
extern size_t vthreads_topdir_len;
extern size_t vdifproc_topdir_len;

/* realpath to the vdifuse mount point */
extern char *vdifuse_mount_realpath;
extern size_t vdifuse_mount_realpath_len;

/* a global choice on verbosity */
extern int vdifuse_debug;

/* set when calling fuse_main() makes sense */
extern int vdifuse_enable;
#define VDIFUSE_ENABLE_SKIP 0
#define VDIFUSE_ENABLE_DOIT 1
#define VDIFUSE_ENABLE_HELP 2

/* protect the cache against accidental erasure */
extern int vdifuse_protect;

/* allow the mount point to be forcibly reused */
extern int vdifuse_reuse;

/* original homogeneous case has both of these as 0 */
/* allow vthread directory logic to be accessed: 0 or 1 (VGOS) */
extern int vthreads_dir;
/* allow vthreads mixed into sequences: 0 or 1 (NOEMA) */
extern int vthreads_seq;

/* prefix for filenames as set by the -m option */
extern char *filelistprefix;

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

/*
 * This structure holds a variety of cache parameter set at
 * creation.  The reserved areas force alignment and make
 * the size consistent with other members of vdifuse_union.
 * These are set to marker values in vdifsup.c clvpo.
 */
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
    uint32_t    vthr_per_seq;       /* vthreads allowed per sequence */
    uint32_t    reserved_tre;       /* reserved for future use */
    /* topdir names for fragments, sequences and threads */
    char        frag_top_dir[VDIFUSE_TOPDIR_SIZE];
    char        seqs_top_dir[VDIFUSE_TOPDIR_SIZE];
    char        proc_top_dir[VDIFUSE_TOPDIR_SIZE];
    char        vthr_top_dir[VDIFUSE_TOPDIR_SIZE];
    /* discovered information */
    uint32_t    maxfrcounter;       /* largest frame count seen */
    uint32_t    est_pkt_rate;       /* estimated rate */
    uint32_t    vthreadseper;       /* vthreadsep of the frags */
    struct timeval  creation;       /* time of cache creation */
    /* provisional vthread support: requested, found, viable */
    uint16_t    vthread_list[VDIFUSE_MAX_VTHREADS];
    uint16_t    reserved_fiv[VDIFUSE_MAX_VTHREADS];
} VDIFUSEpars;

/*
 * Need to store entries as follows:
 *  PARAMS:    (vpars) general cache parameters
 *  FRAGMENT:  (vfuse) non struct stat per-file data (for fragments)
 *  DIRECTORY: (vfuse) subdirectory information
 *  SEQUENCE:  (vseqi) storage for sequences, follows DIRECTORY
 *  VTHREAD:   (vseqi) storage for threads, follows SEQUENCE
 *  ANCILLARY: (voids) ancillary data (SGInfo), follows FRAGMENT
 *  INVALID:   (voids) empty entries in cache to ignore
 * 
 */
typedef struct vdifuse_entry {
    uint32_t    index;                      /* index in array of entries */
    uint8_t     etype;                      /* see VDIFUSE_ENTRY_* above */

    /* need a union over additional stuff */
    uint8_t     stype;                      /* S(ub)TYPE frag/seq/anc */
    uint32_t    vthread;                    /* if single vthreads here */
    uint16_t    reserved;
    uint64_t    vsig;                       /* vdif signature of frag/seq */
    uint32_t    cindex;                     /* continuation index */
    uint32_t    ccount;                     /* continuation count */

    union vdifuse_union {
        struct stat vfuse;                  /* stat buf provided to fuse */
        VDIFUSEpars vpars;                  /* internal cache parameters */
        uint32_t vseqi[VDIFUSE_MAX_SEQI];   /* indices of the sequence */
        void *voids[VDIFUSE_NUM_VOIDS];     /* ancillary data (SGInfo) */
    } u;

    char        path[VDIFUSE_MAX_PATH];     /* absolute real path to entry */
    char        fuse[VDIFUSE_MAX_PATH];     /* absolute fuse path to entry */
    char        hier[VDIFUSE_MAX_PATH];     /* hierarchy placement of frag */
} VDIFUSEntry;

/* descriptive functions -- nonzero return if entry is defective */
/* extern int describe_vprocdir(VDIFUSEntry *vp); */
extern int describe_struct(void);
extern int describe_params(VDIFUSEntry *vp);
extern int describe_fragment(VDIFUSEntry *vp);
extern int describe_sequence(VDIFUSEntry *vp);
extern int describe_vthreads(VDIFUSEntry *vp);
extern int describe_directory(VDIFUSEntry *vp);
extern int describe_ancillary(VDIFUSEntry *vp);
/* a switch into the above, ignoring checking */
extern void describe_cache_entry(VDIFUSEntry *vp);
/* alternate call */
extern void describe_cache_params(void);

/* getattr support: return 0 if path is a fragment, sequence... */
extern int vdifuse_fragment(const char *path, struct stat *stbuf);
extern int vdifuse_sequence(const char *path, struct stat *stbuf);
extern int vdifuse_vprocdir(const char *path, struct stat *stbuf);
extern int vdifuse_vthreads(const char *path, struct stat *stbuf);
/* the start of the cache has a fixed layout */
#define VDIFUSE_TOPDIR_PARAMS   0
#define VDIFUSE_TOPDIR_ROOT_DIR 1
#define VDIFUSE_TOPDIR_FRAGMENT 2
#define VDIFUSE_TOPDIR_SEQUENCE 3
#define VDIFUSE_TOPDIR_VPROCDIR 4
#define VDIFUSE_TOPDIR_VTHREADS 5
/* first dynamic entry in the cache */
#define VDIFUSE_TOPDIR_VARIABLE (VDIFUSE_TOPDIR_VTHREADS+1)
extern void vdifuse_topdir(int which, struct stat *stbuf);

/* readdir support: provide index (name and stbuf) of next fragment */
extern int get_vdifuse_fragment(int index, char **name, struct stat **stbuf);
extern int get_vdifuse_sequence(int index, char **name, struct stat **stbuf);
extern int get_vdifuse_vprocdir(int index, char **name, struct stat **stbuf);
extern int get_vdifuse_vthreads(int index, char **name, struct stat **stbuf);

/* for ffi_info.vffers */
#define VDIFUSE_FFIERROR_NONE   0x0000
#define VDIFUSE_FFIERROR_EOF    0x0001
#define VDIFUSE_FFIERROR_READ   0x0002
#define VDIFUSE_FFIERROR_BEFORE 0x0004
#define VDIFUSE_FFIERROR_AFTER  0x0008
#define VDIFUSE_FFIERROR_RPATH  0x0010

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
    int             vffers; /* for vdif fuse file errors */
    int             vpendx; /* index in the vproc area for orr */
    int             vparnt; /* index of vproc subdir parent */
    void            *sdata; /* private per-subtype state data */
    void            *sfrag; /* private per-subtype per-fragment data */
    void            *cntxt; /* private per-file signal context data */
} FFInfo;
extern int vorrfd, realfd;  /* reserved fd in info cache */
extern int vorr_init(void);
extern int vorr_open(const char *fusepath, FFInfo *ffi);
extern int vorr_release(const char *fusepath, FFInfo *ffi);
extern int vorr_read(const char *fusepath, char *buf, FFInfo *ffi);

/* support for the sequence (or vthreads) hierarchy */
extern int get_sequence_subdir(const char *path);
extern int get_vdifuse_subdir(int index, char **name, struct stat **sb);
extern int get_vprocdir_subdir(const char *path);
extern int get_vthreads_subdir(const char *path);

/* external access to the cache for (careful) manipulations */
VDIFUSEntry *current_cache_start(void);
uint32_t current_cache_entries(void);
/* flag the cache as having been modified */
void increment_cache_modified(void);
/* this one returns a pointer to a free entry at the end */
VDIFUSEntry *create_cache_entry(void);
/* this one returns a pointer to the parameters structure */
VDIFUSEpars *current_cache_pars(void);

/* some basic cache functions that do what they say */
int create_cache(char *cache);
int load_cache(char *cache);
int dump_cache(void);
int populate_cache(int ndirs, char **dirs);

/* additional functions */
int create_root_entry(struct stat *mp);
int create_fragments_entry(struct stat *mp);
int create_sequences_entry(struct stat *mp);
int create_vprocdir_entry(struct stat *mp);
int create_vthreads_entry(struct stat *mp);

void describe_cache_entry(VDIFUSEntry *vp);

/* defined values for the etype in a VDIFUSEntry  */
#define VDIFUSE_ENTRY_INVALID   0   /* allows for holes in the array */
#define VDIFUSE_ENTRY_FRAGMENT  1   /* reference to single vdif file */
#define VDIFUSE_ENTRY_SEQUENCE  2   /* fragments makes complete file */
#define VDIFUSE_ENTRY_DIRECTORY 3   /* a directory within fuse mount */
#define VDIFUSE_ENTRY_PARAMS    4   /* parameters used to find files */
#define VDIFUSE_ENTRY_ANCILLARY 5   /* ancillary data with any above */
#define VDIFUSE_ENTRY_VPROCDIR  6   /* TBD */
#define VDIFUSE_ENTRY_VTHREADS  7   /* TBD */

/* defined values for the stype in a VDIFUSEntry  */
#define VDIFUSE_STYPE_NULL      0   /* (Fragment) unused */
#define VDIFUSE_STYPE_VDIF      1   /* (Fragment) unblocked VDIF */
#define VDIFUSE_STYPE_SGV2      2   /* (Fragment) SGver2 blocked VDIF */
#define VDIFUSE_STYPE_INFO      3   /* (Ancillary) for SGver2 access */
#define VDIFUSE_STYPE_SDIR      4   /* (Ancillary) subdir of subdir */
#define VDIFUSE_STYPE_PART      5   /* (Ancillary) of sequence */
#define VDIFUSE_STYPE_NDEF      6   /* not (yet) defined */
//#define VDIFUSE_STYPE_BLKV    7   /* ## blocked VDIF * /

/* perform any closeout operations */
extern int vdifuse_finish(void);

/* fragment survey evaluations */
#define VDIFUSE_RIGOR_NOCHECK   0x00000001 /* consider everything valid */
#define VDIFUSE_RIGOR_SUFFIX    0x00000010 /* <whateveritmightbe>.vdif */
#define VDIFUSE_RIGOR_ENAME     0x00000100 /* <exp>_<sc>_<scan><ignore>.vdif */
#define VDIFUSE_RIGOR_MAGIC     0x00001000 /* magic number in file (sg2) */
#define VDIFUSE_RIGOR_MINSIZE   0x00010000 /* stat the file for min size */
#define VDIFUSE_RIGOR_REGEX     0x00100000 /* regex exclude/include used */
#define VDIFUSE_RIGOR_VTHRSEQ   0x01000000 /* acceptable to vthreads_seq */
#define VDIFUSE_RIGOR_VTHRDIR   0x10000000 /* acceptable to vthreads_dir */
/* for printing what we have */
#define VDIFUSE_RIGOR_HPRINTF    "%08x"
#define VDIFUSE_RIGOR_PRINTF    "[%08x]"
extern int vdif_rigor_frag(char *path, VDIFUSEpars *pars);

/* populate vfuse entry for a valid file */
extern int create_fragment_vfuse(VDIFUSEntry *vc, int index,
    VDIFUSEpars *pars, int rigor);

/* these are in vdifseq.c, but that maybe can be split */

/* parsing of canonical names */
extern int create_fragtree(void);

/* support for sequence creation */
extern int create_sequences(void);

/* this is mostly for development */
extern int sg_info_size(void);

/* attach ancillary access data fragments */
extern void attach_sgv2_ancillary(uint32_t index);
/* provide a description thereof */
extern int describe_ancillary_sgv2(VDIFUSEntry *vc);
extern char *describe_stype(int stype);

/* work out size of sgv2 sequences */
extern int finalize_sgv2_sequence(VDIFUSEntry *vc);
/* vorr support */
extern int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_sgv2_seq(FFInfo *ffi);
extern int read_sgv2_seq(char *buf, FFInfo *ffi);
extern void update_sgv2_seq(FFInfo *ffi);

/* work out size of flat sequences */
extern int finalize_vdif_sequence(VDIFUSEntry *vc);
/* vorr support */
extern int open_flat_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_flat_seq(FFInfo *ffi);
extern int read_flat_seq(char *buf, FFInfo *ffi);

/* regex utilties */
extern void regexhelp(void);
extern void regexfinal(void);
extern int regexclude(char *patt);
extern int reginclude(char *patt);
extern int regexcludefile(char *file);
extern int regincludefile(char *file);
extern int regexcheck(char *path);

/* trace utilities */
extern void vdifuse_mktrace(char *c, char *m);
extern void vdifuse_rmtrace(int rv);
extern void vdifuse_marker(char *what);
extern void vdifuse_flush_trace(void);
extern void vdifuse_bread(char *fmt, ...);
extern void vdifuse_flush_bread(char *where);
extern char *vdifuse_tracename(void);
extern char *vdifuse_breadname(void);

/* for fprintf(getvdiflog() .. */
extern void setvdiflog(char *);
extern FILE *getvdiflog(void);

/* used to set readfd in vdiforr.c */
extern int getvdiflogfileno(void);

/* the main logging functions */
extern void vdifuse_trace(char *fmt, ...);

/* if vdifuse_debug<=gate, these return immediately */
extern void vdiftrace(int gate, char *fmt, ...);
extern void vdiflog(int gate, char *fmt, ...);

/* this posts messages to the "errors" vprocdir file */
extern void vdiferror(const FFInfo *ffi, char *fmt, ...);

/* vproc dir tracing, top-level vanilla routines */
extern void report_vproc(const char *where);
extern char *vproc_realpath(const char *fusepath, FFInfo *ffi);
extern int vproc_read(char *buf, FFInfo *ffi);
extern void vproc_status_subdir(FFInfo *ffi, const char *base,
    const char *info);
extern void vproc_update_file(const FFInfo *ffi, const char *base,
    const char *inf, int create);

/* bits for the create flag */
#define VPROC_IF_FOUND  0x0     /* update only if it exists */
#define VPROC_TRUNCATE  0x1     /* (make) and replace contents */
#define VPROC_APPENDIF  0x2     /* append only if it is found */
#define VPROC_LENGTHEN  0x3     /* (make) and append contents */

/* macro games to make VDT be simple */
#define S1(X) #X
#define S2(X) S1(X)
/* this prepends file:line to the fmt */
#define VDT(FMT)  (__FILE__ ":" S2(__LINE__) " " FMT)
/* this prepends file:line to a label */
#define VDX(LAB)  (__FILE__ ":" S2(__LINE__) " " LAB)
/* this will generate a two-liner */
#define VDY(FMT)   __FILE__ ":" S2(__LINE__) " " FMT
#define VD3(F,G)  VDY(F) "               " VDY(G)

/*
 * macro games for efficiency in gated logging, in ISO 1999 C standard
 * when G is less than VDIFTRACE_MAX then the code should disappear.
 */
#define VDIFTRACE_MAX   2
/* this one is always a statement */
#define vdiftrace(G, ...)                               \
    do {if(((G)<VDIFTRACE_MAX) && (vdifuse_debug>(G)))  \
          vdifuse_trace(__VA_ARGS__);                   \
    } while(0)
/* this one is sometimes an expression
#define vdiflog(G, ...)                             \
    ((G)<VDIFTRACE_MAX && vdifuse_debug>(G) &&      \
        fprintf(getvdiflog(), __VA_ARGS__))
*/

#endif /* vdifuse_h */

/*
 * eof
 */
