/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: sg_access.h 5770 2023-03-26 22:00:02Z gbc $
 *
 * Code to understand and access sg(2) files efficiently.
 *
 * The code is a bit contorted as it was developed for the original
 * sg version; issues with that resulted in the sg2 version which is
 * at this point the only one that needs to be supported.  Some of
 * the tools and code are adapted from the burst mode recorder and
 * earlier (non-scatter gather) Mark6 prototypes.
 */

#ifndef sg_access_h
#define sg_access_h

#include <stdio.h>
#include <stdint.h>
#include <sys/types.h>
#include "vdif.h"

/*
 * A structure to hold mmap-d info while the file is open
 * This structure is created by mm_init() and cleared by mm_term().
 * The users field allows multiple users to share the mapping in
 * the same program.  (The mapping is MAP_SHARED, so different
 * processes can share it through the kernel as well.)
 */
typedef struct sg_mmap_info {
    off_t       size;           /* size of the file in bytes */
    void        *start;         /* starting offset, non-zero if in use */
    void        *eomem;         /* end of valid memory, non-zero if in use */
    int         mmfd;           /* file descriptor >=0 if in use */
    int         users;          /* number of users for this mapping */
} SGMMInfo; 

/*
 * Supporting all legal 2^16 threads requires considerably more storage.
 * The NOEMA and VGOS use cases require only 4 threads.  The SGInfo size
 * has been 224 and as of rev 0.40, no unused bytes.
 */
#define MAX_VDIF_THREADS    16
#define VTHREP_BUFFER       260
#define MAX_LEGAL_THREAD_ID 32767

/*
 * A structure to hold what we can easily find out about an SG fragment
 */
typedef struct sg_info {
    /* general support */
    SGMMInfo    smi;            /* memory mapping information */
    double      eval_time;      /* open: time opened, closed: time open */
    char        *name;          /* a malloc'd copy of the file name */

    /* vdif general */
    uint32_t    total_pkts;     /* total number of packets */
    uint32_t    pkt_size;       /* VDIF packet size */
    uint32_t    pkt_offset;     /* offset into packet */
    int32_t     read_size;      /* total packet + overhead */
    uint32_t    ref_epoch;      /* reference epoch */
    uint32_t    first_secs;     /* seconds of epoch of first packet */
    uint32_t    first_frame;    /* frame counter of first packet */
    uint32_t    final_secs;     /* seconds of epoch of final packet */
    uint32_t    final_frame;    /* frame counter of final packet */
    uint32_t    frame_cnt_max;  /* maximum frame counter value seen */
    VDIFsigu    vdif_signature; /* header signature (union w/ uint64_t) */
    uint32_t    checksum;       /* intended to be unique per fragment */
    int         verbose;        /* diagnostic on internal processing */

    /* thread handling */
    short       vthreads[MAX_VDIF_THREADS];      /* vthreads */
    short       nvthreads;      /* as enumerated in vthreads */
    short       vthreadsep;     /* separation of vthreads */
    short       padding[2];

    /* sg version 2 */
    int32_t     sg_version;
    uint32_t    sg_fht_size;    /* size of file header tag */
    uint32_t    sg_wbht_size;   /* write block header tag size */
    uint32_t    sg_wr_block;    /* standard write block size */
    uint32_t    sg_wr_pkts;     /* pkts in standard write block */
    uint32_t    sg_wr_blks_bs;  /* number of write blocks before sh.blk */
    uint32_t    sg_wr_blks_as;  /* number of write blocks after sh.blk */
    uint32_t    sg_wr_pkts_bs;  /* pkts in normal wbs before sh.blk */
    uint32_t    sg_wr_pkts_as;  /* pkts in normal wbs after sh.blk */
    uint32_t    sg_sh_block;    /* starter (short) write block size */
    off_t       sg_sh_blk_off;  /* sh.blk offset in the file */
    uint32_t    sg_sh_pkts;     /* pkts in starter (short) write block */
    uint32_t    sg_se_block;    /* sh.end write block size */
    off_t       sg_se_blk_off;  /* sh.end offset in the file */
    uint32_t    sg_se_pkts;     /* pkts in ending (short) write block */
    uint32_t    sg_total_blks;  /* total number of blocks */
    /* for tracking via dplane blocknum(ber)s: */
    int         sg_first_bnum;  /* from first wb_header_tag */
    int         sg_final_bnum;  /* from final wb_header_tag */
} SGInfo;

/* Magic number of SG */
#define SG_VERSION_MAGIC    0xfeed6666
#define SG_MAX_VDIF_BYTES   32000
#define SG_MIN_VDIF_BYTES   64
#define SG_FR_CNT_MAX       16777216

/*
 * These are defined in dplane.h and reformatted here with adjusted comments.
 * The file starts with a 28B header; thereafter an 8B header each block.
 * Life would have been so much easier had both been set to the packet size
 * as the blocking of the fragments could be done at the "packet" level.
 */
typedef struct file_header_tag {    /* file header - one per file */
    unsigned int sync_word;         /* SG_VERSION_MAGIC */
    int version;                    /* this had better be 2 */
    int block_size;                 /* length of blocks with header (Bytes) */
    int packet_format;              /* only VDIF (0) is supported */
    int packet_size;                /* length of packets (Bytes) */
} SGV2Header;
typedef struct wb_header_tag {      /* write block header - one per block */
    int blocknum;                   /* block number, starting with 0 */
    int wb_size;
} SGV2BlkNum;
/* In theory dplane only writes values > 0 */
#define SG_BLOCKNUM_ILLEGAL          0
#define SG_BLOCKNUM_BADPKT          -1      /* sg_pkt_by got no packet */
#define SG_BLOCKNUM_NONEXT          -2      /* block has no followers */
#define SG_BLOCKNUM_INIT            -3      /* blocknum not calculated */
#define SG_BLOCKNUM_NOPREV          -4      /* block has no predecessors */
#define SG_BLOCKNUM_BOGUS           -7      /* blocknum not calculated */

/* positive values >=2 for sg_version are scatter-gather versions */
#define SG_VERSION_OK_2             2
/* unstructured files of VDIF packets describable with the above */
#define SG_VERSION_FLAT             1
/* none of the above */
#define SG_VERSION_NOT              0
/* negative values are for errors encountered during SG analysis */
#define SG_VERSION_NOT_ENOUGH_DATA -1
#define SG_VERSION_SIG_FIRST_FAIL  -2
#define SG_VERSION_SIG_FINAL_FAIL  -3
#define SG_VERSION_SIG_SEBLK_FAIL  -4
#define SG_VERSION_SIG_SEBLK_BAIL1 -5
#define SG_VERSION_SIG_SEBLK_BAIL2 -6
#define SG_VERSION_SIG_SEBLK_BAIL3 -7
#define SG_VERSION_SIG_SHBLK_FAIL1 -8
#define SG_VERSION_SIG_SHBLK_FAIL2 -9
#define SG_VERSION_SIG_SHBLK_BAIL  -10
#define SG_VERSION_SIG_SHBLK_BLOW  -11
#define SG_VERSION_SIG_SHBLK_BUPP  -12
#define SG_VERSION_SIG_SDBLK_BAIL1 -13
#define SG_VERSION_SIG_SDBLK_BAIL2 -14
/* returns a short string explaining the above */
extern char *sg_error_str(int err);

/* default is to check all bits of the station id */
#define SG_STATION_MASK 0xFFFF

/*
 * Methods to open an SG file for use.
 *   sg_access()    opens the file, understands it, and closes it
 *   sg_open()      opens the file, and if sgi is non-null calls
 *   sg_info()      to understand the file opened in smi
 *   sg_close()     closes a file opened with sg_open()
 *   sg_reopen()    assumes sgi is valid and opens sgi->name
 *
 * The SGMMInfo pointer returned is to sgi->smi or a NULL pointer.
 */
extern void sg_access(const char *file, SGInfo *sgi);
extern void sg_info(const char *file, SGInfo *sgi);
extern SGMMInfo *sg_open(const char *file, SGInfo *sgi);
extern SGMMInfo *sg_reopen(SGInfo *sgi);
extern void sg_close(SGInfo *sgi);

/*
 * Allows updates to first and final with revised knowledge of
 * maximum frame count seen (i.e. packet rate - 1)
 */
extern void sg_first_time_check(SGInfo *sgi);
extern void sg_final_time_check(SGInfo *sgi);

/*
 * After sgi->checksum = sg_checksum(sgi), sg_checksum(sgi)
 * should return 0 when the correct fragment is loaded.  If
 * the file is not open, garbage will be returned.
 */
extern uint32_t sg_checksum(SGInfo *sgi);

/*
 * Random access methods to the packets in the file.
 * Each returns a pointer to the requested packet and if end is not NULL,
 * sets it to the end of the block.  Likewise if nl is not NULL, it will
 * contain the number of packets left in the block,
 * In the event of exceptional conditions, NULL32P is returned, and
 * and end is as well; the other variables also have detectable returns:
 * prevp, thisp and nextp will be SG_BLOCKNUM_INIT and the before and
 * after bytes counts will be zero.  Alernatively SG_BLOCKNUM_NOPREV and
 * SG_BLOCKNUM_NONEXT are supplied to prevp and nextp when that is so.
 * For sane files pktbytesbefore+pktbytesafter should never be zero.
 */
#define NULL32P ((uint32_t *)0)
/* this one goes directly to a packet by number */
extern uint32_t *sg_pkt_by_num(SGInfo *sgi, off_t np, int *nl, uint32_t **end);

/* the original "by blk" random access method to the first packet of block */
extern uint32_t *sg_pkt_by_blk(SGInfo *sgi, off_t nb, int *nl, uint32_t **end);

/*
 * a variant of sg_pkt_by_blk that also provides the blocknum(ber)s
 * for the block in question, the preceding block, and the following
 * block.
 */
extern uint32_t *sg_pkt_by_blknm(SGInfo *sgi, off_t nb, int *nl,
    uint32_t **end, int *prevp, int *thisp, int *nextp);

/*
 * a variant of sg_pkt_by_blk that also returns (packet) byte counts
 * before and after the block, but not including the block
 */
extern uint32_t *sg_pkt_blkby(SGInfo *sgi, off_t nb, int *nl,
    off_t *pktbytesbefore, off_t *pktbytesafter);

/*
 * This is the variant that computes all of the above...
 * ...and a debugging version that writes into a buffer.
 */
extern uint32_t *sg_pkt_by_blkall(SGInfo *sgi, off_t nb, int *nl,
    uint32_t **end, off_t *pktbytesbefore, off_t *pktbytesafter,
    int *prevp, int *thisp, int *nextp);
extern uint32_t *sg_pkt_by_blkall_dbg(SGInfo *sgi, off_t nb, int *nl,
    uint32_t **end, off_t *pktbytesbefore, off_t *pktbytesafter,
    int *prevp, int *thisp, int *nextp, char *buff, size_t bsz);

/* this one finds the packet nearest to a given offset */
extern uint32_t *sg_pkt_by_off(SGInfo *sgi, off_t of, int *nl, uint32_t **end);

/*
 * Diagnostic methods to check the signatures on packets found by the above.
 * The _check version returns the number of packets failing the signature
 * check (0 through nl, inclusive), or the end-of-block check.
 * The _times version returns nonzero if there are issues with time ordering.
 * For single-vthreaded data this means one of three errors in sec/frame
 * counters.  For multi-vthreaded data this means only that the different
 * threads are out of time sequence.
 */
extern int seq_pkt_check(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end,
    int *nv);
extern int seq_pkt_times(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end);

/*
 * A diagnostic method to describe the SGInfo contents: stdout or buffer
 * If label is not NULL, it is inserted at the beginning of each line.
 */
extern void sg_report(SGInfo *sgi, char *label);
extern char *sg_repstr(SGInfo *sgi, char *label);

/* Computes the vdif header signature from a pointer to 1st header word */
extern uint64_t sg_signature(uint32_t *vh);
extern uint64_t sg_get_vsig(uint32_t *vhp, void *o, int verb,
    char *lab, VDIFsigu *vc, short *thp);
extern void sg_set_station_id_mask(int mask);
extern int sg_get_station_id_mask(void);

/* Some hooks for vthreads; report should be at least VTHREP_BUFFER */
extern int sg_vthreads(short *thp, short tid, char *report);
extern char *sg_vthreads_rep(short *thp);
extern void sg_set_max_legal_vthread_id(int max);
extern int sg_get_max_legal_vthread_id(void);

/* Extra effort if file starts out invalid */
extern void sg_set_user_poff_and_size(const char *arg);

/* Returns vextime for seconds in a ref epoch */
extern char *sg_vextime(int re, int secs);

/* Returns time since previous, otherwise current */
extern double sg_current_or_elapsed_secs(double *previous);
extern void sg_secs_since(struct timeval *when, double *secs);

/* For reconnecting stdout to something else */
extern void sg_logger(FILE *fp);
extern FILE *sg_logfile(void);

/* For optimizing read performance */
extern void sg_advice(SGInfo *sgi, void *pkt, int dir);
extern void sg_advice_term(int mmfd);
extern void spawn_readahead_thread(int fd, off_t addr, size_t len, size_t mx);
extern void spawn_toucher_thread(int fd, void *a, size_t l, long pg);

#endif /* sg_access_h */

/*
 * eof
 */
