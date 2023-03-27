/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsg2.h 5751 2023-03-26 15:10:20Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides glue to the sg_access library
 * in order to support random access read to sgv2 fragments
 * which when combined form the complete vdif sequence.
 */

#ifndef vdifsg2_h
#define vdifsg2_h

#include "vdifuse.h"
#include "sg_access.h"

/*
 * We maintain a working "stripe" of SGv2 blocks, sorted by
 * packet times across all the files of the sequence.  Typically,
 * the stripe contains as many (numb) as there are members of the
 * sequence (ffi->numb), but if a member ends early, the "zero"
 * will be advanced and the working stripe will contain one or
 * more fewer members.  Likewise, if there is a gap (because the
 * next block is in the same member as the stripe), we may have
 * a stripe containing fewer valid members with a gap at "sgap".
 *
 * The dplane block numbers are also available and should be
 * roughly consistent with the time order.
 *
 * The read byte offset (ffi->offset) should then lie within the
 * "zero" element of the stripe and proceed through the packets of
 * that block up to "sgap".  sfp is a pointer to a fragment (i.e.
 * an SGV2sfrag structure) and sdp is a pointer to a stripe (i.e.
 * an SGV2sdata structure describing a "stripe" of SBlock's).
 *
 * "zero" is managed in the walking/finding
 * "sgap" is managed in the degapping
 * A move will never move past the last block or the first one.
 * Motion occurs to satisfy read requests.  Several routines
 * are called to verify that all is well with the stripe.
 *
 * For single-threaded sg2 files, time is monotonic and reference
 * to the native sg block numbers is not required.  However when
 * multi-threaded sg files are written, the block numbers are a second
 * source of information about read order.  And if time is horribly
 * corrupted (large numbers of invalid packets) this becomes necessary.
 *
 * The major support functions by call order include:
 * (line numbers are ca. mar 2023).
 *
 * 1592 open_sgv2_seq() calls:
 *  393  member_init(sfp)            initialize one member (fragment)
 *  966  stripe_init(sdp, f)         initializes the stripe at start
 *  934   init_stripe_bnums(sdp)     initialize the bnums in the stripe
 *  646   stripe_sort(sdp)           sort the members of the sequence
 *  587    stripe_comp(a, b)         STRIPE_COMP_HOW == STRIPE_COMP_USE_BE
 *  603                           >  STRIPE_COMP_HOW == STRIPE_COMP_MIDDLE
 *  617                              STRIPE_COMP_HOW == combo
 *  834   stripe_dgap(sdp)           adjust the stripe to remove gaps
 *  889   stripe_boff(sdp)           compute the byte offsets in the stripe
 *  512   stripe_bread(sdp, l)       bread-crumb version of current stripe
 *  465    stripe_bread_fmt(sdp,l)   generates the details string
 *  449    calculate_moves(sdp)      (utility) sum moves across stripe
 *  526   stripe_trace(sdp, l, a)    detailed stripe information
 *  270     member_trace(sfp, ith)   detailed fragment information
 *  662   stripe_schk(sdp)           check the sort of the sequence
 * 1549  generate_fh()               unique file handle
 *
 * 1708 release_sgv2_seq(ffi)
 * 1685  diag_counts()               debugging function
 * 1565  release_fh()                and release of it
 *
 * 1672 update_sgv2_seq(ffi)         setup for vproc_update_file()
 * 1904 read_sgv2_seq() calls:
 * 1819  do_read_sgv2_seq() calls:
 * 1748   is_silly_read(sdp)         sanity checker on read request
 * 1785   shift_for_read(sdp)        makes moves so read succeeds
 * 1733    show_sgv2_state(sdp)      debugging function
 * 1283    stripe_move(sdp)          decide strategy on stripe access
 *   50     packet_time_mono(...)
 *  130     packet_time_seq(...)
 *  212     pkt_time_chk(...)
 * 1252     stripe_jump(sdp)         maybe trigger large stripe movement
 * 1226      stripe_find(sdp, dir)   used when "offset" is outside stripe
 * 1187       stripe_spot(sdp, c)    place a stripe at a set of positions
 *  820        stripe_sqze(sdp)      extra work that is prudent after jumps
 *  700         stripe_zero(sdp)     find the new stripe "zero" (dgap,sqze)
 *  716         predecessors_leave_gap(...)
 *  753         successor_closes_gap(...)
 * 1362    stripe_anal(sdp)          double-check preceding check
 * 1400   stripe_read(sdp, buf, l)   reads to buffer from existing stripe
 *  990    memory_check(...)         sanity checker used by stripe_read
 * 1451   stripe_rdfw(sdp, buf)      read forwards from existing stripe
 * 1499   stripe_rdbw(sdp, buf)      read backwards from existing stripe
 * 1087    stripe_walk(sdp, dir)     used when stripe shifts incrementally
 * 1016     stripe_vern(sdp, b)      vernier bw walk into the bytes needed
 *  292     member_move(sfp,blk,dir) move this member to a new block
 * 
 * Note that packets may be smaller than their read_size, and
 * there are headers in the file, so certain calculations must
 * respect this, but the bytes and sizes for tracking the fuse
 * read() bytes are within the virtual file (i.e. just packets).
 *
 * Errors are tracked on a per-member basis.
 *
 * Frame count is limited to 16777216 so times can be easily handled
 * as seconds + 1e-8*frames (PKTDT); we assume a common epoch, since
 * it is illegal for this to change during an observation.
 * (Note that some noise is introduced by this math.)  A pseudo-time
 * is generated in packet_time() which steps over invalid frames at
 * either end of the block.  Of course, secs_inre in ref epoch is
 * bounded at 30 bits (3.5 bytes with 6 bits spare--apparently the
 * plan was one could dwell at an epoch for some time).
 *
 * NOTION: adjust packet time by thread index.
 * This pseudo-time may be diddled further by the thread index if it
 * is not too large (or if PKTDT is reduced).  Such additions can be
 * thread major (more significant than framecount) or minor (less).
 *
 * In addition to packet times, sg2 blocks have block numbers that
 * can be monitored to ensure that we are doing something sensible.
 * The bnum routines and variables are part of this apparatus that
 * was added as an aid in sorting through some of the issues posed
 * by NOEMA (multiple threads in the same block and thus multiple
 * packets with the same time).
 */

#define PKTDT_INV 100000000
#define PKTDT     1e-8
#define PACKET_TIME(pkt) \
    ((pkt)->w1.secs_inre + (PKTDT)*(pkt)->w2.df_num_insec)

/* for the per-fragment error indicators */
#define SGV2_ERR_VIRGIN     0x0000U
#define SGV2_ERR_REOPEN     0x0001U
#define SGV2_ERR_BLKERR     0x0002U
#define SGV2_ERR_ACCESS     0x0004U
#define SGV2_ERR_TIME_F     0x0010U
#define SGV2_ERR_TIME_B     0x0020U
#define SGV2_ERR_SRTCHK     0x0100U
#define SGV2_ERR_TIME_C     0x0200U
#define SGV2_ERR_TIME_0     0x0400U
#define SGV2_ERR_CHKSUM     0x0800U
#define SGV2_ERR_BLKNUM     0x1000U
#define SGV2_ERR_BLKADR     0x2000U
#define SGV2_ERR_BLKNXT     0x4000U
#define SGV2_ERR_PFGCMX     0x8000U

/* for the stripe diagnostic counters */
#define SGV2_DIAG_BREAD     0
#define SGV2_DIAG_TRACE     1
#define SGV2_DIAG_SHOWN     2
#define SGV2_DIAG_VERN      3
#define SGV2_DIAG_WALK      4
#define SGV2_DIAG_FIND      5
#define SGV2_DIAG_MOVE      6
#define SGV2_DIAG_READ      7
#define SGV2_DIAG_RDFW      8
#define SGV2_DIAG_RDBW      9
#define SGV2_DIAG_DOIT      10
#define SGV2_DIAG_SPARE     11
#define SGV2_DIAG_COUNT     (SGV2_DIAG_SPARE + 1)

/* for 16-disk 10MB blocks, this is about 16 walks */
//#define STRIPE_JUMP_ENABLED 1
#define STRIPE_JUMP_TRIGGER 262144000

/*
 * pmid is (ptbe+pten) / 2 which should be more robust in sort
 * than ptbe which was used in the original implementation.
 * We can also use the block number or both in which case we
 * can tie break (should that happen) on pmid or bnum.
 */
#define STRIPE_COMP_USE_BE  0  
#define STRIPE_COMP_MIDDLE  1
#define STRIPE_COMP_USEBLK  2
#define STRIPE_COMP_HOW     STRIPE_COMP_MIDDLE
#define STRIPE_COMP_PMID    3
#define STRIPE_COMP_BNUM    4
#define STRIPE_COMP_TIEBRK  STRIPE_COMP_BNUM

/* per-fragment lookup data: ffi->sfrag[], created by calloc */
typedef struct sgv2_private_sfrag {
    /* about the fragment */
    SGInfo      *sgi;       /* pointer to cached SGInfo */
    uint32_t    err;        /* mask of access errors */
    double      first;      /* first packet time in member */
    double      final;      /* final packet time in member */
    off_t       mcnt;       /* total move count for this fragment */
    /* about the current block */
    uint32_t    *addr;      /* first packet of the block */
    int         nblk;       /* number of packets in block */
    double      ptbe;       /* first  packet time of block */
    double      pmid;       /* middle packet time of block */
    double      pten;       /* final  packet time of block */
    off_t       cblk;       /* currrent block number in this frag */
    off_t       bybb;       /* total (packet) bytes preceding this block */
    off_t       byib;       /* total (packet) bytes in this block */
    off_t       byab;       /* total (packet) bytes following this block */
    /* for tracking via dplane blocknum(ber)s: */
    int         bnum_prev;  /* previous blocknum in fragment */
    int         bnum;       /* current (SGV2BlkNum) blocknum */
    int         bnum_next;  /* the next blocknum in fragment */
    /* needed for access to vprocdir reporting */
    FFInfo      *fip;       /* pointer to file orr area */
} SGV2sfrag;

/* for managing the active block from each member */
typedef struct stripe_entry {
    int         index;      /* index of the fragment in sequence */
    int         mcpys;      /* number of memcpy's while active */
    SGV2sfrag   *sfrag;     /* pointer to the fragment data */
} SBlock;

/* sequence lookup data: ffi->sdata, created by calloc */
typedef struct sgv2_private_sdata {
    /* about the data */
    VDIFUSEntry *vs;        /* pointer to seq entry */
    uint32_t    fcmx;       /* maximum frame count seen */
    int         msbs;       /* minimum short block size */
    int         page;       /* kernel page size */
    off_t       scnt;       /* total move count for stripe */
    off_t       diag[SGV2_DIAG_COUNT];
    /* about the read */
    off_t       roff;       /* requested read offset */
    size_t      size;       /* last requested read size */
    char        *sowb;      /* start of write buffer */
    char        *eowb;      /* end of write buffer */
    int         *vffp;      /* pointer to error flags */
    /* about the stripe */
    int         zero;       /* origin block of stripe */
    int         sgap;       /* first block after a gap */
    int         numb;       /* number of blocks in stripe */
    off_t       bybs;       /* total bytes preceding the stripe */
    off_t       byis;       /* total bytes in this stripe */
    off_t       byas;       /* total bytes following the stripe */
    off_t       bygt;       /* grand total bytes in the sequence */
    /* for tracking via dplane blocknum(ber)s */
    int         prev_bnum;  /* last blocknum before the stripe */
    int         next_bnum;  /* next blocknum after the stripe */
    int         min_bnum;   /* smallest blocknum mentioned */
    int         max_bnum;   /* largest blocknum mentioned */
    SBlock      stripe[VDIFUSE_MAX_SEQI];
    FFInfo      *fip;       /* pointer to file orr area */
} SGV2sdata;

/* functions found in vdifsg2.c; could be elsewhere */
extern double secs_since(struct timeval *when);
extern uint64_t sg_signature(uint32_t *vh);
extern int sg_info_size(void);

/* functions found in vdifsg2.c specific to vdifuse */
extern void attach_sgv2_ancillary(uint32_t vx_index);
extern int describe_ancillary_sgv2(VDIFUSEntry *vc);
extern int finalize_sgv2_sequence(VDIFUSEntry *vp);

/* vdifuse support for open/read/release for sgv2 */
extern int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_sgv2_seq(FFInfo *ffi);
extern int read_sgv2_seq(char *buf, FFInfo *ffi);
extern void update_sgv2_seq(FFInfo *ffi);

#endif /* vdifsg2_h */

/*
 * eof
 */
