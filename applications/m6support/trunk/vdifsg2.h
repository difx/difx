/*
 * $Id: vdifsg2.h 5271 2021-08-05 19:46:35Z gbc $
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
 * The read byte offset (ffi->offset) should then lie within the
 * "zero" element of the stripe and proceed through the packets of
 * that block up to "sgap".  The major support functions include:
 *
 * member_show(sfp, ith)     diagnostic on ith member (fragment)
 * member_show(sfp, ith)     tracing variant (one-liner)
 * member_move(sfp,blk,dir)  move this member to a new block
 * member_init(sfp)          initialize one member (fragment)
 * 
 * stripe_trace(sdp, l, a)   tracing variant (multiline version) of
 * stripe_show(sdp, l, a)    diagnostic on the stripe
 * stripe_comp(a, b)         comparison function for sorting
 * stripe_sort(sdp)          sort the members of the sequence
 * stripe_schk(sdp)          check the sort of the sequence
 * stripe_zero(sdp)          find the new stripe "zero"
 * stripe_dgap(sdp)          adjust the stripe to remove gaps
 * stripe_boff(sdp)          compute the byte offset in sieqence
 * stripe_init(sdp, f)       initializes the stripe at start
 * stripe_vern(sdp, b)       vernier bw walk into the bytes needed
 *
 * stripe_spot(sdp, c)       place a stripe at a set of positions
 * stripe_find(sdp, dir)     used when "offset" is outside stripe
 * stripe_jump(sdp)          used to trigger large stripe movement
 *
 * stripe_walk(sdp, dir)     used when stripe shifts incrementally
 * stripe_chck(sdp)          used to decide strategy on stripe access
 * stripe_anal(sdp)          double-check preceding check
 *
 * stripe_read(sdp, buf, l)  loads bytes into buffer from existing stripe
 * stripe_rdfw(sdp, buf)     read forwards from existing stripe
 * stripe_rdbw(sdp, buf)     read backwards from existing stripe
 *
 * "zero" is managed in the walking/finding
 * "sgap" is managed in the degapping
 * A move will never move past the last block or the first one.
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
 * either end of the block.
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

/* for the stripe diagnostic counters */
#define SGV2_DIAG_BREAD     0
#define SGV2_DIAG_TRACE     1
#define SGV2_DIAG_SHOWN     2
#define SGV2_DIAG_VERN      3
#define SGV2_DIAG_WALK      4
#define SGV2_DIAG_FIND      5
#define SGV2_DIAG_CHECK     6
#define SGV2_DIAG_READ      7
#define SGV2_DIAG_RDFW      8
#define SGV2_DIAG_RDBW      9
#define SGV2_DIAG_DOIT      10
#define SGV2_DIAG_SPARE     11
#define SGV2_DIAG_COUNT     (SGV2_DIAG_SPARE + 1)

/* for 16-disk 10MB blocks, this is about 16 walks */
#define STRIPE_JUMP_ENABLED 1
#define STRIPE_JUMP_TRIGGER 262144000

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
    double      ptbe;       /* first packet time of block */
    double      pten;       /* final packet time of block */
    off_t       cblk;       /* currrent block number in this frag */
    off_t       bybb;       /* total (packet) bytes preceding this block */
    off_t       byib;       /* total (packet) bytes in this block */
    off_t       byab;       /* total (packet) bytes following this block */
} SGV2sfrag;

/* for managing the active block from each member */
typedef struct stripe_entry {
    int         index;      /* index of the fragment in sequence */
    int         reads;      /* number of memcpy's while active */
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
    /* about the stripe */
    int         zero;       /* origin block of stripe */
    int         sgap;       /* first block after a gap */
    int         numb;       /* number of blocks in stripe */
    off_t       bybs;       /* total bytes preceding the stripe */
    off_t       byis;       /* total bytes in this stripe */
    off_t       byas;       /* total bytes following the stripe */
    off_t       bygt;       /* grand total bytes in the sequence */
    SBlock      stripe[VDIFUSE_MAX_SEQI];
} SGV2sdata;

/* functions found in vdifsg2.c */
extern double secs_since(struct timeval *when);
extern uint64_t sg_signature(uint32_t *vh);
extern int sg_info_size(void);
extern void attach_sgv2_anc(uint32_t vx_index);
extern int describe_ancillary_sgv2(VDIFUSEntry *vc);
extern int finalize_sgv2_sequence(VDIFUSEntry *vp);

/* vdifuse support for open/read/release for sgv2 */
extern int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi);
extern void release_sgv2_seq(FFInfo *ffi);
extern int read_sgv2_seq(char *buf, FFInfo *ffi);

#endif /* vdifsg2_h */

/*
 * eof
 */
