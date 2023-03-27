/*
 * (c) Massachusetts Institute of Technology, 2010..2023
 * (c) Geoffrey B. Crew, 2010..2023
 *
 * $Id: vdif.h 5677 2023-03-04 19:06:02Z gbc $
 *
 * Hack code to generate a simple packet stream for testing
 *
 * The VDIF epoch is a count of semesters from 2000 and as originally
 * defined rolls-over at 2032.  The UA bits are originally declared to
 * be 0-filled.  Not much breaks if ref_epoch is expanded to 7 or 8 bits.
 */

#ifndef vdif_h
#define vdif_h

#include <stdint.h>

#ifndef ROLLOVER
#define ROLLOVER    0
#endif /* ROLLOVER using UA */

/* bitfields are not portable, but our applicability is limited */
typedef struct vdif_hdr {
    struct word1 {
        uint32_t secs_inre:30;
        uint32_t legacy:1;
        uint32_t invalid:1;
    } w1;
    struct word2 {
        uint32_t df_num_insec:24;
        uint32_t ref_epoch:6+ROLLOVER;
#if ROLLOVER < 2
        uint32_t UA:2-ROLLOVER;  /* ROLLOVER < 2 */
#endif /* ROLLOVER < 2 uses UA */
    } w2;
    struct word3 {
        uint32_t df_len:24;
        uint32_t num_channels:5;
        uint32_t ver:3;
    } w3;
    struct word4 {
        uint32_t stationID:16;
        uint32_t threadID:10;
        uint32_t bps:5;
        uint32_t dt:1;
    } w4;
    struct word5 {
        uint32_t eud5:24;
        uint32_t edv5:8;
    } w5;
    struct word6 {
        uint32_t PICstatus;
    } w6;
    uint64_t edh_psn;
} __attribute__((__may_alias__)) VDIFHeader;

/* for sync'ing on streams, define a signature */
typedef union vdif_signature_union {
    struct vdif_signature_bits {
        uint64_t df_len:24;
        uint64_t ref_epoch:6+ROLLOVER;
#if ROLLOVER < 2
        uint64_t UA:2-ROLLOVER;  /* ROLLOVER < 2 */
#endif /* ROLLOVER < 2 uses UA */
        uint64_t stationID:16;
        uint64_t num_channels:5;
        uint64_t ver:3;
        uint64_t bps:5;
        uint64_t dt:1;
        uint64_t legacy:1;
        uint64_t unused:1;
    } bits;
    uint64_t word;
} VDIFsigu;

#endif /* vdif_h */

/*
 * eof
 */
