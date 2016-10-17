/*
 * $Id: vdif.h 2184 2014-06-15 17:54:30Z gbc $
 *
 * Hack code to generate a simple packet stream for testing
 */

#ifndef vdif_h
#define vdif_h

#include <stdint.h>

/* bitfields are not portable, but our applicability is limited */
typedef struct vdif_hdr {
    struct word1 {
        uint32_t secs_inre:30;
        uint32_t legacy:1;
        uint32_t invalid:1;
    } w1;
    struct word2 {
        uint32_t df_num_insec:24;
        uint32_t ref_epoch:6;
        uint32_t UA:2;
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
        uint64_t ref_epoch:6;
        uint64_t UA:2;
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
