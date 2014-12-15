/*
 * $Id: almahdr.c 2562 2014-10-09 16:39:23Z gbc $
 *
 * Support for ALMA extended headers
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "exthdr.h"

/*
 * Labels for all the pics
 */
static char *picid[16] = {
    "2A-Q1X","2A-Q1Y","2A-Q2X","2A-Q2Y","2A-Q3X","2A-Q3Y","2A-Q4X","2A-Q4Y",
    "BL-Q1X","BL-Q1Y","BL-Q2X","BL-Q2Y","BL-Q3X","BL-Q3Y","BL-Q4X","BL-Q4Y"
};

/*
 * Verbal descriptions of the status flag bits
 */
static void alma_status(FILE *fp, uint32_t status)
{
    fprintf(fp,
        "%05d:ALMA %s %s %s %s %s %s %s-data personality-%02X\n",
        ext_hdr_work.filecnt,
        (status & 0x0001) ? "bad  " : "ok",
        (status & 0x0002) ? "TEerr" : "ok",
        (status & 0x0004) ? "lock " : "ok",
        (status & 0x0008) ? "temp " : "ok",
        (status & 0x0010) ? "delay" : "ok",
        (status & 0x0020) ? "dkill" : "ok",
        (status & 0x00C0) ?
            ((status & 0x0080) ? " PRN" : " INC") : "NORM",
        ext_hdr_work.pers);
}

/*
 * Compute the FPGA temperature
 */
static void alma_fpgatemp(const uint32_t frame, uint32_t datum)
{
    uint32_t bits = (datum&0xFFC0)>>6;
    double tdegc = (((double)bits) * 503.975/1024.0) - 273.15;
    if (ext_hdr_work.verb>2) fprintf(ext_hdr_work.fp,
        EXT_HDR_STAMP " FPGA Temp = %.2f C (0x%08X -> 0x%X)\n",
        ext_hdr_work.secsre, frame, tdegc, datum, bits);
    else if (ext_hdr_work.verb>1) fprintf(ext_hdr_work.fp,
        EXT_HDR_STAMP " FPGA Temp = %.2f C\n",
        ext_hdr_work.secsre, frame, tdegc);
    ext_hdr_work.fpga_degc = tdegc;
}

/*
 * Processing for offsets
 */
static void alma_offset(const uint32_t frame,
    char *lab, uint64_t *pps, uint32_t offset)
{
    double ave;
    uint64_t dev;
    pps[0] ++;          /* accumulate count for average */
    pps[1] += offset;   /* accumulate sum for average */
    ave = (double)pps[1] / (double)pps[0];
    dev = (uint64_t)fabs(offset - ave);
    if (dev > pps[2]) pps[2] = dev;
    if (ext_hdr_work.verb>1) fprintf(ext_hdr_work.fp,
        EXT_HDR_STAMP " %s %lu <%.2f> +/- %lu\n",
        ext_hdr_work.secsre, frame, lab, offset, ave, dev);
}

/*
 * ALMA case -- id is PIC id, status word rotates by frame number.
 */
void alma_hdr_chk(const int id, const uint32_t status, const uint32_t frame)
{
    static uint32_t last_status;
    uint32_t datum = status & 0x0FFFFFFF;
    ext_hdr_work.id = id;
    switch (frame & 0x7) {
    case 0x0:   /* status */
        if (0 == (ext_hdr_work.mask & 0x01)) {
            ext_hdr_work.pers = (status >> 24) & 0xFF;
            ext_hdr_work.flags |= (status & 0xFF);
            if (status != last_status) {
                last_status = status;
                alma_status(ext_hdr_work.fp, status);
            }
        }
        break;
    case 0x1:   /* GPS 1PPS offset from PIC 1PPS */
        if (0 == (ext_hdr_work.mask & 0x02))
            alma_offset(frame, "GPS", ext_hdr_work.gps_pic_pps, datum);
        break;
    case 0x2:   /* Maser 1PPS offset from PIC 1PPS */
        if (0 == (ext_hdr_work.mask & 0x04))
            alma_offset(frame, "Maser", ext_hdr_work.maser_pic_pps, datum);
        break;
    case 0x3:   /* TE offset from PIC 1PPS at secs 0,6,12,.... */
        if (0 == (ext_hdr_work.mask & 0x08))
            alma_offset(frame, "TE", ext_hdr_work.te_pic_pps, datum);
        break;
    case 0x4:   /* FPGA Temperature */
        if (0 == (ext_hdr_work.mask & 0x10))
            alma_fpgatemp(frame, datum);
        break;
    case 0x5:   /* not implemented, s.b. 0x00000005 */
        if (0 == (ext_hdr_work.mask & 0x20))
            fprintf(ext_hdr_work.fp,
                EXT_HDR_STAMP "0x5\n", ext_hdr_work.secsre, frame);
        break;
    case 0x6:   /* not implemented, s.b. 0x00000006 */
        if (0 == (ext_hdr_work.mask & 0x40))
            fprintf(ext_hdr_work.fp,
                EXT_HDR_STAMP "0x6\n", ext_hdr_work.secsre, frame);
        break;
    case 0x7:   /* not implemented, s.b. 0x00000007 */
        if (0 == (ext_hdr_work.mask & 0x80))
            fprintf(ext_hdr_work.fp,
                EXT_HDR_STAMP "0x7\n", ext_hdr_work.secsre, frame);
        break;
    }
}

/*
 * Provide help
 */
void alma_hdr_help(void)
{
    fprintf(stdout,
        "The mask may be set to ignore certain data:\n"
        "  0x01 -  status bits\n"
        "  0x02 -  Maser PPS - internal PPS\n"
        "  0x04 -  GPS PPS - internal PPS\n"
        "  0x08 -  TE PPS - internal PPS\n"
        "  0x10 -  FPGA Temperature\n"
        "  0x20 -  unused\n"
        "  0x40 -  unused\n"
        "  0x80 -  unused\n"
        "You'll normally want mask=E0 to see everything useful\n"
    );
}

/*
 * Provide a summary
 */
#define divchk(X) ((X==0)?1:(X))
void alma_hdr_sum(FILE *fp)
{
    fprintf(fp,
        "%05d:ALMA %u pkts %.2f C PIC-%s\n",
        ext_hdr_work.filecnt, ext_hdr_work.pktcnt,
        ext_hdr_work.fpga_degc, picid[ext_hdr_work.id]);
    fprintf(fp,
        "%05d:ALMA GPS %.2lf(%lu) Maser %.2lf(%lu) TE %.2lf(%lu)\n",
        ext_hdr_work.filecnt,
        (double)ext_hdr_work.gps_pic_pps[1] /
        divchk((double)ext_hdr_work.gps_pic_pps[0]), 
        ext_hdr_work.gps_pic_pps[2],
        (double)ext_hdr_work.maser_pic_pps[1] /
        divchk((double)ext_hdr_work.maser_pic_pps[0]), 
        ext_hdr_work.maser_pic_pps[2],
        (double)ext_hdr_work.te_pic_pps[1] /
        divchk((double)ext_hdr_work.te_pic_pps[0]),
        ext_hdr_work.te_pic_pps[2]);
    alma_status(fp, ext_hdr_work.flags);
}

/*
 * eof
 */
