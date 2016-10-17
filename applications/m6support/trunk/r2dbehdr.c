/*
 * $Id: r2dbehdr.c 4124 2016-09-08 15:48:37Z gbc $
 *
 * Support for R2DBE extended headers
 *   v0 was deployed for Mar2015 campaign
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "exthdr.h"

/*
 * R2DBEv0 case -- id is pol, status word is a signed counter
 */
void r2dbev0_hdr_chk(const int id, const uint32_t status, const uint32_t frame)
{
    ext_hdr_work.pol = id;
    double ave, dev, datum;
    int32_t offset = status;

    datum = (double)offset * 3.90625;   /* ns */
    ext_hdr_work.last_datum = datum;
    ext_hdr_work.last_frame = frame;
    ext_hdr_work.last_valid = 1;

    ext_hdr_work.r2dbe_gps_pps[0] += 1.0;
    ext_hdr_work.r2dbe_gps_pps[1] += datum;

    ave = ext_hdr_work.r2dbe_gps_pps[1]
        / ext_hdr_work.r2dbe_gps_pps[0];
    dev = fabs(datum - ave);
    if (dev > ext_hdr_work.r2dbe_gps_pps[2])
        ext_hdr_work.r2dbe_gps_pps[2] = dev;
    if (ext_hdr_work.verb>2) fprintf(ext_hdr_work.fp,
        EXT_HDR_STAMP " PPS - GPS %+.2f ns <%+.2lf> +/- %.2lf\n",
        ext_hdr_work.secsre, frame, datum, ave, dev);
}

/*
 * Provide help
 */
void r2dbev0_hdr_help(void)
{
    fprintf(stdout,
        "\nR2DBE: There are no options currently for R2DBE parsing.\n"
    );
}

/*
 * Provide a summary
 */
#define divchk(X) ((X==0)?1:(X))
void r2dbev0_hdr_sum(FILE *fp)
{
    fprintf(fp,
        "%05d:R2DBEv0 %u pkts Pol-%s\n",
        ext_hdr_work.filecnt, ext_hdr_work.pktcnt,
        ext_hdr_work.pol ? "X" : "Y");
    fprintf(fp,
        "%05d:R2DBEv0 PPS - GPS %+.2lf ns (%.2lf)\n",
        ext_hdr_work.filecnt,
        (double)ext_hdr_work.r2dbe_gps_pps[1] /
        divchk((double)ext_hdr_work.r2dbe_gps_pps[0]), 
        ext_hdr_work.r2dbe_gps_pps[2]);
}

/*
 * eof
 */
