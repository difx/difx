/*
 * $Id: exthdr.c 3008 2015-04-14 19:01:58Z gbc $
 *
 * Support for extended headers
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "exthdr.h"

/* quasi-common work area */
struct ext_hdr_work ext_hdr_work;

/*
 * Prepare for extended headers using option string
 * Return nonzero on error.
 */
int extended_hdr_opt(const char *opt)
{
    char *next;
    if (!strncmp(opt, "help", 4)) {
        fprintf(stdout,
            "Extended header processing is controlled by one or more\n"
            "exthdr=key:val[,key:val...] directives:\n"
            "   verb:N      set verbosity to N\n"
            "   type:hex    assert hdr type (ALMA: A5AE50, R2DBEv0: 000000)\n"
            "   file:name   write results to named file\n"
            "   mask:bits   hex mask of disinterest (bits by frame type)\n"
            "you can use either use one exthdr= argument with ,-sep options,\n"
            "or you can use multiple exthdr= arguments as you prefer.\n"
        );
        alma_hdr_help();
        r2dbev0_hdr_help();
    } else if (!strncmp(opt, "verb:", 5)) {
        extended_hdr_verb(atoi(opt+5), (char *)0);
    } else if (!strncmp(opt, "type:", 5)) {
        ext_hdr_work.typeval = strtol(opt+5, 0, 16);
        ext_hdr_work.typeset = 1;
        if (ext_hdr_work.verb > 1) fprintf(stdout,
            "Ext hdr type now 0x%X\n", ext_hdr_work.typeval);
    } else if (!strncmp(opt, "file:", 5)) {
        if (ext_hdr_work.fp != stdout) fclose(ext_hdr_work.fp);
        ext_hdr_work.fp = fopen(opt+5, "w");
        if (!ext_hdr_work.fp) {
            perror("extended_hdr_opt:fopen");
            ext_hdr_work.fp = stdout;
        } else {
            if (ext_hdr_work.verb > 1) fprintf(stdout,
                "Ext hdr output to %s\n", opt+5);
        }
    } else if (!strncmp(opt, "mask:", 5)) {
        ext_hdr_work.mask = strtol(opt+5, 0, 16);
        if (ext_hdr_work.verb > 1) fprintf(stdout,
            "Ext hdr mask now 0x%X\n", ext_hdr_work.mask);
    }
    next = strchr(opt, ',');
    if (next) return(extended_hdr_opt(next + 1));
    return(0);
}

/*
 * Common verbosity.  This is called before any other ext hdr processing.
 */
void extended_hdr_verb(const int verbose, const char *filename)
{
    if (!ext_hdr_work.fp) ext_hdr_work.fp = stdout;
    if (verbose > ext_hdr_work.verb) ext_hdr_work.verb = verbose;
    if (ext_hdr_work.verb > 1) fprintf(stdout,
        "Ext hdr verb now %d, working on %s\n", verbose, filename);
    if (filename && (ext_hdr_work.fp != stdout)) fprintf(ext_hdr_work.fp,
        "%05d:type 0x%6X (%d)\n"
        "%05d:file %s\n"
        "%05d:mask 0x%X\n",
        ext_hdr_work.filecnt, ext_hdr_work.typeval, ext_hdr_work.typeset,
        ext_hdr_work.filecnt, filename,
        ext_hdr_work.filecnt, ext_hdr_work.mask);
}

/*
 * Called every packet to examine extended header
 */
void extended_hdr_chk(const uint32_t *pkt)
{
    if (ext_hdr_work.verb == 0) return;     /* no way to report results */
    if ((pkt[4] & 0xFF000000) != 0x02000000) return;
    ext_hdr_work.pktcnt ++;
    ext_hdr_work.secsre = pkt[0] & 0x3FFFFFFF;
    if (ext_hdr_work.typeset == 1) {        /* asserted by human */
        ext_hdr_work.type = ext_hdr_work.typeval & 0x00FFFFF0;
    } else if (ext_hdr_work.typeset == 0) { /* set first time */
        ext_hdr_work.type = (pkt[4] & 0x00FFFFF0);
        ext_hdr_work.typeset = 2;
    }
    switch (ext_hdr_work.type) {
    case ALMA_EXT_HDR:
        alma_hdr_chk(pkt[4]&0xF, pkt[5], pkt[1]&0x00FFFFFF);
        break;
    case R2DBEv0_EXT_HDR:
        r2dbev0_hdr_chk(pkt[4]&0xF, pkt[5], pkt[1]&0x00FFFFFF);
        break;
    default:        /* unknown pkt */
        break;
    }
}

/*
 * Called when done to print summary information
 * After that, it preps for the next file.
 */
void extended_hdr_sum(const uint32_t opt)
{
    struct ext_hdr_work temp;
    if (ext_hdr_work.verb == 0) return;     /* no way to report results */
    switch (ext_hdr_work.type) {
    case ALMA_EXT_HDR:
        alma_hdr_sum(stdout);
        if (ext_hdr_work.fp != stdout) alma_hdr_sum(ext_hdr_work.fp);
        break;
    case R2DBEv0_EXT_HDR:
        r2dbev0_hdr_sum(stdout);
        if (ext_hdr_work.fp != stdout) r2dbev0_hdr_sum(ext_hdr_work.fp);
        break;
    default:        /* unknown pkt */
        fprintf(stdout, "%05d:Status (unknown) %d packets\n",
            ext_hdr_work.filecnt, ext_hdr_work.pktcnt);
        break;
    }
    /* prep for next file -- remember a few things, nuke the rest */
    temp = ext_hdr_work;
    memset(&ext_hdr_work, 0, sizeof(ext_hdr_work));
    ext_hdr_work.verb = temp.verb;
    if (temp.typeset == 1) {    /* asserted by user */
        ext_hdr_work.typeset = temp.typeset;
        ext_hdr_work.typeval = temp.typeval;
    }                           /* else it is zero */
    ext_hdr_work.filecnt = temp.filecnt + 1;
    ext_hdr_work.fp = temp.fp;
    ext_hdr_work.mask = temp.mask;
    fflush(ext_hdr_work.fp);
}

/*
 * eof
 */
