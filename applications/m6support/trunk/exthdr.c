/*
 * $Id: exthdr.c 4169 2016-11-18 21:55:19Z gbc $
 *
 * Support for extended headers
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "exthdr.h"
#include "sg_access.h"

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
            "   jump:0|1    vex time (1) or pkt offset (0) for clock jumps\n"
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
    } else if (!strncmp(opt, "jump:", 5)) {
        ext_hdr_work.jump = atoi(opt+5);
        if (ext_hdr_work.verb > 1) fprintf(stdout,
            "Ext hdr clock jumps now %s\n",
                ext_hdr_work.jump ? "vex time" : "pkt offset");
    } else {
	if (strlen(opt) > 0)
	    fprintf(stdout, "What is %s\n", opt);
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
        "%05d:jump %s\n"
        "%05d:mask 0x%X\n",
        ext_hdr_work.filecnt, ext_hdr_work.typeval, ext_hdr_work.typeset,
        ext_hdr_work.filecnt, filename,
        ext_hdr_work.filecnt, ext_hdr_work.jump ? "vex time" : "pkt offset",
        ext_hdr_work.filecnt, ext_hdr_work.mask);
    if (ext_hdr_work.verb > 1) fprintf(stdout,
        "Ext hdr jumps now %d\n", ext_hdr_work.jump);
}

/*
 * Called every packet to examine extended header
 */
void extended_hdr_chk(const uint32_t *pkt)
{
    if (ext_hdr_work.verb == 0) return;     /* no way to report results */
    if ((pkt[0] & 0x80000000)) return;      /* invalid packet */
    if ((pkt[4] & 0xFF000000) != 0x02000000) return;
    ext_hdr_work.pktcnt ++;
    ext_hdr_work.secsre = pkt[0] & 0x3FFFFFFF;
    ext_hdr_work.last_valid = 0;
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
    ext_hdr_work.jump = temp.jump;
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
 * Items below support the extended header search.
 */

/*
 * (Re)Allocate stack and search history space.
 */
static void ehs_more(SrchState *wss, int want_stack)
{
    ext_hdr_work.last_valid = 0;
    if (want_stack) {
        wss->srch_salloc += SRCH_ALLOC;
        wss->srch_stack =
            realloc(wss->srch_stack, wss->srch_salloc * sizeof(SrchStack));
        if (ext_hdr_work.verb>1)
            fprintf(stdout, "Stack alloc now %u\n", wss->srch_salloc);
    } else {    /* want history */
        wss->srch_halloc += SRCH_ALLOC;
        wss->srch_history =
            realloc(wss->srch_history, wss->srch_halloc * sizeof(SrchData));
        if (ext_hdr_work.verb>1)
            fprintf(stdout, "History alloc now %u\n", wss->srch_halloc);
    }
}

/*
 * Initialize for the search: allocate storage.
 */
static int ehs_begin(SrchState *wss)
{
    wss->where = EXT_SS_BEGIN;
    wss->srch_salloc = SRCH_ALLOC;
    wss->srch_halloc = SRCH_ALLOC;
    /* initial allocations */
    ehs_more(wss, 0);
    ehs_more(wss, 1);
    /* start at the beginning */
    wss->srch_hwrite = wss->srch_swrite = wss->srch_s_read = 0;
    wss->where = EXT_SS_FIRST;
    return(0);
}

/*
 * Save the data and push the history pointer
 * For debugging, don't cache repeat visits, but keep a count.
 */
static void update_ehs_his(SrchState *wss)
{
    ext_hdr_work.last_valid = 0;    /* consumed */

    if (wss->srch_hwrite > 0 &&
        wss->srch_history[wss->srch_hwrite - 1].offset == wss->srch_next) {
        wss->srch_history[wss->srch_hwrite].count ++;
    } else {
        wss->srch_history[wss->srch_hwrite].offset = wss->srch_next;
        wss->last_datum =
        wss->srch_history[wss->srch_hwrite].zdatum = ext_hdr_work.last_datum;
        wss->srch_history[wss->srch_hwrite].count = 1;
        if (++ wss->srch_hwrite >= wss->srch_halloc) ehs_more(wss, 0);
    }
}

/*
 * Save the data and push the stack pointer
 */
static void append_ehs_stk(SrchState *wss, uint32_t pkt0, uint32_t pkt1,
    uint32_t lesser, double ldatum,
    uint32_t bigger, double bdatum)
{
    if (ext_hdr_work.verb>2) fprintf(stdout, " Appending %d\n",
        wss->srch_swrite);
    wss->srch_stack[wss->srch_swrite].lesser = lesser;
    wss->srch_stack[wss->srch_swrite].ldatum = ldatum;
    wss->srch_stack[wss->srch_swrite].bigger = bigger;
    wss->srch_stack[wss->srch_swrite].bdatum = bdatum;
    wss->srch_stack[wss->srch_swrite].pkt[0] = pkt0;
    wss->srch_stack[wss->srch_swrite].pkt[1] = pkt1;
    if (++ wss->srch_swrite >= wss->srch_salloc) ehs_more(wss, 1);
}

/*
 * Generate a new search point from the stack read point
 */
static uint32_t next_from_ehs_stack(SrchState *wss)
{
    uint32_t spot;
    if (wss->srch_s_read < wss->srch_swrite)
        spot = (wss->srch_stack[wss->srch_s_read].bigger +
                wss->srch_stack[wss->srch_s_read].lesser) / 2;
    else
        spot = wss->srch_midway;
    return(spot);
}

/*
 * Reconcile the last datum and the working stack pair.
 *  (stack) lesser & ldatum (at srch_s_read)
 *  wss->srch_next & wss->last_datum
 *   (stack)bigger & bdatum (at srch_s_read)
 * Among these, we either 1, 2 or 3 values at maxgap resolution.
 * If 1, we're done; continue with next on stack
 * If 2, update stack and continue
 * If 3, push onto stack and continue with first
 *
 * We flag we are done by placing wss->srch_midway onto the
 * stack as the endpoints so that next_from_ehs_stack() will
 * return this as an exit condition.  Otherwise return
 * next_from_ehs_stack(wss).
 */
#define EHS_INT(A)  ((int)rint(fabs((A)) / (wss->maxgap)))
static uint32_t resolve_ehs_stk(SrchState *wss)
{
    int ld = EHS_INT(wss->srch_stack[wss->srch_s_read].ldatum);
    int md = EHS_INT(wss->last_datum);
    int bd = EHS_INT(wss->srch_stack[wss->srch_s_read].bdatum);

    if (ext_hdr_work.verb>2) fprintf(stdout,
        " Resolve %d: (%d %d %d)\t", wss->srch_s_read, ld, md, bd);

    if (ld == md && md == bd) {         /* 1 value: all the same */
        if (ext_hdr_work.verb>2) fprintf(stdout, "1 Value\n");
        ++ wss->srch_s_read;
        if (wss->srch_s_read == wss->srch_swrite)
            return(wss->srch_midway);   /* no work left */
    } else if (ld == md) {              /* 2 values: md != bd */
        if (wss->srch_stack[wss->srch_s_read].lesser != wss->srch_next) {
            if (ext_hdr_work.verb>2) fprintf(stdout, "2 Lower same, narrow\n");
            wss->srch_stack[wss->srch_s_read].lesser = wss->srch_next;
            wss->srch_stack[wss->srch_s_read].ldatum = wss->last_datum;
            wss->srch_stack[wss->srch_s_read].pkt[0] = wss->last_pkt[0];
            wss->srch_stack[wss->srch_s_read].pkt[1] = wss->last_pkt[1];
        } else {                        /* narrow as it gets */
            if (ext_hdr_work.verb>2) fprintf(stdout, "2 Lower same, done\n");
            wss->srch_s_read ++;
        }
    } else if (md == bd) {              /* 2 values: ld != md */
        if (wss->srch_stack[wss->srch_s_read].bigger != wss->srch_next) {
            if (ext_hdr_work.verb>2) fprintf(stdout, "2 Upper same, narrow\n");
            wss->srch_stack[wss->srch_s_read].bigger = wss->srch_next;
            wss->srch_stack[wss->srch_s_read].bdatum = wss->last_datum;
            wss->srch_stack[wss->srch_s_read].pkt[0] = wss->last_pkt[0];
            wss->srch_stack[wss->srch_s_read].pkt[1] = wss->last_pkt[1];
        } else {                        /* narrow as it gets */
            if (ext_hdr_work.verb>2) fprintf(stdout, "2 Upper same, done\n");
            wss->srch_s_read ++;
        }
    } else {                            /* 3 values ld != md != bd */
        if (ext_hdr_work.verb>2) fprintf(stdout, "Else\n");
        /* look into bigger later */
        append_ehs_stk(wss,
            wss->last_pkt[0], wss->last_pkt[1],
            wss->srch_next, wss->last_datum,
            wss->srch_stack[wss->srch_s_read].bigger,
            wss->srch_stack[wss->srch_s_read].bdatum);
        wss->srch_stack[wss->srch_s_read].bigger = wss->srch_next;
        wss->srch_stack[wss->srch_s_read].bdatum = wss->last_datum;
       /* continue with lesser now */ 
    }
    return(next_from_ehs_stack(wss));
}

/*
 * Load the "starter" offset and gps pps offset values.
 * Note that pkt was selected independently of the value of srch_first;
 * but that srch_first was initialized the same way.
 * Here as elsewhere, not all packets will contain a valid reading,
 * so we need to be prepared to step to find one that is.
 */
static int ehs_first(SrchState *wss)
{
    if (!ext_hdr_work.last_valid) {
        wss->srch_next = ++ wss->srch_first;
        wss->where = EXT_SS_FIRST;
        return(0);
    }
    update_ehs_his(wss);
    wss->srch_next = wss->srch_final;
    wss->where = EXT_SS_FINAL;
    return(0);
}

/*
 * Load the "final" offset and gps pps offset values.
 */
static int ehs_final(SrchState *wss)
{
    if (!ext_hdr_work.last_valid) {
        wss->srch_next = -- wss->srch_final;
        return(0);
    }
    update_ehs_his(wss);
    append_ehs_stk(wss, wss->last_pkt[0], wss->last_pkt[1],
        wss->srch_first, wss->srch_history[0].zdatum,
        wss->srch_final, wss->srch_history[1].zdatum);
    wss->srch_midway = wss->srch_next = next_from_ehs_stack(wss);
    wss->where = EXT_SS_SEARCH;
    return(0);
}

/*
 * Load the "next" offset and gps pps offset values.
 * Using first and final as bounds against stupidity,
 * we step invalid reads to those extremes.  Once we
 * have a valid read, we can update the stack.
 */
static int ehs_search(SrchState *wss)
{
    if (!ext_hdr_work.last_valid) {
        if ((wss->srch_next > wss->srch_first) &&
            (wss->srch_next < wss->srch_midway))
                wss->srch_next --;
        else if ((wss->srch_next < wss->srch_final) &&
                 (wss->srch_next > wss->srch_midway))
                    wss->srch_next ++;
        else
            return((wss->where = EXT_SS_FINISH) ? 1 : 0);
        return(0);
    }
    update_ehs_his(wss);
    wss->srch_next = resolve_ehs_stk(wss);
    if (wss->srch_next == wss->srch_midway)
        return((wss->where = EXT_SS_FINISH) ? 1 : 0);
    return(0);
}

/*
 * Comparison function on SrchData for qsort()
 * Here the sort is on offset which can be done
 * for the stack (listing of segments) and history (all data).
 */
static int srch_cmph(const void *pa, const void *pb)
{
    SrchData *sa = (SrchData *)pa;
    SrchData *sb = (SrchData *)pb;
    if (sa->offset < sb->offset) return(-1);
    if (sa->offset > sb->offset) return( 1);
    return(0);
}
static int srch_cmps(const void *pa, const void *pb)
{
    SrchStack *sa = (SrchStack *)pa;
    SrchStack *sb = (SrchStack *)pb;
    if (sa->lesser < sb->lesser) return(-1);
    if (sa->lesser > sb->lesser) return( 1);
    if (sa->bigger < sb->bigger) return(-1);
    if (sa->bigger > sb->bigger) return( 1);
    return(0);
}

/*
 * Sort the search history for reporting.
 */
static int ehs_finish(SrchState *wss)
{
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Sorting stack with %u\n"
        "Sorting history with %u\n",
        wss->srch_swrite, wss->srch_hwrite);
    qsort(wss->srch_stack, wss->srch_swrite, sizeof(SrchStack), srch_cmps);
    qsort(wss->srch_history, wss->srch_hwrite, sizeof(SrchData), srch_cmph);
    wss->where = EXT_SS_SORTED;
    return(1);
}

/*
 * Reset to the unused state.
 */
static void ehs_cleanup(SrchState *wss)
{
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Freeing %p with %u\n", wss->srch_stack, wss->srch_salloc);
    free(wss->srch_stack);
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Freeing %p with %u\n", wss->srch_history, wss->srch_halloc);
    free(wss->srch_history);
    memset(wss, 0, sizeof(SrchState));
}

/*
 * This is the driver called from within check_search(),
 * and pkt points to the packet at pkts_seqoffset from
 * the start of the file.  Each invocation should set
 * pkts_seqoffset for the next pkt to read.
 *
 * It returns nonzero if we are done.
 */
int extended_hdr_search(uint32_t *pkt, SrchState *wss)
{
    int bail;
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Search entry: %d @ %u %08X %08X\n",
            wss->where, wss->srch_next, pkt[0], pkt[1]);
    wss->last_pkt[0] = pkt[0];
    wss->last_pkt[1] = pkt[1];
    extended_hdr_chk(pkt);  /* wss->last_datum computed */
    switch (wss->where) {
    case EXT_SS_BEGIN:      bail = ehs_begin(wss);    /* fall through */
    case EXT_SS_FIRST:      bail = ehs_first(wss);    break;
    case EXT_SS_FINAL:      bail = ehs_final(wss);    break;
    case EXT_SS_SEARCH:     bail = ehs_search(wss);   break;
    case EXT_SS_FINISH:     bail = ehs_finish(wss);   break;
    case EXT_SS_SORTED:     bail = 1;                 break;
    default:
        fprintf(stderr, "Compiler error\n");
        exit(1);
    }
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Search exit:  %d @ %u\n", wss->where, wss->srch_next);
    return(bail);
}

/*
 * Work out the vextime for the packet at this offset
 */
static char *srch_vextime(uint32_t *pkt)
{
    static char vt[80] = "i-really-dunno";
    char *vx = sg_vextime((pkt[1] & 0x3F000000) >> 24, pkt[0] & 0x3FFFFFFF);
    strncpy(vt, vx, sizeof(vt));
    return(vt);
}

/*
 * Hopefully it is all in the history at this point
 */
void extended_report(char *lab, SrchState *wss)
{
    int ii, bail;
    if (ext_hdr_work.verb>2) fprintf(stdout,
        "Report state %u\n", wss->where);
    if (wss->where < EXT_SS_SORTED) bail = ehs_finish(wss);
    strncpy(wss->lab, lab, 5);
    if (ext_hdr_work.verb>1) {
        fprintf(stdout, "%s:gap have %u samples\n",
            wss->lab, wss->srch_hwrite);
        for (ii = 0; ii < wss->srch_hwrite; ii++)
            fprintf(stdout, "%s:gap at offset %10u gps delta %.3lf ns (%d)\n",
                wss->lab,
                wss->srch_history[ii].offset,
                wss->srch_history[ii].zdatum,
                wss->srch_history[ii].count);
    }
    fprintf(stdout, "%s:gap have %u gps jumps\n",
        wss->lab, wss->srch_swrite);
    for (ii = 0; ii < wss->srch_swrite; ii++) {
        if (ext_hdr_work.jump) fprintf(stdout,
            "%s:jump after %s, %u s.r.e by %.3lf ns\n",
            wss->lab,
            srch_vextime(wss->srch_stack[ii].pkt),
            wss->srch_stack[ii].pkt[0] & 0x3FFFFFFF,
            (wss->srch_stack[ii].bdatum - wss->srch_stack[ii].ldatum));
        else fprintf(stdout,
            "%s:gap jump at %10u -> %-10u ( %.3lf -> %.3lf ~ %.3lf ns)\n",
            wss->lab,
            wss->srch_stack[ii].lesser,
            wss->srch_stack[ii].bigger,
            wss->srch_stack[ii].ldatum,
            wss->srch_stack[ii].bdatum,
            (wss->srch_stack[ii].bdatum - wss->srch_stack[ii].ldatum));
    }
    ehs_cleanup(wss);
}

/*
 * eof
 */
