/*
 * $Id: per_file.c 4508 2017-12-04 20:02:49Z gbc $
 *
 * Scan checker for Mark6.  Files are presumed to be:
 *   SGv2 files of VDIF packets
 *   Flat files of VDIF packets
 * The packets may have a prefix of some number of 32-bit words
 * (i.e. a VTP PSN which is tracked, but otherwise ignored).
 */

#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "sg_access.h"
#include "sc_stats.h"
#include "m6scmodule.h"

#ifndef EXTEND_HCHK
# define EXTEND_HCHK 0
#endif /* EXTEND_HCHK */
#if EXTEND_HCHK
# include "exthdr.h"
#endif /* EXTEND_HCHK */

static int verb = 0;
static int ispy = 0;

static struct checker_work {
    /* details */
    SGInfo      sgi;                /* SG access structure */
    BSInfo      bsi;                /* Bit states structure */
    SCInfo      sci;                /* Scan Check structure */

    /* options */
    double      alarm_secs;         /* initial per-file allowance */
    uint32_t    trial_seed;         /* random number seed */
    uint32_t    pkts_loops;         /* number of packets to hit */
    uint32_t    pkts_runs;          /* number of packets in a row */
    uint32_t    pkts_seqstarter;    /* starting point for seq check */
    uint32_t    pkts_seqoffset;     /* seq check current offset */
    char        stat_chans[256];    /* csv list of channels */
    uint32_t    stat_octets;        /* max number of samples/packet */
    uint32_t    stat_delta;         /* dump stats after so many pkts */
    uint32_t    station_mask;       /* bits of station to check */
    uint32_t    extend_hchk;        /* check the extended header */
    uint32_t    flist_only;         /* only generate an flist entry */
    uint32_t    pps_search;         /* looking for pps jumps */
    uint32_t    showthreads;        /* whether to report on threads */

    /* evolving */
    char        *cur_file;          /* name of the current file */
    char        *pickername;        /* name of picker */
    uint32_t    cur_numb;           /* and its ordinal number */
    uint64_t    pkts_passed;        /* number of packets passing */
    uint64_t    pkts_failed;        /* number of packets failing */
    uint64_t    pkts_tested;        /* number of packets tested */
    uint64_t    pkts_timing;        /* packets w/timing issues */
    uint64_t    pkts_filled;        /* packets marked invalid */
    double      avail_process_secs; /* time remaining before alarm */
    double      file_start_secs;    /* time current file started */
    double      file_process_secs;  /* current file consumption */
    double      total_process_secs; /* total time consumed */
#if EXTEND_HCHK
    SrchState   srch_state;         /* complete search state */
#endif /* EXTEND_HCHK */

    /* delta stats */
    BSInfo      bdel, blst;         /* used by stat_delta */
    off_t       np0, np1;           /* packet offsets */
} work = {
    /* initialize the non-zero values */
    .alarm_secs = 60.0,             /* a minute per file is alot */
    .trial_seed = 17,               /* doesn't really matter */
    .pkts_loops = 5,                /* briefest of touches */
    .pkts_runs = 20,                /* a really small patch */
    .stat_octets = SG_MAX_VDIF_BYTES,
    .station_mask = SG_STATION_MASK
};

/*
 * Returns 1 if we have exceeded the limit
 */
static int time_to_bail(void)
{
    work.file_process_secs = current_or_elapsed_secs(&work.file_start_secs);
    if (work.file_process_secs > work.alarm_secs) return(1);
    return(0);
}

/*
 * Type for a generic packet finder
 */
typedef uint32_t *(Random_Picker)(int *nlp, uint32_t **endp);

/*
 * Developer error: epic fail.
 */
static uint32_t *search_packet_er(int *nlp, uint32_t **endp)
{
    return(NULL32P);
}
static uint32_t *random_packet_er(int *nlp, uint32_t **endp)
{
    return(NULL32P);
}
static uint32_t *sequence_packet_er(int *nlp, uint32_t **endp)
{
    return(NULL32P);
}

/*
 * Pick a packet at random in the file using one of the 3 sg methods
 */
static uint32_t *random_packet_sg(int *nlp, uint32_t **endp)
{
    int how = (int)floor( 3.0 * (double)random() / (double)RAND_MAX ) ;
    off_t ww;
    uint32_t *pkt;
    switch (how) {
    case 0:
        ww = rint( (double)(work.sgi.total_pkts - 1) *
                   (double)random() / (double)RAND_MAX );
        pkt = sg_pkt_by_num(&work.sgi, ww, nlp, endp);
        break;
    case 1:
        ww = rint( (double)(work.sgi.sg_total_blks - 1) *
                   (double)random() / (double)RAND_MAX );
        pkt = sg_pkt_by_blk(&work.sgi, ww, nlp, endp);
        break;
    case 2:
        ww = rint( (double)(work.sgi.smi.size - 1) *
                   (double)random() / (double)RAND_MAX );
        pkt = sg_pkt_by_off(&work.sgi, ww, nlp, endp);
        break;
    default:
        pkt = NULL32P;  /* this should not happen */
        break;
    }
    return(pkt);
}
/*
 * Pick the packets in order for sg files
 * Essentially the same code as above.
 */
static uint32_t *sequence_packet_sg(int *nlp, uint32_t **endp)
{
    uint32_t *pkt = sg_pkt_by_num(&work.sgi, work.pkts_seqoffset, nlp, endp);
    work.pkts_seqoffset ++;
    return(pkt);
}
/*
 * Pick a packet in support of a binary search.
 */
static uint32_t *search_packet_sg(int *nlp, uint32_t **endp)
{
#if EXTEND_HCHK
    work.pkts_seqoffset = work.srch_state.srch_next;
#endif /* EXTEND_HCHK */
    return(sequence_packet_sg(nlp, endp));
}

/*
 * Pick a packet at random by packet number.  To pick by
 * offset * is trivially different, so we shall not bother.
 */
static uint32_t *random_packet_fl(int *nlp, uint32_t **endp)
{
    void *pktp;
    off_t ww = rint( (double)(work.sgi.total_pkts - 1) *
                     (double)random() / (double)RAND_MAX );
    pktp = work.sgi.smi.start + ww * (off_t)work.sgi.read_size;
    *endp = work.sgi.smi.eomem;
    *nlp = (work.sgi.smi.eomem - pktp) / (off_t)work.sgi.read_size;
    pktp += work.sgi.pkt_offset;
    return((uint32_t *)pktp);
}
/*
 * Pick the packets in order for flat files.
 * Essentially the same code as above.
 */
static uint32_t *sequence_packet_fl(int *nlp, uint32_t **endp)
{
    void *pktp;
    pktp = work.sgi.smi.start + work.pkts_seqoffset * (off_t)work.sgi.read_size;
    *endp = work.sgi.smi.eomem;
    *nlp = (work.sgi.smi.eomem - pktp) / (off_t)work.sgi.read_size;
    pktp += work.sgi.pkt_offset;
    work.pkts_seqoffset ++;
    return((uint32_t *)pktp);
}
/*
 * Pick a packet in support of a binary search.
 */
static uint32_t *search_packet_fl(int *nlp, uint32_t **endp)
{
#if EXTEND_HCHK
    work.pkts_seqoffset = work.srch_state.srch_next;
#endif /* EXTEND_HCHK */
    return(sequence_packet_fl(nlp, endp));
}

/*
 * For reporting purposes
 */
static char *picker_name(Random_Picker *picker)
{
    if (picker == search_packet_sg) return("schsg"); else
    if (picker == search_packet_fl) return("schfl"); else
    if (picker == search_packet_er) return("scher"); else
    if (picker == random_packet_sg) return("rndsg"); else
    if (picker == random_packet_fl) return("rndfl"); else
    if (picker == random_packet_er) return("rnder"); else
    if (picker == sequence_packet_sg) return("seqsg"); else
    if (picker == sequence_packet_fl) return("seqfl"); else
    if (picker == sequence_packet_er) return("seqer"); else
    return("check");
}

/*
 * For the number of requested loops, check the
 * packet and its statistics, as requested.
 */
static void check_random(Random_Picker *rp)
{
    int nl, tt, ii, fails, fills;
    uint32_t *pkt, *end;
    for (tt = 0; tt < work.pkts_loops; tt++) {
        pkt = (*rp)(&nl, &end);
        if (!pkt) {
            work.pkts_failed ++;
            work.pkts_tested ++;
            continue;
        }
        if (nl > work.pkts_runs) nl = work.pkts_runs;
        work.pkts_tested += nl;
        fails = sg_pkt_check(&work.sgi, pkt, nl, end, &fills);
        work.pkts_filled += fills;
        work.pkts_failed += fails;
        work.pkts_passed += (nl - fails);
        if (fails == 0)
            work.pkts_timing += sg_pkt_times(&work.sgi, pkt, nl, end);
        if (fails == 0 && work.stat_octets > 0) {
            int dp = work.sgi.read_size/sizeof(uint32_t);
            for (ii = 0; ii < nl; ii++, pkt += dp)
                stats_check(&work.bsi, (uint64_t*)(pkt + 8));
        }
#if EXTEND_HCHK
        if (work.extend_hchk) extended_hdr_chk(pkt);
#endif /* EXTEND_HCHK */
        if (work.stat_delta)
            stats_delta(&work.bsi, &work.blst, &work.bdel,
                pkt, nl, work.cur_numb, work.sgi.smi.start);
        if (work.showthreads) (void)sg_get_vsig(pkt,
            (void*)0, work.sgi.verbose > 2 ? 1 : 0,
            "thr:", (VDIFsigu*)0, work.sgi.threads);
        if (time_to_bail()) break;
    }
}

/*
 * Examine work.pkts_runs packets in order
 */
static void check_sequence(Random_Picker *rp)
{
    int tt, nl, num_check, fails, fills;
    uint32_t *pkt, *end, *pkt0 = 0;
    num_check = (work.pkts_runs == 0) ? work.sgi.total_pkts : work.pkts_runs;
    if (work.pkts_seqoffset + num_check > work.sgi.total_pkts)
        num_check = work.sgi.total_pkts - work.pkts_seqoffset - 1;
    for (tt = 0; tt < num_check; tt++) {
        pkt = (*rp)(&nl, &end);
        if (pkt0 == 0) pkt0 = pkt;
        if (!pkt) {
            work.pkts_failed ++;
            work.pkts_tested ++;
            continue;
        }
        work.pkts_tested += 1;
        fails = sg_pkt_check(&work.sgi, pkt, 1, end, &fills);
        work.pkts_filled += fills;
        work.pkts_failed += fails ? 1 : 0;
        work.pkts_passed += fails ? 0 : 1;
        if (fails == 0)
            work.pkts_timing += sg_pkt_times(&work.sgi, pkt, 2, end);
        if (fails == 0 && work.stat_octets > 0)
            stats_check(&work.bsi, (uint64_t*)(pkt));
#if EXTEND_HCHK
        if (work.extend_hchk) extended_hdr_chk(pkt);
#endif /* EXTEND_HCHK */
        if (work.stat_delta &&
            ((tt % work.stat_delta) == (work.stat_delta - 1))) {
                stats_delta(&work.bsi, &work.blst, &work.bdel,
                pkt0, work.stat_delta, work.cur_numb, work.sgi.smi.start);
            pkt0 = pkt;
        }
        if (time_to_bail()) break;
    }
}

/*
 * Drive a (binary) search for gaps exceeding the limit specified.
 * We carry along the same checking baggage, and in addition, track
 * the GPS PPS value in the search history.
 */
static void check_search(Random_Picker *rp)
{
#if EXTEND_HCHK
    int tt, nl, num_check, fails, fills, bail = 0;
    uint32_t *pkt, *end, *pkt0 = 0;
    num_check = (work.pkts_runs == 0) ? work.sgi.total_pkts : work.pkts_runs;
    if (work.pkts_seqoffset + num_check > work.sgi.total_pkts)
        num_check = work.sgi.total_pkts - work.pkts_seqoffset - 1;
    work.srch_state.srch_first = work.pkts_seqoffset;
    work.srch_state.srch_next  = work.pkts_seqoffset;
    work.srch_state.srch_final = work.pkts_seqoffset + num_check - 1;
    work.srch_state.maxgap = work.pps_search;
    for (tt = 0; tt < num_check; tt++) {
        /* generates new packet based on work.pkts_seqoffset */
        pkt = (*rp)(&nl, &end);
        if (pkt0 == 0) pkt0 = pkt;
        if (!pkt) {
            work.pkts_failed ++;
            work.pkts_tested ++;
            continue;
        }
        work.pkts_tested += 1;
        fails = sg_pkt_check(&work.sgi, pkt, 1, end, &fills);
        work.pkts_filled += fills;
        work.pkts_failed += fails ? 1 : 0;
        work.pkts_passed += fails ? 0 : 1;
        if (fails == 0)
            work.pkts_timing += sg_pkt_times(&work.sgi, pkt, 2, end);
        if (fails == 0 && work.stat_octets > 0)
            stats_check(&work.bsi, (uint64_t*)(pkt));

        if (work.extend_hchk)
            bail = extended_hdr_search(pkt, &work.srch_state);

        if (work.stat_delta &&
            ((tt % work.stat_delta) == (work.stat_delta - 1))) {
                stats_delta(&work.bsi, &work.blst, &work.bdel,
                pkt0, work.stat_delta, work.cur_numb, work.sgi.smi.start);
            pkt0 = pkt;
        }
        if (time_to_bail() || bail) break;
    }
#else /* EXTEND_HCHK */
    fprintf(stderr, "Sorry, required extended header methods\n"
        "were not compiled into this executable scan_check.\n");
    exit(1);
#endif /* EXTEND_HCHK */
}

/*
 * Provide a one-liner for DiFX.
 */
static void flist_report(void)
{
    static char first[200], final[200];
    uint32_t ftime;
    ftime = work.sgi.first_secs;
    if (work.sgi.frame_cnt_max>0)
        ftime += rint(work.sgi.first_frame/work.sgi.frame_cnt_max);
    strncpy(first, sg_vextime(work.sgi.ref_epoch, ftime), sizeof(first));
    ftime = work.sgi.final_secs;
    if (work.sgi.frame_cnt_max>0)
        ftime += rint(work.sgi.final_frame/work.sgi.frame_cnt_max);
    strncpy(final, sg_vextime(work.sgi.ref_epoch, ftime), sizeof(final));
    fprintf(stdout, "%s    %s %s\n", work.cur_file, first, final);
    fflush(stdout);
}

/*
 * Provide a one-liner on threads
 */
static void thread_summary(char *lab, short *threads)
{
    int ii;
    fprintf(stdout, "%s:threads", lab);
    for (ii = 0; ii < MAX_VDIF_THREADS; ii++) {
        if (threads[ii] < 0 || threads[ii] > 1023) break;
        fprintf(stdout, " %d", threads[ii]);
    }
    fprintf(stdout, "\n");
}

/*
 * Provide a summary report
    else if (work.pps_search > 0) search_report();
 */
static void summary_report(void)
{
    static char lab[] = "0000               ";
    
    snprintf(lab, 6, "%05d", work.cur_numb);
    if (verb>0) fprintf(stdout,
        "%s:%s (%.1fms) %s\n"
        "%s:%s %lu pass + %lu fail | %lu time %lu fill / %lu tested\n",
        lab, work.cur_file, 1.0e3 * work.total_process_secs,
             sg_vextime(work.sgi.ref_epoch, work.sgi.first_secs),
        lab, work.pickername, work.pkts_passed, work.pkts_failed,
             work.pkts_timing, work.pkts_filled, work.pkts_tested);
    if (work.showthreads) thread_summary(lab, work.sgi.threads);
    strcat(lab, ":");
    if (verb>0) sg_report(&work.sgi, lab);
    strcat(lab, "stats:");
    if (verb>0) stats_report(&work.bsi, lab);
#if EXTEND_HCHK
    if (work.extend_hchk) extended_hdr_sum(work.extend_hchk);
    if (work.pps_search > 0) extended_report(lab, &work.srch_state);
#endif /* EXTEND_HCHK */
    fflush(stdout);
}

/*
 * Declare the start of a new group
 */
void m6sc_sr_start(const char *scanref, int nf, int mf, double tol)
{
    if (!ispy) return;
    memset(&work.sci, 0, sizeof(work.sci));
    /* has zeroed everything */
    strncpy(work.sci.scanref, scanref, sizeof(work.sci.scanref) - 2);
    strncpy(work.sci.status, m6sc_NULL_STATUS, sizeof(work.sci.status) - 2);
    work.sci.alpha_s = 2000000000;
    work.sci.alpha_fr = 0;
    work.sci.nf_exp = nf;
    work.sci.exp_fr = mf;
    work.sci.tol = (tol > 0.0) ? tol : 0.2;
}

/*
 * Accumulate statistics percentages, and notice as we do so.
 */
static void accum_stats(void)
{
    int ii, jj;
    double nf = (work.sci.nfiles > 0) ? (double)work.sci.nfiles : 1;
    static double targ[2][4] = {
        { 0.500, 0.500, 0.000, 0.000 },
        { 0.159, 0.341, 0.341, 0.159 },
    };
    for (ii = 0; ii < 4; ii++)
        work.sci.pc[ii] +=
            (double)work.bsi.bstates[ii] / (double)work.bsi.bcounts;
    if (work.sci.nfiles == 1) work.sci.bps = work.bsi.bits_sample;
    else if (work.sci.bps != work.bsi.bits_sample) work.sci.stat_err ++;
    switch (work.sci.bps) {
    case 1:
        for (jj = 0; jj < 2; jj++)
            if (fabs(work.sci.pc[jj] / nf - targ[0][jj]) > work.sci.tol)
                work.sci.stat_err ++;
        break;
    case 2:
        for (jj = 0; jj < 4; jj++)
            if (fabs(work.sci.pc[jj] / nf - targ[1][jj]) > work.sci.tol)
                work.sci.stat_err ++;
        break;
    default:
        work.sci.stat_err++;
        break;
    }

    /* not really a stat error, but if file is invalid... */
    if (work.sgi.sg_version <= 0) work.sci.stat_err++;
}

/*
 * Accumulate time extremes, number of packets, and frame count max
 */
static void adjust_times(void)
{
    if ((work.sgi.first_secs < work.sci.alpha_s) || 
        ( (work.sgi.first_secs == work.sci.alpha_s &&
           work.sgi.first_frame < work.sci.alpha_fr) ) ) {
        work.sci.alpha_s = work.sgi.first_secs;
        work.sci.alpha_fr = work.sgi.first_frame;
    }
    if ((work.sgi.final_secs > work.sci.omega_s) || 
        ( (work.sgi.final_secs == work.sci.omega_s &&
           work.sgi.final_frame > work.sci.omega_fr) ) ) {
        work.sci.omega_s = work.sgi.final_secs;
        work.sci.omega_fr = work.sgi.final_frame;
    }
    work.sci.npkts += work.sgi.total_pkts;

    if (work.sci.nfiles == 1) work.sci.bpp = work.sgi.read_size;
    else if (work.sci.bpp != work.sgi.read_size) work.sci.time_err ++;

    if (work.sci.nfiles == 1) work.sci.ref_epoch = work.sgi.ref_epoch;
    else if (work.sci.ref_epoch != work.sgi.ref_epoch) work.sci.time_err ++;

    if (work.sci.max_fr < work.sgi.frame_cnt_max)
        work.sci.max_fr = work.sgi.frame_cnt_max;
}

/*
 * Accumulate data from each file scan check
 */
void m6sc_sr_accum(void)
{
    if (!ispy) return;
    work.sci.nfiles ++;
    accum_stats();
    adjust_times();
}

/*
 * Retrieve a status string for the group (8 data strings)
 * <stream>:<status>:vdif:<starttime>:<duration>:<datasize>:<rate>:0
 *
 * alternate return:
 *  if (!work.sci.scanref[0]) return(NULL);
 * estimating missing bytes requires us to be told the data rate.
 */
char *m6sc_sr_status(const char *scanref)
{
    int missing = 0;
    double duration, datasize, streamrate, frame_count;
    char *st, *startime;

    if (!ispy) return(NULL);
    if (!work.sci.scanref[0]) return(m6sc_NULL_STATUS);
    if (strcmp(scanref, work.sci.scanref)) return(m6sc_NULL_STATUS);

    /* if we haven't actually checked anything */
    if (2000000000 == work.sci.alpha_s) return(m6sc_NULL_STATUS);

    startime = sg_vextime(work.sci.ref_epoch & 0x3F, work.sgi.first_secs);
    st = (work.sci.time_err ? "time?" : (work.sci.stat_err ? "data?" : "OK"));
    if ((st[1] == 'K') && (work.sci.nfiles < work.sci.nf_exp)) st = "ok";

    frame_count = (work.sci.exp_fr > 0) ? work.sci.exp_fr : work.sci.max_fr;
    duration = (frame_count > 0.0)
             ? (double)(work.sci.omega_fr - work.sci.alpha_fr) / frame_count
             : 0.0;
    duration += (double)(work.sci.omega_s - work.sci.alpha_s);
    datasize = (double)work.sci.npkts * (double)work.sci.bpp / 1e9;

    streamrate = (duration > 0.0) ? 8.0 * datasize / duration : 0.0;
    // TODO: account for missing bytes

    snprintf(work.sci.status, sizeof(work.sci.status),
        "%s:%s:vdif:%s:%.3f:%.3f:%.3f:%d",
        work.sci.scanref, st /*format*/, startime,
        duration, datasize, streamrate, missing);
    return(work.sci.status);
}

/*
 * Return the internal structure in a string format, so that when
 * something is failing or not understood one can debug it...
 */
char *m6sc_sr_scinfo(void)
{
    static char details[4000];
    snprintf(details, sizeof(details)-2,
        "%s\n"
        "nfiles  = %d (%d) bps=%d\n"
        "pc[0..3]= %.3f %.3f %.3f %.3f\n"
        "stat_err= %d time_err=%d max_fr=%d (%d) npkts=%d bpp=%d\n"
        "alpha   = %d@%d+%d\n"
        "omega   = %d@%d+%d\n",
        work.sci.status, work.sci.nfiles, work.sci.nf_exp, work.sci.bps,
        work.sci.pc[0] / (double)work.sci.nfiles,
        work.sci.pc[1] / (double)work.sci.nfiles,
        work.sci.pc[2] / (double)work.sci.nfiles,
        work.sci.pc[3] / (double)work.sci.nfiles,
        work.sci.stat_err, work.sci.time_err,
        work.sci.max_fr, work.sci.exp_fr, work.sci.npkts, work.sci.bpp,
        work.sci.ref_epoch, work.sci.alpha_s, work.sci.alpha_fr,
        work.sci.ref_epoch, work.sci.omega_s, work.sci.omega_fr);
    return(details);
}

/*
 * Capture the processing time; if elapsed is 0, we want to get
 * the start time; otherwise, compute elapsed process time.
 */
#ifdef DEAD_OLD_CODE
static double capture_process_secs(int elapsed)
{
    static struct timeval now;
    double dnow;
    dnow = (!gettimeofday(&now, 0))
         ? (double)now.tv_sec + 1e-6 * (double)now.tv_usec
         : 0.0;
    if (elapsed) {
        if (work.file_process_secs > 0.0 && dnow > 0.0)
            dnow -= work.file_process_secs;
        else
            dnow = 0.0;
    }
    return(dnow);
}
#endif /* DEAD_OLD_CODE */

/*
 * Handlers called for SIGQUIT and SIGALRM
 */
static void progress(int signum)
{
    if (signum != SIGQUIT) return;
    fprintf(stdout,
        "%05d:%s (%lu pass %lu fail %lu time %lu fill)/%lu\n",
        work.cur_numb, work.cur_file,
        work.pkts_passed, work.pkts_failed,
        work.pkts_timing, work.pkts_filled, work.pkts_tested);
}
static void all_done(int signum)
{
    if (signum != SIGALRM && signum != SIGINT && signum != SIGBUS) return;
    if (signum == SIGALRM) fprintf(stdout, "alarm: Timeout!\n");
    if (signum == SIGINT) fprintf(stdout, "signal: Interrupt!\n");
    if (signum == SIGBUS) fprintf(stdout, "signal: bus error!\n");

    /* now perform the cleanup work from m6sc_per_file() */
    sg_close(&work.sgi);

    work.file_process_secs = current_or_elapsed_secs(&work.file_start_secs);
    work.total_process_secs += work.file_process_secs;
    work.avail_process_secs -= work.file_process_secs;

    if (work.flist_only)          flist_report();
    else                          summary_report();
    exit(0);
}

/*
 * Handler for bus errors; see sigaction() man page for details.
 * We have no use for the context pointer (ucontext_t *) at the
 * moment; but it looks like one can in general save previous context
 * and recover....
 */
static void buserror(int signum, siginfo_t *info, void *cptr)
{
    if (signum != SIGBUS) return;
    fprintf(stderr, "signal: bus error at address %p, after %luB!\n",
        info->si_addr, info->si_addr - work.sgi.smi.start);
    switch(info->si_code) {
    case BUS_ADRALN: fprintf(stderr,"Invalid address alignment.\n"); break;
    case BUS_ADRERR: fprintf(stderr,"Nonexistent physical address.\n"); break;
    case BUS_OBJERR: fprintf(stderr,"Object-specific hardware error.\n"); break;
    default: fprintf(stderr,"No idea what happened here.\n"); break;
    }

    /* try to end gracefully */
    all_done(signum);
    exit(3);
}

/*
 * SIGQUIT is made available to the human for a progress report
 * SIGALRM is set to limit total processing time.
 *
 * Cannot set signals if python is in use (multi-threaded app),
 * but still need to compute and respect the time limits.  This
 * is controlled by a global variable (ispy) which limits use of
 * set_alarm_sig().  (If alarms were set under python usage, the
 * main application would get clobbered.)
 *
 * For disk corruption, we also look for bus errors.
 */
static int set_alarm_sig(int files)
{
    struct sigaction bail, chck, berr;
    unsigned int alarm_secs;

    chck.sa_handler = progress;
    sigemptyset(&chck.sa_mask);
    chck.sa_flags = 0;

    berr.sa_sigaction = buserror;
    sigemptyset(&berr.sa_mask);
    berr.sa_flags = SA_SIGINFO;

    work.total_process_secs = 0.0;
    if (work.alarm_secs > 0.0) {
        bail.sa_handler = all_done;
        sigemptyset(&bail.sa_mask);
        bail.sa_flags = 0;
        if (sigaction(SIGALRM, &bail, (struct sigaction*)0))
            return(perror("sigaction"),1);
        work.avail_process_secs = 1.2 * files * work.alarm_secs;
        alarm_secs = ceil(work.avail_process_secs);
        if (verb>1) fprintf(stdout,
            "alarm: Terminating after %u (%g) secs\n",
                alarm_secs, work.avail_process_secs);
        alarm(alarm_secs);

        if (sigaction(SIGINT, &bail, (struct sigaction*)0))
            return(perror("sigaction"),1);
    }

    if (sigaction(SIGBUS, &berr, (struct sigaction*)0))
        return(perror("sigaction"),1);
    if (sigaction(SIGQUIT, &chck, (struct sigaction*)0))
        return(perror("sigaction"),1);
    if (verb>1) fprintf(stdout,
        "alarm: Send SIGQUIT (kill -3, or ^\\) for a progress report\n");
    return(0);
}
static int set_alarm_etc(int files)
{
    if (!ispy) return(set_alarm_sig(files));
    work.total_process_secs = 0.0;
    if (work.alarm_secs > 0.0)
        work.avail_process_secs = 1.2 * files * work.alarm_secs;
    return(0);
}

/*
 * Help for the few remaining options
 */
static int help_chk_opt(void)
{
    fprintf(stdout, "Checker options include:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  version          of m6support toolset (%g)\n",
        M6_VERSION);
    fprintf(stdout, "  alarm=<float>    per-file check time limit (%g s)\n",
        work.alarm_secs);
    fprintf(stdout, "  seed=<int>       seed for trials (%u)\n",
        work.trial_seed);
    fprintf(stdout, "  sgv=<int>        internal verbosity (%u) on sg lib\n",
        work.sgi.verbose);
    fprintf(stdout, "  loops=<int>      approx # spots to check (%u)\n",
        work.pkts_loops);
    fprintf(stdout, "  runs=<int>       approx # packets/spot (%u)\n",
        work.pkts_runs);
    fprintf(stdout, "  starter=<int>    start packet (%u) in sequential mode\n",
        work.pkts_seqstarter);
    fprintf(stdout, "  chans=<csv>      list of channels for stats (\"\")\n"
        /*work.stat_chans*/);
    fprintf(stdout, "  stats=<int>      # octets/packet to test (%u)\n",
        work.stat_octets);
    fprintf(stdout, "  sdelta=<int>     dump statistics after (%u) pkts\n",
        work.stat_delta);
    fprintf(stdout, "  srate=<int>      set rate for sdelta (%lu pkts/s)\n",
        stats_get_packet_rate());
    fprintf(stdout, "  smask=<int>      station bits to check (%u)\n",
        work.station_mask);
    fprintf(stdout, "  exthdr=<str>     examine extended headers (%s)\n",
        work.extend_hchk ? "activated" : "inactive");
    fprintf(stdout, "  ofs=<int>:<int>  user supplied offset and size\n");
    fprintf(stdout, "  flist=<int>      only generate flist if !0 (%u)\n",
        work.flist_only);
    fprintf(stdout, "  mxppsdel=<ns>    search for jumps > this (%u)\n",
        work.pps_search);
    fprintf(stdout, "  threads=0|max    show threads (%d), set max legal\n",
        work.showthreads);
    /* ispy is not settable from command line */
    fprintf(stdout, "\n");
    fprintf(stdout, "Generally speaking, you can increase the amount\n");
    fprintf(stdout, "of checking by increasing loops (how many random\n");
    fprintf(stdout, "locations to check) and/or runs (how many packets\n");
    fprintf(stdout, "in each location to check).  If you set loops=0,\n");
    fprintf(stdout, "the program will start at the beginning and run\n");
    fprintf(stdout, "through the number of packets specified by runs.\n");
    fprintf(stdout, "To do the whole file sequentially, set runs=0.\n");
    fprintf(stdout, "To start in the middle, set starter > 0.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "Use exthdr=help for help with those options.\n");
    fprintf(stdout, "-c exthdr= activates the capability, but you will\n");
    fprintf(stdout, "may only get useful results with some configuration.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "The ofs= argument supplies an offset in the pkt\n");
    fprintf(stdout, "read size where the VDIF packet is found together\n");
    fprintf(stdout, "with that read size.  This is used to \"try harder\n");
    fprintf(stdout, "to find a valid packet if there are invalid packets\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "If a GPS PPS is available in the extended header,\n");
    fprintf(stdout, "you can use the mxppsdel to search for gaps which\n");
    fprintf(stdout, "exceed the value supplied.  This assumes you have\n");
    fprintf(stdout, "used the exthdr option to configure the GPS PPS.\n");
    fprintf(stdout, "Minimal support for threads is provided.  If you\n");
    fprintf(stdout, "set a max value for legal thread id, you will get\n");
    fprintf(stdout, "a list of thread ids that are less than that value.\n");
    return(1);
}

/*
 * If we are called by python, we may need to make a few adjustments
 * In particular, the signals should not be set.
 */
static int init_for_python(void)
{
    ispy = 1;
    return(0);
}

/*
 * Entry to set the options.
 */
int m6sc_set_chk_opt(const char *arg)
{
    if (!arg) return(init_for_python());
    if (!strncmp(arg, "help", 4)) return(help_chk_opt());
    if (!strncmp(arg, "alarm=", 6)) {
        work.alarm_secs = atof(arg+6);
        if (work.alarm_secs > 3600.0) work.alarm_secs = 3600.0;
        if (work.alarm_secs < 0.0) work.alarm_secs = 0.0;
        if (verb>1) fprintf(stdout,
            "opt: Per-file work allowance %g secs\n", work.alarm_secs);
    } else if (!strncmp(arg, "seed=", 5)) {
        work.trial_seed = atoi(arg+5);
        if (verb>1) fprintf(stdout,
            "opt: Testing packets with seed %u\n", work.trial_seed);
    } else if (!strncmp(arg, "sgv=", 4)) {
        work.sgi.verbose = atoi(arg+4);
        if (verb>1) fprintf(stdout,
            "opt: SG verbosity now %d\n", work.sgi.verbose);
    } else if (!strncmp(arg, "loops=", 6)) {
        work.pkts_loops = atoi(arg+6);
        if (verb>1) fprintf(stdout,
            "opt: Looping %u times to hit packets\n", work.pkts_loops);
    } else if (!strncmp(arg, "runs=", 5)) {
        work.pkts_runs = atoi(arg+5);
        if (verb>1) fprintf(stdout,
            "opt: Sequences of %u packets per loop\n", work.pkts_runs);
    } else if (!strncmp(arg, "starter=", 8)) {
        work.pkts_seqstarter = atoi(arg+8);
        if (verb>1) fprintf(stdout,
            "opt: Starting %u packets into file\n", work.pkts_seqstarter);
    } else if (!strncmp(arg, "chans=", 6)) {
        strncpy(work.stat_chans, arg+6, sizeof(work.stat_chans));
        if (verb>1) fprintf(stdout,
            "opt: Statistics on channels %s\n", work.stat_chans);
        //stats_chmask(&work.bsi, work.stat_chans);
    } else if (!strncmp(arg, "stats=", 6)) {
        work.stat_octets = atoi(arg+6);
        if (work.stat_octets > SG_MAX_VDIF_BYTES/8)
            work.stat_octets = SG_MAX_VDIF_BYTES/8;
        if (verb>1) fprintf(stdout,
            "opt: Statistics on %u octets per packet\n", work.stat_octets);
    } else if (!strncmp(arg, "sdelta=", 7)) {
        work.stat_delta = atoi(arg+7);
        if (work.stat_delta < 0) work.stat_delta = 0;
        if (verb>1) fprintf(stdout,
            "opt: Statistics dump every %u packets\n", work.stat_delta);
    } else if (!strncmp(arg, "srate=", 6)) {
        stats_set_packet_rate(atol(arg+6));
        if (verb>1) fprintf(stdout,
            "opt: Statistics pkt rate now %ul pps\n", stats_get_packet_rate());
    } else if (!strncmp(arg, "smask=", 6)) {
        sg_set_station_id_mask(atoi(arg+6));
        work.station_mask = sg_get_station_id_mask();
        if (verb>1) fprintf(stdout,
            "opt: Station Mask is now %x (%x)\n",
            work.station_mask, atoi(arg+6));
    } else if (!strncmp(arg, "exthdr=", 7)) {
#if EXTEND_HCHK
        work.extend_hchk = extended_hdr_opt(arg+7) ? 0 : 1;
#else /* EXTEND_HCHK */
        work.extend_hchk = 0;
#endif /* EXTEND_HCHK */
        if (verb>1) fprintf(stdout,
            "opt: %s extended headers\n", work.extend_hchk
                ? "Examining" : "Ignoring");
    } else if (!strncmp(arg, "ofs=", 4)) {
        sg_set_user_poff_and_size(arg+4);
        if (verb>1) fprintf(stdout,
            "opt: Set packet offset and size using %s\n", arg+4);
    } else if (!strncmp(arg, "flist=", 6)) {
        work.flist_only = atoi(arg+6);
        if (verb>1) fprintf(stdout, work.flist_only
            ? "opt: only generating flist entry"
            : "opt: providing normal summary");
    } else if (!strncmp(arg, "mxppsdel=", 9)) {
        work.pps_search = atoi(arg+9);
        if (verb>1) fprintf(stdout,
            "opt: Searching for PPS jumps > %u\n", work.pps_search);
    } else if (!strncmp(arg, "threads=", 8)){
        work.showthreads = atoi(arg+8);
        if (work.showthreads > 0 || work.showthreads < 1024)
            sg_set_max_legal_thread_id(work.showthreads);
        if (verb>1) fprintf(stdout,
            "opt: %s showing threads; max legal thread id is %d\n",
            work.showthreads ? "am" : "not", sg_get_max_legal_thread_id());
    } else {
        fprintf(stderr, "Unknown option %s\n", arg);
        return(1);
    }
    return(0);
}

/*
 * Get support for python
 */
char *m6sc_get_chk_opt(const char *arg)
{
    static char answer[80];
    if (!strncmp(arg, "alarm", 5)) {
        snprintf(answer, sizeof(answer), "%f", work.alarm_secs);
    } else if (!strncmp(arg, "version", 7)) {
        snprintf(answer, sizeof(answer), "%g", M6_VERSION);
    } else if (!strncmp(arg, "seed", 4)) {
        snprintf(answer, sizeof(answer), "%u", work.trial_seed);
    } else if (!strncmp(arg, "sgv", 3)) {
        snprintf(answer, sizeof(answer), "%u", work.sgi.verbose);
    } else if (!strncmp(arg, "loops", 5)) {
        snprintf(answer, sizeof(answer), "%u", work.pkts_loops);
    } else if (!strncmp(arg, "runs", 4)) {
        snprintf(answer, sizeof(answer), "%u", work.pkts_runs);
    } else if (!strncmp(arg, "starter", 7)) {
        snprintf(answer, sizeof(answer), "%u", work.pkts_seqstarter);
    } else if (!strncmp(arg, "stats", 5)) {
        snprintf(answer, sizeof(answer), "%u", work.stat_octets);
    } else if (!strncmp(arg, "sdelta", 6)) {
        snprintf(answer, sizeof(answer), "%u", work.stat_delta);
    } else if (!strncmp(arg, "smask", 5)) {
        snprintf(answer, sizeof(answer), "%u", work.station_mask);
    } else {
        /* exthdr and many other things not supported in python */
        return(NULL);
    }
    return(answer);
}

/*
 * Not really needed, but supplied so the option setting can be tracked.
 */
void m6sc_verbosity(const int verbose)
{
    verb = verbose;
}

/*
 * Entry to the checker.  On the first file, it computes how long
 * it will allow processing to run in total, and set an alarm to
 * be sure this is enforced.
 */
int m6sc_per_file(const char *file, int files, int verbose)
{
    static int n = 0;
    static int not_armed = 1;
    Random_Picker *picker;

    verb = verbose;

    if (not_armed || ispy) not_armed = set_alarm_etc(files);
    if (not_armed) return(2);

    if (verb>1) fprintf(stdout,
        "file: Checking-%d: %s with %d to go\n", n, file, files);
    if (work.avail_process_secs < 0.0) {
        fprintf(stdout, "file: Processing time limit exhausted\n");
        return(0);
    }

    /* per-file initializations */
    work.cur_numb = n;
    work.pkts_seqoffset = work.pkts_seqstarter;
    strcpy((work.cur_file = malloc(strlen(file) + 2)), file);
    work.pkts_timing = work.pkts_tested = 0;
    work.pkts_passed = work.pkts_failed = 0;
    work.pkts_filled = 0;
    work.sgi.verbose = verb;
    memset(&work.bsi, 0, sizeof(BSInfo));
    memset(&work.bdel, 0, sizeof(BSInfo));
    memset(&work.blst, 0, sizeof(BSInfo));
#if EXTEND_HCHK
    if (work.extend_hchk) extended_hdr_verb(verb, work.cur_file);
#endif /* EXTEND_HCHK */
    work.file_process_secs = 0.0;
    work.file_start_secs = current_or_elapsed_secs((double *)0);

    (void)sg_open(file, &work.sgi);

    stats_chmask(&work.bsi, work.stat_chans);
    work.bsi.packet_octets = work.sgi.vdif_signature.bits.df_len - 4;
    if (work.bsi.packet_octets > work.stat_octets)
        work.bsi.packet_octets = work.stat_octets;
    work.bsi.bits_sample = work.sgi.vdif_signature.bits.bps + 1;

    if (work.pps_search > 0) {
        switch (work.sgi.sg_version) {
        case SG_VERSION_OK_2: picker = search_packet_sg; break;
        case SG_VERSION_FLAT: picker = search_packet_fl; break;
        default:              picker = search_packet_er; break;
        }
        work.pickername = picker_name(picker);
        check_search(picker);
    } else if (work.pkts_loops > 0) {
        switch (work.sgi.sg_version) {
        case SG_VERSION_OK_2: picker = random_packet_sg; break;
        case SG_VERSION_FLAT: picker = random_packet_fl; break;
        default:              picker = random_packet_er; break;
        }
        work.pickername = picker_name(picker);
        check_random(picker);
    } else {
        switch (work.sgi.sg_version) {
        case SG_VERSION_OK_2: picker = sequence_packet_sg; break;
        case SG_VERSION_FLAT: picker = sequence_packet_fl; break;
        default:              picker = sequence_packet_er; break;
        }
        work.pickername = picker_name(picker);
        check_sequence(picker);
    }

    sg_close(&work.sgi);

    // work.file_process_secs = capture_process_secs(1); // DEAD_OLD_CODE
    work.file_process_secs = current_or_elapsed_secs(&work.file_start_secs);
    work.total_process_secs += work.file_process_secs;
    work.avail_process_secs -= work.file_process_secs;

    if (work.flist_only)          flist_report();
    else                          summary_report();
    free(work.cur_file);
    work.cur_file = 0;
    n++;

    return( work.pkts_failed ? 1 : 0);
}

/* performance stub we don't need */
void sg_advice_term(int mmfd) { return; }

/*
 * eof
 */
