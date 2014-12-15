/*
 * $Id: per_file.c 2562 2014-10-09 16:39:23Z gbc $
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
    uint32_t    stat_octets;        /* max number of samples/packet */
    uint32_t    station_mask;       /* bits of station to check */
    uint32_t    extend_hchk;        /* check the extended header */

    /* evolving */
    char        *cur_file;          /* name of the current file */
    uint32_t    cur_numb;           /* and its ordinal number */
    uint32_t    pkts_passed;        /* number of packets passing */
    uint32_t    pkts_failed;        /* number of packets failing */
    uint32_t    pkts_tested;        /* number of packets tested */
    uint32_t    pkts_timing;        /* packets w/timing issues */
    double      avail_process_secs; /* time remaining before alarm */
    double      file_process_secs;  /* current file consumption */
    double      total_process_secs; /* total time consumed */
} work = {
    .alarm_secs = 60.0,             /* a minute per file is alot */
    .trial_seed = 17,               /* doesn't really matter */
    .pkts_loops = 5,                /* briefest of touches */
    .pkts_runs = 20,                /* a really small patch */
    .stat_octets = SG_MAX_VDIF_BYTES,
    .station_mask = SG_STATION_MASK
};

static double capture_process_secs(int elapsed);
#define time_to_bail() ((capture_process_secs(1) > work.alarm_secs) ? 1 : 0)

/*
 * Type for a generic packet finder
 */
typedef uint32_t *(Random_Picker)(int *nlp, uint32_t **endp);

/*
 * Developer error: epic fail.
 */
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
    static off_t ww = 0;
    uint32_t *pkt = sg_pkt_by_num(&work.sgi, ww, nlp, endp);
    ww ++;
    return(pkt);
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
    static off_t ww = 0;
    pktp = work.sgi.smi.start + ww * (off_t)work.sgi.read_size;
    *endp = work.sgi.smi.eomem;
    *nlp = (work.sgi.smi.eomem - pktp) / (off_t)work.sgi.read_size;
    pktp += work.sgi.pkt_offset;
    ww ++;
    return((uint32_t *)pktp);
}

/*
 * For the number of requested loops, check the
 * packet and its statistics, as requested.
 */
static void check_random(Random_Picker *rp)
{
    int nl, tt, ii, fails;
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
        fails = sg_pkt_check(&work.sgi, pkt, nl, end);
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
        if (time_to_bail()) break;
    }
}

/*
 * Examine work.pkts_runs packets in order
 */
static void check_sequence(Random_Picker *rp)
{
    int tt, nl, num_check, fails;
    uint32_t *pkt, *end;
    num_check = (work.pkts_runs == 0) ? work.sgi.total_pkts : work.pkts_runs;
    for (tt = 0; tt < num_check; tt++) {
        pkt = (*rp)(&nl, &end);
        if (!pkt) {
            work.pkts_failed ++;
            work.pkts_tested ++;
            continue;
        }
        work.pkts_tested += 1;
        fails = sg_pkt_check(&work.sgi, pkt, 1, end);
        work.pkts_failed += fails;
        work.pkts_passed += (nl - fails);
        if (fails == 0)
            work.pkts_timing += sg_pkt_times(&work.sgi, pkt, 1, end);
        if (fails == 0 && work.stat_octets > 0)
            stats_check(&work.bsi, (uint64_t*)(pkt));
#if EXTEND_HCHK
        if (work.extend_hchk) extended_hdr_chk(pkt);
#endif /* EXTEND_HCHK */
        if (time_to_bail()) break;
    }
}

/*
 * Provide a summary report
 */
static void summary_report(void)
{
    static char lab[] = "0000               ";
    
    snprintf(lab, 6, "%05d", work.cur_numb);
    if (verb>0) fprintf(stdout,
        "%s:Report for %s (%.1fms) %s\n"
        "%s:check %u pass + %u fail | %u timing / %u tested\n",
        lab, work.cur_file, 1.0e3 * work.total_process_secs,
             sg_vextime(work.sgi.ref_epoch, work.sgi.first_secs),
        lab, work.pkts_passed, work.pkts_failed,
             work.pkts_timing, work.pkts_tested);
    strcat(lab, ":");
    if (verb>0) sg_report(&work.sgi, lab);
    strcat(lab, "stats:");
    if (verb>0) stats_report(&work.bsi, lab);
#if EXTEND_HCHK
    if (work.extend_hchk) extended_hdr_sum(work.extend_hchk);
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
    // missing bytes

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

/*
 * Handlers called for SIGQUIT and SIGALRM
 */
static void progress(int signum)
{
    if (signum != SIGQUIT) return;
    fprintf(stdout, "\nFile-%d: %s, (%u pass + %u fail) / %u\n",
        work.cur_numb, work.cur_file,
        work.pkts_passed, work.pkts_failed, work.pkts_tested);
}
static void all_done(int signum)
{
    if (signum != SIGALRM) return;
    fprintf(stdout, "alarm: Timeout!\n");
    work.file_process_secs = capture_process_secs(1);
    summary_report();
    exit(0);
}

/*
 * SIGQUIT is made available to the human for a progress report
 * SIGALRM is set to limit total processing time.
 *
 * Cannot set signals if python is in use (multi-threaded app),
 * but still need to compute and respect the time limits.
 */
static int set_alarm_sig(int files)
{
    struct sigaction bail, chck;
    unsigned int alarm_secs;

    chck.sa_handler = progress;
    sigemptyset(&chck.sa_mask);
    chck.sa_flags = 0;

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
    }

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
    fprintf(stdout, "Checker options:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  version          readonly internal version (%g)\n",
        M6_VERSION);
    fprintf(stdout, "  alarm=<float>    per-file check time limit (%g)\n",
        work.alarm_secs);
    fprintf(stdout, "  seed=<int>       seed for trials (%u)\n",
        work.trial_seed);
    fprintf(stdout, "  sgv=<int>        internal sg verbosity (%u)\n",
        work.sgi.verbose);
    fprintf(stdout, "  loops=<int>      approx # spots to check (%u)\n",
        work.pkts_loops);
    fprintf(stdout, "  runs=<int>       approx # packets/spot (%u)\n",
        work.pkts_runs);
    fprintf(stdout, "  stats=<int>      # octets/packet to test (%u)\n",
        work.stat_octets);
    fprintf(stdout, "  smask=<int>      station bits to check (%u)\n",
        work.station_mask);
    fprintf(stdout, "  exthdr=<str>     examine extended headers (%s)\n",
        work.extend_hchk ? "inactive" : "activated");
    /* ispy is not settable from command line */
    fprintf(stdout, "\n");
    fprintf(stdout, "Generally speaking, you can increase the amount\n");
    fprintf(stdout, "of checking by increasing loops (how many random\n");
    fprintf(stdout, "locations to check) and/or runs (how many packets\n");
    fprintf(stdout, "in each location to check).  If you set loops=0,\n");
    fprintf(stdout, "the program will start at the beginning and run\n");
    fprintf(stdout, "through the number of packets specified by runs.\n");
    fprintf(stdout, "To do the whole file sequentially, set runs=0.\n");
    fprintf(stdout, "Use exthdr=help for help with those options.\n");
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
    } else if (!strncmp(arg, "stats=", 6)) {
        work.stat_octets = atoi(arg+6);
        if (work.stat_octets > SG_MAX_VDIF_BYTES/8)
            work.stat_octets = SG_MAX_VDIF_BYTES/8;
        if (verb>1) fprintf(stdout,
            "opt: Statistics on %u octets per packet\n", work.stat_octets);
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
    } else if (!strncmp(arg, "stats", 5)) {
        snprintf(answer, sizeof(answer), "%u", work.stat_octets);
    } else if (!strncmp(arg, "smask", 5)) {
        snprintf(answer, sizeof(answer), "%u", work.station_mask);
    } else {
        /* exthdr not supported in python */
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
    strcpy((work.cur_file = malloc(strlen(file) + 2)), file);
    work.pkts_timing = work.pkts_tested = 0;
    work.pkts_passed = work.pkts_failed = 0;
    work.sgi.verbose = verb;
    memset(&work.bsi, 0, sizeof(BSInfo));
#if EXTEND_HCHK
    if (work.extend_hchk) extended_hdr_verb(verb, work.cur_file);
#endif /* EXTEND_HCHK */

    work.file_process_secs = capture_process_secs(0);

    (void)sg_open(file, &work.sgi);
    work.bsi.packet_octets = work.sgi.vdif_signature.bits.df_len - 4;
    if (work.bsi.packet_octets > work.stat_octets)
        work.bsi.packet_octets = work.stat_octets;
    work.bsi.bits_sample = work.sgi.vdif_signature.bits.bps + 1;
    if (work.pkts_loops > 0) {
        switch (work.sgi.sg_version) {
        case SG_VERSION_OK_2: picker = random_packet_sg; break;
        case SG_VERSION_FLAT: picker = random_packet_fl; break;
        default:              picker = random_packet_er; break;
        }
        check_random(picker);
    } else {
        switch (work.sgi.sg_version) {
        case SG_VERSION_OK_2: picker = sequence_packet_sg; break;
        case SG_VERSION_FLAT: picker = sequence_packet_fl; break;
        default:              picker = sequence_packet_er; break;
        }
        check_sequence(picker);
    }
    sg_close(&work.sgi);

    work.file_process_secs = capture_process_secs(1);
    work.total_process_secs += work.file_process_secs;
    work.avail_process_secs -= work.file_process_secs;

    summary_report();
    free(work.cur_file);
    work.cur_file = 0;
    n++;
    return( work.pkts_failed ? 1 : 0);
}

/*
 * eof
 */
