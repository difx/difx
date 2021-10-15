/*
 * $Id: vdiftrc.c 5271 2021-08-05 19:46:35Z gbc $
 *
 * This file provides support for the fuse interface.
 * Here we provide a timing trace capability.
 */

#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include "vdifuse.h"

#define NUM_BCS 300
#define MAX_BCS 480     /* 6 lines */

static char vdtmpname[64];
static FILE *vdftmp = 0;
static struct timeval born, mark;
static char marker[10] = "none";

static int bc_slot = 0;
static char bc_mess[NUM_BCS][2*MAX_BCS];

static pthread_mutex_t bread_mutex = PTHREAD_MUTEX_INITIALIZER;

static void trace_timestamp(const struct timeval *tvp,
    const struct timeval *rfp, char *l1, char *l2)
{
    struct tm *gm = gmtime(&tvp->tv_sec);
    int dms = 0, dus = 0;
    if (tvp->tv_usec < rfp->tv_usec) {
        dus = 1000000;
        dms = 1;
    }
    fprintf(vdftmp,
        "%7lu.%06lu %s %s %04d-%02d-%02dT%02d:%02d:%02d.%06ld trace\n",
        tvp->tv_sec - rfp->tv_sec - dms,
        tvp->tv_usec - rfp->tv_usec + dus, l1, l2,
        gm->tm_year + 1900, gm->tm_mon + 1, gm->tm_mday,
        gm->tm_hour, gm->tm_min, gm->tm_sec, tvp->tv_usec);
}

/*
 * Start the trace (or not).
 */
void vdifuse_mktrace(char *cache, char *mount)
{
    static char cwd[4096];
    if (gettimeofday(&born, 0)) { perror("gettimeofday"); return; }
    snprintf(vdtmpname, sizeof(vdtmpname), "/tmp/vdifuse-trace.%d", getpid());
    vdftmp = fopen(vdtmpname, "w");
    if (vdifuse_debug>0) fprintf(vdflog, "Trace log to %s\n", vdtmpname);
    trace_timestamp(&born, &born, "at", "birth");
    mark = born;
    vdifuse_marker("birth");
    vdifuse_trace(VDT("Trace started, cache %s\n"), cache);
    if (!getcwd(cwd, sizeof(cwd))) sprintf(cwd, "Directory unknown");
    vdifuse_trace(VDT("Working in %s\n"), cwd);
    vdifuse_trace(VDT("Mount point is %s\n"), mount);
    vdifuse_trace(VDT("Debug level is %d\n"), vdifuse_debug);
    vdifuse_flush_trace();
}

/*
 * Note time relative to a marker
 */
void vdifuse_marker(char *what)
{
    struct timeval tv;
    if (gettimeofday(&tv, 0)) { perror("gettimeofday"); return; }
    trace_timestamp(&tv, &mark, marker, what);
    strncpy(marker, what, sizeof(marker)-1);
    mark = tv;
}

/*
 * If we opened a trace file, we will want to remove it
 * unless debugging or we had an abnormal exit (rv != 0).
 */
void vdifuse_rmtrace(int rv)
{
    if (vdftmp && (vdifuse_debug) && (rv == 0)) unlink(vdtmpname);
    else vdifuse_trace("Trace finished\n");
    vdifuse_flush_trace();
}

/*
 * Wrapper for vfprintf for the trace commentary
 */
void vdifuse_trace(char *fmt, ...)
{
    static int cnt = 0, clk = 0;
    va_list args;
    struct timeval tv;
    int dms = 0, dus = 0;
    if (!vdftmp) return;
    if (gettimeofday(&tv, 0)) { perror("gettimeofday"); return; }
    if (tv.tv_usec < born.tv_usec) {
        dus = 1000000;
        dms = 1;
    }
    fprintf(vdftmp, "%7lu.%06lu ",
        tv.tv_sec - born.tv_sec - dms, tv.tv_usec - born.tv_usec + dus);
    va_start(args, fmt);
    vfprintf(vdftmp, fmt, args);
    va_end(args);
    if (++cnt == 20) { vdifuse_flush_trace(); cnt = 0; }
    if (++clk == 100) { trace_timestamp(&tv, &born, "elapsed", "at"); clk = 0; }
}

/*
 * For use as desirable; flushing the log at this point is sane, too.
 */
void vdifuse_flush_trace(void)
{
    fflush(vdftmp);
    fflush(vdflog);
}

/*
 * Same idea, but cache the results in a circular buffer to be dumped
 * if requested.
 */

void vdifuse_bread(char *fmt, ...)
{
    static char buf[2*MAX_BCS-1];
    static int serial = 0;
    va_list args;
    struct timeval tv;
    int len;
    if (!vdftmp) return;
    if (gettimeofday(&tv, 0)) { perror("gettimeofday"); return; }
    pthread_mutex_lock(&bread_mutex);
    snprintf(buf, MAX_BCS, "%7lu.%06lu %9d ",
        tv.tv_sec - born.tv_sec, tv.tv_usec, serial++);
    len = strlen(buf);
    va_start(args, fmt);
    vsnprintf(buf + len, 2*MAX_BCS - len - 3, fmt, args);
    va_end(args);
    strncpy(bc_mess[bc_slot++], buf, 2*MAX_BCS-1);
    if (++bc_slot >= NUM_BCS) bc_slot = 0;
    pthread_mutex_unlock(&bread_mutex);
}

/*
 * Flush out the bread crumbs.  Skip empty slots.
 */
void vdifuse_flush_bread(void)
{
    int bb;
    if (!vdftmp) return;
    vdifuse_trace(VDT("Bread Crumbs, bc_slot = %d\n"), bc_slot);
    pthread_mutex_lock(&bread_mutex);
    for (bb = bc_slot; bb < NUM_BCS; bb++) {
        if (bc_mess[bb][0]) fputs(bc_mess[bb], vdftmp);
        bc_mess[bb][0] = 0;
    }
    for (bb = 0;       bb < bc_slot; bb++) {
        if (bc_mess[bb][0]) fputs(bc_mess[bb], vdftmp);
        bc_mess[bb][0] = 0;
    }
    pthread_mutex_unlock(&bread_mutex);
    vdifuse_trace(VDT("Bread Crumbs, done.\n"));
    vdifuse_flush_trace();
}

/*
 * eof
 */
