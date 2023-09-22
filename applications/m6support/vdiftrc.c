/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdiftrc.c 5775 2023-03-27 14:21:32Z gbc $
 *
 * This file provides support for the fuse interface.
 * Here we provide a timing trace capability and a bread
 * crumb capability into separate files.  Previously they
 * were combined and it was a pain to sort through them.
 *
 * NOTION: it should be possible to reroute sg_access:sgalog
 *
 * vdifuse_trace is available in this file (duh).
 */

#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include "vdifuse.h"

#define NUM_BCS 300
#define MAX_BCS 480     /* 6 lines */

static FILE *vdflog = 0;
static char vdtracename[64], vdbreadname[64];
static FILE *vdtracefp = 0, *vdbreadfp = 0;
static struct timeval born, mark;
static char marker[10] = "none";

static int bc_slot = 0;
static char bc_mess[NUM_BCS][2*MAX_BCS];

static pthread_mutex_t bread_mutex = PTHREAD_MUTEX_INITIALIZER;

/*
 * Function to generally support vdflog activity.
 *
 * This replaces the more verbose construct:
 *  if (vdifuse_debug>gate) fprintf(vdflog, ...)
 * with
 *  vdiflog(gate, ....)
 * this is the one file where we cannot use this macro:
 */
#ifdef vdiflog
#undef vdiflog
#endif /* def vdiflog */
void vdiflog(int gate, char *fmt, ...)
{
    va_list args;
    if (vdifuse_debug <= gate) return;
    va_start(args, fmt);
    vfprintf(vdflog, fmt, args);
    va_end(args);
}
/*
 * This sets it to stdout or a file that is opened
 */
void setvdiflog(char *logfile)
{
    if (!logfile) vdflog = stdout;
    else vdflog = fopen(logfile, "w");
}
FILE *getvdiflog(void)
{
    return(vdflog);
}
int getvdiflogfileno(void)
{
    return(fileno(vdflog));
}

static void trace_timestamp(const struct timeval *tvp,
    const struct timeval *rfp, char *l1, char *l2)
{
    struct tm *gm = gmtime(&tvp->tv_sec);
    int dms = 0, dus = 0;
    if (tvp->tv_usec < rfp->tv_usec) {
        dus = 1000000;
        dms = 1;
    }
    fprintf(vdtracefp,
        "%7lu.%06lu %s(<-%s) at %04d-%02d-%02dT%02d:%02d:%02d.%06ld trace\n",
        tvp->tv_sec - rfp->tv_sec - dms,
        tvp->tv_usec - rfp->tv_usec + dus, l2, l1,
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
    snprintf(vdtracename, sizeof(vdtracename),
        "/tmp/vdifuse-trace.%d", getpid());
    vdtracefp = fopen(vdtracename, "w");
    if (!vdtracefp) { perror("vdifuse_mktrace:trace"); return; }
    snprintf(vdbreadname, sizeof(vdbreadname),
        "/tmp/vdifuse-bread.%d", getpid());
    vdbreadfp = fopen(vdbreadname, "w");
    if (!vdbreadfp) { perror("vdifuse_mktrace:bread"); return; }
    vdiflog(0, "Trace log is to %s\n", vdtracename);
    vdiflog(0, "Bread Crumbs to %s\n", vdbreadname);
    /* ok, start your engines... */
    trace_timestamp(&born, &born, "none", VDT("birth"));
    mark = born;
    vdifuse_marker(VDT("birth"));
    vdifuse_trace(VDT("Trace started, cache %s\n"), cache);
    if (!getcwd(cwd, sizeof(cwd))) sprintf(cwd, "Directory unknown");
    vdifuse_trace(VDT("Working in %s\n"), cwd);
    vdifuse_trace(VDT("Mount point is %s\n"), mount);
    vdifuse_trace(VDT("Debug level is %d\n"), vdifuse_debug);
    vdifuse_flush_bread("Starting the trail");
}

/*
 * Access for the signal handler
 */
char *vdifuse_tracename(void) { return(vdtracename); }
char *vdifuse_breadname(void) { return(vdbreadname); }

/*
 * Note time relative to a marker
 */
void vdifuse_marker(char *what)
{
    struct timeval tv;
    if (gettimeofday(&tv, 0)) { perror("gettimeofday"); return; }
    trace_timestamp(&tv, &born, marker, what);
    //trace_timestamp(&tv, &mark, marker, what);
    strncpy(marker, what, sizeof(marker)-1);
    mark = tv;
}

/*
 * If we opened trace/bread files, we will want to remove it
 * unless we had an abnormal exit (rv != 0).  Otherwise the
 * files will accumulate in /tmp and that is annoying.
 */
void vdifuse_rmtrace(int rv)
{
    vdifuse_trace(VDT("Trace finished rv=%d vd=%d tp=%p bp=%p\n"),
        rv, vdifuse_debug, vdtracefp, vdbreadfp);
    vdifuse_flush_bread("end of the trail");
    if (vdtracefp && (rv == 0)) unlink(vdtracename);
    if (vdbreadfp && (rv == 0)) unlink(vdbreadname);
    if (vdflog) fclose(vdflog);
    if (vdifuse_mount_realpath) free(vdifuse_mount_realpath);
}

/*
 * Common workhorse for calling vfprintf()
 *
 * The buf (with its len) is used for vdiferror().
 */
static void vdifuse_trace_args(char *buf, int len, char *fmt, va_list args)
{
    struct timeval tv;
    int dms = 0, dus = 0;
    if (!vdtracefp) return;
    if (gettimeofday(&tv, 0)) { perror("gettimeofday"); return; }
    if (tv.tv_usec < born.tv_usec) {
        dus = 1000000;
        dms = 1;
    }
    fprintf(vdtracefp, "%7lu.%06lu ",
        tv.tv_sec - born.tv_sec - dms, tv.tv_usec - born.tv_usec + dus);
    /* directly to file or to buffer as appropriate */
    if (buf && len>0) vsnprintf(buf, len, fmt, args);
    else              vfprintf(vdtracefp, fmt, args);
}

/*
 * Wrapper for vfprintf for the trace commentary
 */
void vdifuse_trace(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vdifuse_trace_args(0, 0, fmt, args);
    va_end(args);
}

/*
 * Variant call that uses identical arguments to vdiflog(...)
 *
 * this is the one file where we cannot use this macro:
 */
#ifdef vdiftrace
#undef vdiftrace
#endif /* def vdiftrace */
void vdiftrace(int gate, char *fmt, ...)
{
    va_list args;
    if (vdifuse_debug<=gate) return;
    va_start(args, fmt);
    vdifuse_trace_args(0, 0, fmt, args);
    va_end(args);
}

/*
 * Variant of the above that grabs the last trace and appends it
 * to an error file in the vprocdir area.  We allocate a large buffer
 * and capture the trace with that, then stuff it to the output file.
 * Then, we can also shove the buffer into the "errors" file.
 *
 * This file is called at significant points, so automatic flushing
 * of the trace log and bread crumbs makes sense.
 */
void vdiferror(const FFInfo *ffi, char *fmt, ...)
{
    va_list args;
    char *buf = calloc(4096, 1);
    if (buf) {
        va_start(args, fmt);
        vdifuse_trace_args(buf, 4090, fmt, args);
        va_end(args);
        fputs(buf, vdtracefp);
    } else {
        vdifuse_trace_args(  0,    0, fmt, args);
    }
    strcat(buf, "===\n");
    vproc_update_file(ffi, "errors", buf, VPROC_LENGTHEN);
    if (buf) free(buf);
    vdifuse_flush_bread("error");
}

/*
 * For use as desirable; flushing the log at this point is sane, too.
 */
void vdifuse_flush_trace(void)
{
    if (vdtracefp) fflush(vdtracefp);
    if (vdflog) fflush(vdflog);
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
    if (!vdbreadfp) return;
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
 * Flush out the bread crumbs.  Skip empty slots and nuke slots output.
 */
void vdifuse_flush_bread(char *where)
{
    int bb;
    vdifuse_flush_trace();
    if (!vdbreadfp) return;
    vdifuse_bread(VDT("Bread Crumbs at %s bc_slot = %d/%d\n"),
        where, bc_slot, NUM_BCS);
    pthread_mutex_lock(&bread_mutex);
    for (bb = bc_slot; bb < NUM_BCS; bb++) {
        if (bc_mess[bb][0]) fputs(bc_mess[bb], vdbreadfp);
        bc_mess[bb][0] = 0;
    }
    for (bb = 0;       bb < bc_slot; bb++) {
        if (bc_mess[bb][0]) fputs(bc_mess[bb], vdbreadfp);
        bc_mess[bb][0] = 0;
    }
    pthread_mutex_unlock(&bread_mutex);
    if (vdbreadfp) fflush(vdbreadfp);
}

/*
 * eof
 */
