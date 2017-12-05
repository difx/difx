/*
 * $Id: sg_spwrda.c 4317 2017-05-11 21:11:42Z gbc $
 *
 * Code to support thread creation to use readhead to supplement
 *
 * addr is the address relative to the start of mapped memory,
 * and we are interested in len more bytes.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <time.h>
#include "vdifuse.h"
//#include "sg_access.h"

#define RHMULT 4

typedef struct pidex { pthread_t tid; pthread_mutex_t tmx; } Pidex;
extern Pidex *sg_advice_threads;

/* to hold the work assignment */
typedef struct rh_task_data { int fd; off64_t offset; size_t count; } Rhtd;
typedef struct th_task_data { int f; void *a; size_t c; long p; } Thtd;

/*
 * this call blocks until the requested data is read into memory
 */
void *readahead_task(void *arg)
{
    Rhtd *work = (Rhtd *)arg;
    int rv = readahead(work->fd, work->offset, work->count);
    pthread_exit(&rv);
    return(0);
}

/*
 * Launch a thread to use the readahead system call (which blocks)
 */
void spawn_readahead_thread(int fd, off_t addr, size_t len, size_t max)
{
    pthread_t tid;
    Rhtd task_data;

    /* save arguments */
    task_data.fd = fd;
    task_data.offset = (off64_t)addr;
    task_data.count = len;

    /* ask for several times as much */
    if (task_data.offset + RHMULT*task_data.count < max)
        task_data.count *= RHMULT;

    (void)pthread_create(&tid, NULL, &readahead_task, &task_data);
    vdifuse_trace(VDT("RH-%lu on %02d at %lu for %lu\n"),
        tid, task_data.fd, task_data.offset, task_data.count);
}

/*
 * Thread task to touch pages.  We don't care about the
 * result or blocking on page faults, as long as we get the
 * kernel reading ahead (before the real reader catches up).
 *
 * FIXME:  munmap will occur after sg_advice_term(mmfd) is called.
 *         logic in that call should cause this thread to be cancelled first.
 */
void *toucher_task(void *arg)
{
    Thtd *work = (Thtd *)arg;
    long cnt = work->c, cc, tot = 0L;
    void *addr = work->a;
    size_t page = work->p;
    int ii, f = work->f;
    struct timespec req;

    req.tv_sec  = 0;
    req.tv_nsec = 100;

    for (cc = 0; ++cc < cnt/10; ) {
        pthread_mutex_lock(&sg_advice_threads[f].tmx);
        for (ii = 0; ii < 10; addr += page)
            tot += *(char *)addr;
        pthread_mutex_unlock(&sg_advice_threads[f].tmx);
        // open hole for cancellability
        (void)nanosleep(&req, 0);
    }
    pthread_exit(&tot);
    return(0);
}

/*
 * Launch a thread to step through and touch the pages we expect to use.
 */
void spawn_toucher_thread(int fd, void *addr, size_t len, long page)
{
    pthread_t *tidp = &(sg_advice_threads[fd].tid);
    Thtd task_data;

    /* save arguments */
    task_data.f = fd;
    task_data.a = addr;
    task_data.c = len / page;
    task_data.p = page;

    (void)pthread_create(tidp, NULL, &toucher_task, &task_data);
    vdifuse_trace(VDT("TH-%lu on %02d at %p on %lu pages of %lu\n"),
        *tidp, task_data.f, task_data.a, task_data.c, task_data.p);
    vdifuse_flush_trace();
}

/*
 * eof
 */
