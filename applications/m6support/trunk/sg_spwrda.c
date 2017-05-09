/*
 * $Id: sg_spwrda.c 4289 2017-05-01 17:17:07Z gbc $
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
#include "vdifuse.h"

#define RHMULT 4

/* to hold the work assignment */
typedef struct rh_task_data { int fd; off64_t offset; size_t count; } Rhtd;

/*
 * this call blocks until the requested data is read into memory
 */
void *readahead_task(void *arg)
{
    Rhtd *work = (Rhtd *)arg;
    int rv = readahead(work->fd, work->offset, work->count);
    vdifuse_trace(VDT("RH exit %d, %d\n"), rv, errno);
    pthread_exit(&rv);
    return(0);
}

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
    vdifuse_trace(VDT("RH-%lu on %d at %lu for %lu\n"),
        tid, task_data.fd = fd, task_data.offset, task_data.count);
}

/*
 * eof
 */
