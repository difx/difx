/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsig.c 5755 2023-03-26 16:47:18Z gbc $
 *
 * This file provides (optional) trapping of bus errors to allow
 * paging over bad blocks (to the extent possible).
 *
 * vdifuse_trace is available in this file.
 */

#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "vdifuse.h"
#include "vdifsg2.h"
#include "vdifsig.h"

/* flag for context switch with error return value */
volatile int vdsig_trap_rv = 0;

/* these is so the handler can locate its data */
static volatile FFInfo *open_ffi = 0;
static volatile FFInfo *read_ffi = 0;
static volatile FFInfo *release_ffi = 0;

/*
 * share what little is known from the interrupt
 */
static void report_trap(int code, void *addr, char *who)
{
    static volatile int trap_counter = 0;
    static char codes[][80] = {
        "Invalid address alignment.",
        "Nonexistent physical address.",
        "Object-specific hardware error.",
        "No idea what happened here." };
    char *story = "***";
    switch(code) {
    case BUS_ADRALN:    story = codes[0];    break;
    case BUS_ADRERR:    story = codes[1];    break;
    case BUS_OBJERR:    story = codes[2];    break;
    default:            story = codes[3];    break;
    }
    vdiftrace(-1, VDT("bus error (%s) at %p code '%s' counter %d\n"),
        who, addr, story, trap_counter++);
    vdifuse_flush_bread("trap");
}

/*
 * Handlers for bus errors on implicit reads from disk to mmap'd area.
 */
static void bus_error_open(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "open");
    if (!open_ffi || !open_ffi->cntxt) {
        vdiftrace(-1, VDT("trap on no active open call\n"));
        vdifuse_flush_bread("bus_error_open: trap on no active open call");
        exit(101);
    }
    vdiftrace(-1, VDT("return from open for %d\n"), open_ffi->fh);
    VDSigContext *cp = (VDSigContext *)open_ffi->cntxt;
    if (!cp->valid_open) exit(102);
    vdsig_trap_rv = -EIO;
    cp->valid_open = 0;
    setcontext(&(cp->open_context));
    vdiftrace(-1, VDT("setcontext returned!\n"));
    vdifuse_flush_bread("bus_error_open: setcontext returned!");
    exit(103);
}
static void bus_error_read(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "read");
    if (!read_ffi || !read_ffi->cntxt) {
        vdiftrace(-1, VDT("trap on no active read call\n"));
        vdifuse_flush_bread("bus_error_read: trap on no active read call");
        exit(104);
    }
    vdiftrace(-1, VDT("return from read for %d\n"), read_ffi->fh);
    VDSigContext *cp = (VDSigContext *)read_ffi->cntxt;
    if (!cp->valid_read) exit(105);
    vdsig_trap_rv = -EIO;
    cp->valid_read = 0;
    setcontext(&(cp->read_context));
    vdiftrace(-1, VDT("setcontext returned!\n"));
    vdifuse_flush_bread("bus_error_read: setcontext returned!");
    exit(106);
}
static void bus_error_release(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "release");
    if (!release_ffi || !release_ffi->cntxt) {
        vdiftrace(-1, VDT("trap on no active release call\n"));
        vdifuse_flush_bread("bus_error_rel: trap on no active release call");
        exit(107);
    }
    vdiftrace(-1, VDT("return from release for %d\n"), release_ffi->fh);
    VDSigContext *cp = (VDSigContext *)release_ffi->cntxt;
    if (!cp->valid_release) exit(108);
    vdsig_trap_rv = -EIO;
    cp->valid_release = 0;
    setcontext(&(cp->release_context));
    vdiftrace(-1, VDT("setcontext returned!\n"));
    vdifuse_flush_bread("bus_error_rel: setcontext returned!");
    exit(109);
}

/*
 * If the parameters allow it, set up the signal handler.
 */
static VDSigContext *init_sig_context(FFInfo *ffi)
{
    VDSigContext *vdsc;
    VDIFUSEpars *vp = current_cache_pars();

    if (!ffi) return(0);

    if (getenv("VDIFUSE_SIGBUS")) vp->catchbuserrs = 1;
    vdiftrace(-1, VDT("init: signal enable %d\n"), vp->catchbuserrs);
    if (vp->catchbuserrs == 0) return(0);      /* disabled */
    vdsc = (VDSigContext *)calloc(1, sizeof(VDSigContext));
    if (!vdsc) { perror("calloc:init_sig_context"); return(0); }

    /* on/off switch */
    vdsc->catchbuserrs = 1;

    /* set up the bus error handlers */
    vdsc->siga_open.sa_sigaction = &bus_error_open;
    sigemptyset(&vdsc->siga_open.sa_mask);
    vdsc->siga_open.sa_flags = 0;

    vdsc->siga_read.sa_sigaction = &bus_error_read;
    sigemptyset(&vdsc->siga_read.sa_mask);
    vdsc->siga_read.sa_flags = 0;

    vdsc->siga_release.sa_sigaction = &bus_error_release;
    sigemptyset(&vdsc->siga_release.sa_mask);
    vdsc->siga_release.sa_flags = 0;

    vdsc->defl.sa_handler = SIG_DFL;
    sigemptyset(&vdsc->defl.sa_mask);
    vdsc->defl.sa_flags = 0;

    vdiftrace(0, VDT("init: complete\n"));
    return((ffi->cntxt = vdsc));
}

void vdsig_free(FFInfo *ffi)
{
    /* free all the resources */
    vdiftrace(-1, VDT("done\n"));
    free(ffi->cntxt);
    ffi->cntxt = 0;
}

void vdsig_open_prep(FFInfo *ffi)
{
    VDSigContext *cp = init_sig_context(ffi);
    if (!cp || cp->catchbuserrs) return;
    open_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_open, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (open set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_open = !getcontext(&cp->open_context); 
}
void vdsig_open_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdiftrace(-1, VDT("open result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    vdiftrace(-1, VDT("open done with fh %d\n"), ffi->fh);
    if (vdsig_trap_rv) vdiftrace(-1, VDT("rv open %d on %d\n"),
        vdsig_trap_rv, open_ffi->fh);
    open_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (open clr)\n"));
}

void vdsig_read_prep(FFInfo *ffi)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (!cp || !cp->catchbuserrs) return;
    read_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_read, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (read set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_read = !getcontext(&cp->read_context); 
}
void vdsig_read_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdiftrace(-1, VDT("read result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    if (vdsig_trap_rv) vdiftrace(-1, VDT("rv read %d on fh %d\n"),
        vdsig_trap_rv, read_ffi->fh);
    read_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (read clr)\n"));
}

void vdsig_release_prep(FFInfo *ffi)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (!cp || !cp->catchbuserrs) return;
    release_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_release, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (release set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_release = !getcontext(&cp->release_context); 
}
void vdsig_release_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdiftrace(-1, VDT("release result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    if (vdsig_trap_rv) vdiftrace(-1, VDT("rv release %d on fh %d\n"),
        vdsig_trap_rv, release_ffi->fh);
    release_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdiftrace(-1, VDT("sigaction nonzero return (release clr)\n"));
    vdsig_free(ffi);
}

static void saveit(char *file)
{
    char *tmpfile = file, *newfile = malloc(strlen(tmpfile)+10);
    if (!newfile) { perror("saveit"); return; }
    snprintf(newfile, strlen(tmpfile)+9, "%s-save", tmpfile);
    rename(tmpfile, newfile);
    free(newfile);
}
static void handle_info(int signum)
{
    if (signum != SIGQUIT) return;
    vdifuse_flush_bread("trap");
    saveit(vdifuse_tracename());
    saveit(vdifuse_breadname());
}
void vdsig_info(void)
{
    struct sigaction info;
    info.sa_handler = handle_info;
    sigemptyset(&info.sa_mask);
    info.sa_flags = 0;
    if (sigaction(SIGQUIT, &info, (struct sigaction*)0)) {
        vdiftrace(-1, VDT("nonzero from SIGQUIT"));
        vdifuse_flush_bread("info");
    }
}

/*
 * eof
 */
