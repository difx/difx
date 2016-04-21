/*
 * $Id: vdifsig.c 3894 2016-04-17 22:30:56Z gbc $
 *
 * This file provides (optional) trapping of bus errors to allow
 * paging over bad blocks (to the extent possible).
 */

#include <errno.h>
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
    vdifuse_trace(VDT("vdsig: bus error (%s) at %p code '%s' counter %d\n"),
        who, addr, story, trap_counter++);
    vdifuse_flush_trace();
}

/*
 * Handlers for bus errors on implicit reads from disk to mmap'd area.
 */
static void bus_error_open(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "open");
    if (!open_ffi || !open_ffi->cntxt) {
        vdifuse_trace(VDT("vdsig: trap on no active open call\n"));
        vdifuse_flush_trace();
        exit(101);
    }
    vdifuse_trace(VDT("vdsig: return from open for %d\n"), open_ffi->fh);
    VDSigContext *cp = (VDSigContext *)open_ffi->cntxt;
    if (!cp->valid_open) exit(102);
    vdsig_trap_rv = -EIO;
    cp->valid_open = 0;
    setcontext(&(cp->open_context));
    vdifuse_trace(VDT("vdsig: setcontext returned!\n"));
    vdifuse_flush_trace();
    exit(103);
}
static void bus_error_read(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "read");
    if (!read_ffi || !read_ffi->cntxt) {
        vdifuse_trace(VDT("vdsig: trap on no active read call\n"));
        vdifuse_flush_trace();
        exit(104);
    }
    vdifuse_trace(VDT("vdsig: return from read for %d\n"), read_ffi->fh);
    VDSigContext *cp = (VDSigContext *)read_ffi->cntxt;
    if (!cp->valid_read) exit(105);
    vdsig_trap_rv = -EIO;
    cp->valid_read = 0;
    setcontext(&(cp->read_context));
    vdifuse_trace(VDT("vdsig: setcontext returned!\n"));
    vdifuse_flush_trace();
    exit(106);
}
static void bus_error_release(int signum, siginfo_t *info, void *cptr)
{
    report_trap(info->si_code, info->si_addr, "release");
    if (!release_ffi || !release_ffi->cntxt) {
        vdifuse_trace(VDT("vdsig: trap on no active release call\n"));
        vdifuse_flush_trace();
        exit(107);
    }
    vdifuse_trace(VDT("vdsig: return from release for %d\n"), release_ffi->fh);
    VDSigContext *cp = (VDSigContext *)release_ffi->cntxt;
    if (!cp->valid_release) exit(108);
    vdsig_trap_rv = -EIO;
    cp->valid_release = 0;
    setcontext(&(cp->release_context));
    vdifuse_trace(VDT("vdsig: setcontext returned!\n"));
    vdifuse_flush_trace();
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
    vdifuse_trace(VDT("vdsig: init: signal enable %d\n"), vp->catchbuserrs);
    if (vp->catchbuserrs == 0) return(0);      /* disabled */
    vdsc = (VDSigContext *)calloc(1, sizeof(VDSigContext));
    if (!vdsc) { perror("vdsig: init: calloc"); return(0); }

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

    vdifuse_trace(VDT("vdsig: init: for %d\n"), ffi->fh);
    return((ffi->cntxt = vdsc));
}

void vdsig_free(FFInfo *ffi)
{
    /* free all the resources */
    vdifuse_trace(VDT("vdsig: done\n"));
    free(ffi->cntxt);
    ffi->cntxt = 0;
}

void vdsig_open_prep(FFInfo *ffi)
{
    VDSigContext *cp = init_sig_context(ffi);
    if (!cp || cp->catchbuserrs) return;
    open_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_open, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (open set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_open = !getcontext(&cp->open_context); 
}
void vdsig_open_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdifuse_trace(VDT("vdsig: open result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    vdifuse_trace(VDT("vdsig: open done with %d\n"), ffi->fh);
    if (vdsig_trap_rv) vdifuse_trace(VDT("vdsig: rv open %d on %d\n"),
        vdsig_trap_rv, open_ffi->fh);
    open_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (open clr)\n"));
}

void vdsig_read_prep(FFInfo *ffi)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (!cp || !cp->catchbuserrs) return;
    read_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_read, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (read set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_read = !getcontext(&cp->read_context); 
}
void vdsig_read_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdifuse_trace(VDT("vdsig: read result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    if (vdsig_trap_rv) vdifuse_trace(VDT("vdsig: rv read %d on %d\n"),
        vdsig_trap_rv, read_ffi->fh);
    read_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (read clr)\n"));
}

void vdsig_release_prep(FFInfo *ffi)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (!cp || !cp->catchbuserrs) return;
    release_ffi = ffi;
    if (sigaction(SIGBUS, &cp->siga_release, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (release set)\n"));
    vdsig_trap_rv = 0;
    cp->valid_release = !getcontext(&cp->release_context); 
}
void vdsig_release_done(FFInfo *ffi, int result)
{
    VDSigContext *cp = (VDSigContext *)ffi->cntxt;
    if (result < 0) vdifuse_trace(VDT("vdsig: release result %d\n"), result);
    if (!cp || !cp->catchbuserrs) return;
    if (vdsig_trap_rv) vdifuse_trace(VDT("vdsig: rv release %d on %d\n"),
        vdsig_trap_rv, release_ffi->fh);
    release_ffi = 0;
    /* disable the handler and restore the default */
    if (sigaction(SIGBUS, &cp->defl, (struct sigaction*)0))
        vdifuse_trace(VDT("vdsig: sigaction nonzero return (release clr)\n"));
    vdsig_free(ffi);
}

/*
 * eof
 */
