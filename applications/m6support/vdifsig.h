/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsig.h 5704 2023-03-09 22:18:13Z gbc $
 *
 * This file provides (optional) trapping of bus errors to allow
 * paging over bad blocks (to the extent possible).
 */

#include <signal.h>
#include <ucontext.h>
#include "vdifuse.h"

typedef struct vdif_sig_context {
    FFInfo      *ffi;               /* active file */
    uint32_t    catchbuserrs;       /* enable/disable */
    int         valid_open;         /* if the following is good */
    ucontext_t  open_context;       /* open user context */
    int         valid_read;         /* if the following is good */
    ucontext_t  read_context;       /* read user context */
    int         valid_release;      /* if the following is good */
    ucontext_t  release_context;    /* release user context */
    struct sigaction siga_open;     /* setup for open handler */
    struct sigaction siga_read;     /* setup for read handler */
    struct sigaction siga_release;  /* setup for release handler */
    struct sigaction defl;          /* setup for default handler */
} VDSigContext;

/* signal handler transitions within individual mutices */
extern void vdsig_open_prep(FFInfo *ffi);
extern void vdsig_open_done(FFInfo *ffi, int res);

extern void vdsig_read_prep(FFInfo *ffi);
extern void vdsig_read_done(FFInfo *ffi, int res);

extern void vdsig_release_prep(FFInfo *ffi);
extern void vdsig_release_done(FFInfo *ffi, int res);

extern volatile int vdsig_trap_rv;

/* general catcher of SIGQUIT */
extern void vdsig_info(void);

/*
 * eof
 */
