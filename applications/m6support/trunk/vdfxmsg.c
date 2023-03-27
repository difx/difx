/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdfxmsg.c 5714 2023-03-10 21:31:00Z gbc $
 *
 * This provides some difxmessage glue, should someone want that.
 *
 * Leaving out the calls to vdifuse trace eliminates some compiler gyrations.
 */

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "vdfxmsg.h"

#if HAVE_DIFXMESSAGE
void difxmsg_init_message(void)
{
    char hostname[DIFX_MESSAGE_LENGTH];
    gethostname(hostname, sizeof(hostname)-1);
    difxMessageInitFull(-1, "vdifuse", hostname);
    difxMessageSendDifxAlert("vdifuse started", DIFX_ALERT_LEVEL_INFO);
    difxMessagePrint();
}
void difxmsg_open_message(char *fusename)
{
    DifxMessageMark6Status m6st;
    memset(&m6st, 0x00, sizeof(m6st));
    m6st.state = MARK6_STATE_OPEN;
    snprintf(m6st.scanName, sizeof(m6st.scanName)-1, "%s", fusename);
    difxMessageSendMark6Status(&m6st);
}

void difxmsg_release_message(char *fusename)
{
    DifxMessageMark6Status m6st;
    memset(&m6st, 0x00, sizeof(m6st));
    m6st.state = MARK6_STATE_CLOSE;
    snprintf(m6st.scanName, sizeof(m6st.scanName)-1, "%s", fusename);
    difxMessageSendMark6Status(&m6st);
}

/* TODO: use gettimeofday() etc to report a read rate (DIFX) */
void difxmsg_read_message(off_t foffset, char *fusename, double rate)
{
    char do_difxmsg_report = ((rand() % 10000) == 0);
    if (do_difxmsg_report) {
        DifxMessageMark6Status m6st;
        memset(&m6st, 0x00, sizeof(m6st));
        m6st.state = MARK6_STATE_PLAY;
        m6st.position = foffset;
        m6st.rate = rate;
        snprintf(m6st.scanName, sizeof(m6st.scanName)-1, "%s", fusename);
        difxMessageSendMark6Status(&m6st);
    }
}
#else /* HAVE_DIFXMESSAGE */
/* stubs are in vdfxmsg.h */
#endif /* HAVE_DIFXMESSAGE */

/*
 * eof
 */
