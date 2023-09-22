/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdfxmsg.h 5704 2023-03-09 22:18:13Z gbc $
 *
 * This provides some difxmessage glue, should someone want that.
 */
#ifndef vdfxmsg_h
#define vdfxmsg_h

#if HAVE_CONFIG_H
    #include "config.h"
    #if HAVE_DIFXMESSAGE
        #include <difxmessage.h>
        #include <unistd.h>
    #endif /* HAVE_DIFXMESSAGE */
#endif /* HAVE_CONFIG_H */

#ifndef HAVE_DIFXMESSAGE
#define HAVE_DIFXMESSAGE 0
#endif /* HAVE_DIFXMESSAGE */

#if HAVE_DIFXMESSAGE
void difxmsg_init_message(void);
void difxmsg_open_message(char *fusename);
void difxmsg_release_message(char *fusename);
void difxmsg_read_message(off_t foffset, char *name, double rate);
#else /* HAVE_DIFXMESSAGE */
/* stub out the calls */
#define difxmsg_init_message() do { ; } while(0)
#define difxmsg_open_message(X) do { ; } while(0)
#define difxmsg_release_message(X) do { ; } while(0)
#define difxmsg_read_message(X,Y,Z) do { ; } while(0)
#endif /* HAVE_DIFXMESSAGE */

#endif /* vdfxmsg_h */
/*
 * eof
 */
