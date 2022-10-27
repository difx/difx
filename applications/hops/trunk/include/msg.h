/*************************************************************/
/*                                                           */
/* Trivial routine to send messages to stderr, under control */
/* of "importance level" argument.  This allows for user     */
/* selectable verbosity levels for programs.                 */
/*                                                           */
/* Initial version CJL 10 July 1991                          */
/* Modified by CJL, 23 Oct 1991 to allow printf()-style      */
/*      format string and argument list, using varargs       */
/* mod. 2005.4.14 rjc  use stdarg.h instead of varargs.h     */
/*                                                           */
/*************************************************************/
#ifndef MSG_H_
#define MSG_H_

#include <string.h>
#include <stdio.h>
#include <stdarg.h>

extern char progname[256];
extern int msglev;

extern void set_progname(const char* local_progname);
extern const char* get_progname();
extern void set_msglev(int lev);
extern void msg (const char *string, int level, ...);

#endif