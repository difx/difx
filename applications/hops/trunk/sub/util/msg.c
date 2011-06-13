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

#include <stdio.h>
#include <stdarg.h>

					/* To understand this, check stdarg man pages */
void msg (char *string, 
          int level, 
          ...)	

    {
    extern int msglev;
    extern char progname[];
    va_list args;

    if (level >= msglev)
        {
        va_start (args, level);
        fprintf (stderr, "%s: ", progname);
        vfprintf (stderr, string, args);
        putc ('\n', stderr);
        va_end (args);
        fflush (stderr);
        }
    }
