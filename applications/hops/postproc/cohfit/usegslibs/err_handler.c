/*
 * The normal GSL behavior on error is a message and an abort().
 *   See https://www.gnu.org/software/gsl/doc/html/err.html
 * That is not useful in production, so we install a handler that
 * shares the issue with a a message that cannot be ignored and
 * then continues so that the error status value can be handled
 * by the main programs.  Internally, sub/msg/msg.c does not care
 * about the integer value (its default is 5).
 */
#include "cohfit.h"

/* hold the description until used */
static char err_handled_description[1000];

void clear_err_handler_report(void)
{
    memset(err_handled_description, 0, sizeof(err_handled_description));
}

/* type gsl_error_handler_t, installed with gsl_set_error_handler() */
void err_handler(const char * reason,
    const char * file, int line, int gsl_errno)
{
    const char *progname = get_progname();
    clear_err_handler_report();
    snprintf(err_handled_description, 999,
        "%s: GSL ERROR at line %d of %s\n"
        "%s: GSL ERROR: %s\n"
        "%s: GSL ERROR: %s\n"
        "%s: GSL ERROR: handled\n",
        progname, line, file,
        progname, reason,
        progname, gsl_strerror(gsl_errno),
        progname);
}

char *get_err_handler_report(void)
{
    return(err_handled_description);
}

/*
 * eof vim:nospell
 */
