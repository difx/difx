/*
 * $Id: scan_check.c 4508 2017-12-04 20:02:49Z gbc $
 *
 * Code to verify scans at time of recording.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "m6scmodule.h"

/*
 * Global options
 */
static int verb = 0;

/*
 * Boilerplate
 */
static int cmdhelp = 0;
static int usage(char *name)
{
    printf("Usage: %s [options] action\n\n", name);
    printf( "where the options are:\n"
            "\n"
            "  -v            verbose, may be repeated for more\n"
            "  -c <string>   configure the checker\n"
            "\n"
            "%s will check files and return nonzero if something\n"
            "is amiss.  Increasing verbosity will provide more details.\n"
            "Use -c\"help\" for options on configuring the checker;\n"
            "however the defaults are intended to be sensible.\n",
            name
    );
    return(cmdhelp = 1);
}
static int version(char **argv)
{
    if (!argv[1]) return(0);
    if (!strcmp(argv[1], "--help"))    return(usage(argv[0]));
    if ( strcmp(argv[1], "--version")) return(0);
    printf(__FILE__ "\t[" __DATE__ " " __TIME__ "]\n");
    return(cmdhelp = 1);
}
static int options(int argc, char **argv)
{
    int     c;
    if (version(argv)) return(1);
    while ((c = getopt(argc, argv, "vc:s:")) != -1) switch(c) {
    case 'v': m6sc_verbosity(++verb);                       break;
    case 'c': if (m6sc_set_chk_opt(optarg))                 return(1);
              else                                          break;
    default:                                                return(2);
    }
    return(0);
}
static int
cmdline(int *argc, char ***argv)
{
    int     x = options(*argc, *argv);
    *argc -= optind;
    *argv += optind;
    return(x);
}
/* static int run_test(void) */

/*
 * Main Entry.
 */
int main(int argc, char **argv)
{
    int     errs = 0, n = 0;

    /* basic command line parsing */
    if (cmdline(&argc, &argv)) return(!cmdhelp);
    /* run_test() */

    while (argc-- > 0) errs += m6sc_per_file(*argv++, argc, verb);

    return(errs);
}

/*
 * eof
 */
