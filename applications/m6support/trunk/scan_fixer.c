/*

 * $Id: scan_fixer.c 4778 2018-11-19 16:04:50Z gbc $
 *
 * Code to verify scans at time of recording.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "fix_the_file.h"

/*
 * Options
 */
static int verb = 0;
static int prate = 125000;
static int extra = 60;
static off_t start = 0;
static char *ofile = 0;

/*
 * Boilerplate
 */
static int cmdhelp = 0;
static int usage(char *name)
{
    printf("Usage: %s [options] action\n\n", name);
    printf( "where the options are:\n"
            "\n"
            "  -v            verbose, may be repeated for increasingly more\n"
            "  -j <int>      jump this many bytes into the file to start\n"
            "  -r <int>      specify the packets/sec (125000 default)\n"
            "  -e <int>      how many extra seconds of fill/good to allow\n"
            "  -o <string>   output file\n"
            "\n"
            "%s will step through a file of VDIF packets and attempt\n"
            "to recover good packets from it.  If an offset is supplied\n"
            "via the -j option, the search will start at that offset into\n"
            "the file.  The working assumption is that this starting point\n"
            "is a good packet and thus the output file will contain similar\n"
            "packets in sequence, with fill inserted according to the number\n"
            "of missing packets based on the packet rate (supplied by -r).\n"
            "The default rate is 125000 pps which is appropriate to the EHT.\n"
            "The output file size is based on the input file size; you may\n"
            "specify how many additional seconds may be added each time the\n"
            "file needs to grow (based on the new fill packets).\n"
            , name
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
    while ((c = getopt(argc, argv, "vj:o:r:e:")) != -1) switch(c) {
    case 'v': ++verb;                       break;
    case 'j': start = atol(optarg);         break;
    case 'r': prate = atoi(optarg);         break;
    case 'e': extra = atoi(optarg);         break;
    case 'o': ofile = optarg;               break;
    default:                                return(2);
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
    static Fixer fixwrk;
    int     errs = 0;

    /* basic command line parsing */
    if (cmdline(&argc, &argv)) return(!cmdhelp);
    /* run_test() */

    fixwrk.verb = verb;
    fixwrk.prate = prate;
    fixwrk.extra = prate * extra;
    fixwrk.start = start;
    fixwrk.clone = 0;
    fixwrk.ifile = *argv++;
    fixwrk.ofile = ofile;

    errs += fix_the_file(&fixwrk);
    return(errs);
}

/*
 * eof
 */
