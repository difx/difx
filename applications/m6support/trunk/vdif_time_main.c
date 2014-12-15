/*
 * $Id: bmr_time_main.c 978 2011-02-04 18:39:30Z gbc $
 *
 * A simple time utility for times needed in VDIF work.
 */

#include <libgen.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "vdif_time_util.h"

/*
 * Global options
 */
int	    verb = 0;

/*
 * Private data
 */
static int doecho = 0;
static int precis = 9;
static int qstyle = TIMESTYLE_SECS;
static int set_timestyle(char *);

/*
 * Boilerplate
 */
static int cmdhelp = 0;
static int usage(char *name)
{
    printf("Usage: %s [options] time[s]\n", name);
    printf( "where the options are:\n"
	    "  -v            verbose, may be repeated for more\n"
	    "  -q string     type of time response requested\n"
	    "  -E int        epoch to use for Vdif times\n"
	    "  -e            echos the query time and ==\n"
	    "  -t            imprecision treated as truncation\n"
	    "  -r            imprecision treated as rounding\n"
	    "  -p int        specify ns precision (9) of output\n"
	    "\n"
	    "The -q and -E arguments allow %s to be configured\n"
	    "to parse the remaining arguments as times to print\n"
	    "out their interpretation.  The supported types are:\n",
	    basename(name)
    );
    printf( "\n"
	    "  Clock   -- UNIX clock seconds SS[.ss]\n"
	    "  Now     -- +/- seconds (within a week) from now\n"
	    "  DOT     -- [YY]YYMMDDHHMMSS[.ss]\n"
	    "  ISO     -- YYYY-MM-DDTHH:MM:SS[.ss]\n"
            "  MJD     -- ddddd.dddddddddd\n"
	    "  Vex     -- [YY]YYyDOYdHHhMMmSS[.ss]\n"
	    "  Vdif    -- XX@SS.ss XX in 0..63\n"
	    "\n"
	    "Times are treated as calendar times, so leap seconds\n"
	    "will probably not appear unless supported by libc.\n"
	    "Since - is interpreted as options, you will need to\n"
	    "preceed negative relative times with, e.g., a space (' ')\n"
	    "If time is incompletely specified -t and -r affect the\n"
	    "unspecified parts.\n"
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
    int	    c;
    if (version(argv)) return(1);
    while ((c = getopt(argc, argv, "vq:E:etrp:")) != -1) switch(c) {
    case 'v': verb++;						break;
    case 'E': timevdifepoch = atoi(optarg);			break;
    case 'q': if (set_timestyle(optarg))                        return(2);
	      else                                              break;
    case 'e': doecho++;						break;
    case 't': timetruncate = 1;					break;
    case 'r': timetruncate = 0;					break;
    case 'p': time_ns_precision(precis = atoi(optarg));		break;
    default:							return(3);
    }
    if (timevdifepoch < 0 || timevdifepoch > 63) return(1);
    return(0);
}
static int
cmdline(int *argc, char ***argv)
{
    int	    x = options(*argc, *argv);
    *argc -= optind;
    *argv += optind;
    return(x);
}

/*
 * Main Entry.
 */
int main(int argc, char **argv)
{
    int		errs = 0;
    time_t	secs;
    TimeSpec	scns;
    char	bftt[80], bfts[80];

    /* basic command line parsing */
    if (cmdline(&argc, &argv)) return(!cmdhelp);

    if (precis == 0) while (argc--) {
	secs = timeis(*argv);
	timefmtTT(secs, bftt, qstyle);
	if (doecho) {
	    if (verb>1) printf("%20s #%d ", *argv, timestyle(*argv));
	    else        printf("%20s == ", *argv);
	}
	if (verb>0) printf(PRI_TSS "           ", secs);
	printf("%s\n", bftt);
	argv++;
    } else while (argc--) {
	scns = timens(*argv);
	timefmtTS(scns, bfts, qstyle);
	if (doecho) {
	    if (verb>1) printf("%20s #%d ", *argv, timestyle(*argv));
	    else        printf("%20s == ", *argv);
	}
	if (verb>0) printf(PRI_TS " ", scns.tv_sec, scns.tv_nsec);
	printf("%s\n", bfts);
	argv++;
    }

    return(errs);
}

static int set_timestyle(char *type)
{
    if      (!strcmp(type, "Clock")) qstyle = TIMESTYLE_SECS;
    else if (!strcmp(type, "Now"))   qstyle = TIMESTYLE_WEEK;
    else if (!strcmp(type, "DOT"))   qstyle = TIMESTYLE_DOT;
    else if (!strcmp(type, "ISO"))   qstyle = TIMESTYLE_ISO;
    else if (!strcmp(type, "MJD"))   qstyle = TIMESTYLE_MJD;
    else if (!strcmp(type, "Vex"))   qstyle = TIMESTYLE_VEX;
    else if (!strcmp(type, "Vdif"))  qstyle = TIMESTYLE_VDIF;
    else qstyle = -1;
    if (verb>1) printf("# %s is style %d\n", type, qstyle);
    return(qstyle >= 0 ? 0 : 1);
}

/*
 * eof
 */
