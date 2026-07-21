/************************************************************************/
/*                                                                      */
/* Deals with everything on the command line.  The averaging mode,      */
/* integration time, verbosity, output filename, and amplitude debias   */
/* "fudge-factor" are all specified through command-line flags.         */
/*                                                                      */
/*      Inputs:         argc, argv      command line args               */
/*                                                                      */
/*      Output:         fpout           output file pointer             */
/*                      configuration   various elements set            */
/*                      return value    0=OK, 1=BAD                     */
/*                                                                      */
/* Created Jan 17 1995 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "average.h"
#include "mk4_util.h"

int
parse_cmdline (int argc,
               char **argv,
               FILE **fpout,
               struct config *configuration)
    {
    char c;
    static char outfile[2000];
    extern char *optarg;
    extern int optind, msglev;
                                        /* Defaults */
    *fpout = stdout;
 /* configuration->snrfact = 0.95;       * Latest empirical estimate */
    configuration->snrfact = 1.0;       /* Presumably corrected now */
    configuration->int_time = 0;        /* Means scan average */
    configuration->coherent = FALSE;
    configuration->cofit_output = FALSE;
    configuration->binary_input = FALSE;
    configuration->multiscan = FALSE;
    configuration->account = FALSE;
    configuration->header = TRUE;
                                        /* parse command line and read in */
                                        /* filename */
    while ((c = getopt (argc, argv, "abchi:l:m:o:s:w:x")) != -1)
        {
        switch (c)
            {
                                        /* Switch on accounting */
            case 'a':
                configuration->account = TRUE;
                break;
                                        /* Expect binary data in pipe from fringex */
            case 'b':
                configuration->binary_input = TRUE;
                break;
                                        /* Switch on cofit mode */
            case 'c':
                configuration->cofit_output = TRUE;
                break;
                                        /* Suppress headers in output */
            case 'h':
                configuration->header = FALSE;
                break;
                                        /* Integration time, sanity checks */
                                        /* come later on */
            case 'i':
                configuration->int_time = get_int_time (optarg);
                break;
                                        /* Verbosity control */
            case 'm':
                if (sscanf (optarg, "%d", &msglev) != 1)
                    {
                    msg ("Invalid -m flag argument '%s'", 2, optarg);
                    msg ("Message level remains at %d", 2, msglev);
                    }
                if (msglev > 3) msglev = 3;
                if (msglev < -3) msglev = -3;
                break;
                                        /* Output override, default stdout above */
            case 'o':
                strcpy (outfile, optarg);
                if ((*fpout = fopen (outfile, "w")) == NULL)
                    {
                    msg ("Could not open output file '%s'", 3, outfile);
                    return (1);
                    }
                break;
                                        /* Override empirically determined snrfact */
            case 's':
                /* originally 0.925 was used here if arg not parsable */
                if (sscanf (optarg, "%f", (&configuration->snrfact)) == 0)
                    {
                    msg ("Unable to parse -s '%s'", 3, optarg);
                    return (1);
                    }
                break;
                                        /* Allow averaging across scan boundaries */
            case 'x':
                configuration->multiscan = TRUE;
                break;

            case '?':
            default:
                syntax("average/parse_cmdline.c");
                return (1);
            }
        }
                                        /* Check for incompatible options */
    if (configuration->multiscan && (configuration->int_time == 0))
        {
        msg ("You must specify a nonzero integration time if you want", 3);
        msg ("to use the -x option", 3);
        return (1);
        }
    if (configuration->cofit_output && (configuration->int_time != 0))
        {
        msg ("You must specify scan integration time in cofit mode", 3);
        return (1);
        }

    msg ("Configuration: integration time = %d, snrfact = %f", 0,
                configuration->int_time, configuration->snrfact);
    msg ("coherent = %d, multiscan = %d", 0,
                configuration->coherent, configuration->multiscan);
                                        /* Input files on command line handled */
                                        /* in main routine. */
                                        /* Better not be any in -b mode! */
    if (configuration->binary_input && (argc > optind))
        {
        msg ("You cannot specify input filenames in -b mode", 3);
        msg ("This mode is reserved for binary pipe data from fringex", 3);
        return (1);
        }

    return (0);
    }
