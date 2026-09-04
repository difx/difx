/************************************************************************/
/*                                                                      */
/* Deals with everything on the command line.                           */
/*                                                                      */
/*      Inputs:         argc, argv      command line args               */
/*                                                                      */
/*      Output:         fpout           output file pointer             */
/*                      display         For graphical output            */
/*                      return value    0=OK, 1=BAD                     */
/*                                                                      */
/* Created October 5 1995 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "cpgplot.h"
#include "search.h"

#include <getopt.h>

int
parse_cmdline (int argc, char **argv, FILE **fpout, int *plot, int *sqp)
    {
    static char device[1000];
    static char outfile[1000];
    int i, c, nxsub = 2, nysub = 2;
    extern char *optarg;
    extern int optind, msglev;
                                        /* Defaults */
    *fpout = stdout;
    *plot = FALSE;
    *sqp = FALSE;
                                        /* parse command line and read in */
                                        /* filename */
    while ((c = getopt (argc, argv, "d:g:m:o:x")) != -1)
        {
        switch (c)
            {
            case 'd':                   /* File away the display string */
                strncpy (device, optarg, sizeof(device));
                *plot = TRUE;
                break;
#if BIGGER
            case 'g':                   /* Specify the gridding */
                if (3 == sscanf(optarg, "%dx%d:%d", &nxsub, &nysub, sqp))
                    {
                    if (getenv("PGPLOT_DEV")) *plot = TRUE;
                    }
                else
                    {
                    nxsub = nysub = 2;
                    *sqp = 0;
                    }
                break;
#else /* BIGGER */
#endif /* BIGGER */
            case 'm':                   /* Verbosity control */
                if (sscanf (optarg, "%d", &msglev) != 1)
                    {
                    msg ("Invalid -m flag argument '%s'", 3, optarg);
                    msg ("Message level remains at %d", 3, msglev);
                    }
                if (msglev > 3) msglev = 3;
                if (msglev < -3) msglev = -3;
                break;

            case 'o':                   /* Override default to stdout */
                strncpy (outfile, optarg, sizeof(outfile));
                if ((*fpout = fopen (outfile, "w")) == NULL)
                    {
                    msg ("Could not open output file '%s'", 3, outfile);
                    return (1);
                    }
                break;

            case 'x':                   /* short for -d /xw */
                strcpy (device, "/XW");
                *plot = TRUE;
                break;

            case '?':
            default:
                syntax("search/parse_cmdline.c");
                return (1);
            }
        }
                                        /* Input files on command line are */
                                        /* handled in main routine. */

                                        /* Open plot device */
    if (*plot)
        {
        if (cpgbeg (0, device, 1, 1) != 1)
            {
            msg ("Could not open pgplot device '%s', abort.", 3, device);
            exit (1);
            }
#if BIGGER
        cpgsubp (nxsub, nysub);
#else /* BIGGER */
        cpgsubp (2, 2);
#endif /* BIGGER */
        }

    return (0);
    }

/*
 * eof vim: nospell
 */
