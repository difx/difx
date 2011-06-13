/************************************************************************/
/*                                                                      */
/* This handles everything to do with the command line.  First, it      */
/* looks for UNIX-style option flags (which must come first), then it   */
/* interprets the remaining arguments as a specification of the         */
/* correlator fringe data files to segment, and uses the                */
/* standard recursive filename extraction in the UTIL library to        */
/* produce a list of files. The list is terminated by a negative number */
/* in the 'order' field of the fstruct structure.                       */
/*                                                                      */
/*      Inputs:         argc, argv              command line arguments  */
/*                                              in standard form        */
/*                                                                      */
/*      Output:         files     fstruct array  List of input data     */
/*                      fxp                     Structure of option     */
/*                                              values                  */
/*                      loops                   structure describing    */
/*                                              instructions            */
/*                                                                      */
/* Rewritten by CJL, Nov 1 6 1995                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "fstruct.h"
#include "fringex.h"

parse_cmdline (argc, argv, files, fxp, loops)
int argc;
char **argv;
fstruct **files;
struct fxparam *fxp;
struct loops *loops;
    {
    int err, dflag, iflag, rflag;
    double raoff, decoff, freq;
    char c, rdarg[100], iarg[100], rarg[256];
    extern char *optarg;
    extern int optind;

    if (argc == 1)
        {
        syntax ();
        return (1);
        }
                                        /* Defaults */
    err = FALSE;
    dflag = iflag = rflag = FALSE;
    fxp->account = FALSE;
                                        /* Interpret command line flags */
    while ((c = getopt (argc, argv, "abcd:f:i:m:nop:qr:")) != -1) 
        {
        switch (c) 
            {
            case 'a':
                fxp->account |= TRUE;
                break;

            case 'b':
                fxp->mode |= BINARYMODE;
                break;

            case 'c':
                fxp->mode |= CMODE;
                break;

            case 'd':
                strcpy (rdarg, optarg);
                dflag = TRUE;
                break;

            case 'f':
                if (sscanf (optarg, "%lf", &freq) != 1)
                    {
                    msg ("Unreadable frequency '%s'", 2, optarg);
                    err = TRUE;
                    }
                else fxp->userfreq = freq;
                break;

            case 'i':
                strcpy (iarg, optarg);
                iflag = TRUE;
                break;

            case 'm':
		msglev = atoi(optarg);
		if (msglev < -3) msglev = -3;
		if (msglev >  3) msglev =  3;
                break;

            case 'n':
                fxp->no_amp_corr = FALSE;
                break;

            case 'o':
                fxp->mode |= OMODE;
                break;

            case 'p':
                if (sscanf (optarg, "%lf,%lf", &raoff, &decoff) != 2)
                    {
                    msg ("Unreadable ra/dec offsets '%s'", 2, optarg);
                    err = TRUE;
                    }
                else
                    {
                    fxp->raoff = raoff;
                    fxp->decoff = decoff;
                    }
                break;

            case 'q':
                fxp->mode |= QMODE;
                break;

            case 'r':
                strcpy (rarg, optarg);
                rflag = TRUE;
                break;

            case '?':
                err = TRUE;
                break;
            default:
                err = TRUE;
                break;
            }
        }
                                        /* Parse looping instructions */
    if (iflag)
        if (parse_iflag (iarg, &(fxp->mode), loops) != 0)
            {
            msg ("Unreadable integration time '%s'", 2, iarg);
            err = TRUE;
            }
    if (dflag)
        if (parse_dflag (rdarg, &(fxp->mode), loops) != 0)
            {
            msg ("Unreadable rate/delay offsets '%s'", 2, rdarg);
            err = TRUE;
            }
    if ((loops->nnsec > 1) && ((loops->nrates>1) || (loops->ndelays>1)))
        {
        msg ("You cannot do multiple rates/delays and", 3);
        msg ("multiple segment lengths in one go", 3);
        err = TRUE;
        }
                                        /* Check for sensible -i mode */
    if (! rflag)
        {
        if ((fxp->mode & SEARCH) || (fxp->mode & NOLOSS))
            {
            msg ("You must use the -r flag when using '-i %s'", 3, iarg);
            err = TRUE;
            }
        }
                                        /* Check for sensible -d mode */
    if (! rflag)
        {
        if (fxp->mode & SRCHPOS)
            {
            msg ("You must use the -r flag when using '-d %s'", 3, rdarg);
            err = TRUE;
            }
        }
                                        /* Read A-file, getting coh. times */
                                        /* as well as filenames in some modes */
    else
        {
        if (filelist (rarg, fxp->mode, files) != 0)
            {
            msg ("Error interpreting A-file list of target data", 2);
            err = TRUE;
            }
        }
                                        /* Bad syntax or -? specified */
    if (err)
        { 
        syntax ();
        return (-1);
        }

                                        /* Now extract all matching fringe files */
    if (! rflag)
        {
        if (get_filelist (argc-optind, argv+optind, 2, files) != 0)
            {
            msg ("Error extracting list of files to process from command line args", 2);
            return (-1);
            }
        }

    return (0);
    }
