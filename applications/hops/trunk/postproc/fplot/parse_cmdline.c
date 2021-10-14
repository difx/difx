/************************************************************************/
/*                                                                      */
/* This handles everything to do with the command line.  First, it      */
/* looks for UNIX-style option flags (which must come first), then it   */
/* interprets the remaining arguments as a specification of the         */
/* correlator fringe data files to display or print, and uses the       */
/* standard recursive filename extraction in the UTIL library to        */
/* produce a list of files. The list is terminated by a negative number */
/* in the 'order' field of the fstruct structure.                       */
/*                                                                      */
/*      Inputs:         argc, argv              command line arguments  */
/*                                              in standard form        */
/*                                                                      */
/*      Output:         files                   fstruct array           */
/*                      display                 type of fringe plot     */
/*                                              display requested       */
/*                                              (defs in fplot.h)       */
/*                                                                      */
/* Created 8 July 1993 by CJL                                           */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include "fstruct.h"
#include "fplot.h"

#include "mk4_util.h"

int
parse_cmdline (int argc, char** argv, fstruct** files, int* display, char** file_name)
    {
    int err;
    char c;
    extern char *optarg;
    extern int optind, msglev;
                                        /* Interpret command line flags */
    *display = NONE;
    err = FALSE;
    while((c=getopt(argc,argv,"xd:p:hlm:")) != -1) 
        {
        switch (c) 
            {
            case 'x':
                if (*display != NONE) err = TRUE;
                else *display = XWINDOW;
                break;

            case 'd':
                if (*display != NONE) err = TRUE;
                else *display = DISKFILE;
                *file_name = optarg;
                break;

            case 'p':
                if (*display != NONE) err = TRUE;
                else *display = PSTOPDF;
                *file_name = optarg;
                break;

            case 'h':
                if (*display != NONE) err = TRUE;
                else *display = HARDCOPY;
                break;

            case 'l':
                if (*display != NONE) err = TRUE;
                else *display = PRINTLPR;
                break;

            case 'm':
                if (sscanf (optarg, "%d", &msglev) != 1)
                    {
                    msg ("Invalid -m flag argument '%s'", 2, optarg);
                    msg ("Message level remains at %d", 2, msglev);
                    }
                if (msglev > 3) msglev = 3;
                if (msglev < -3) msglev = -3;
                break;


            case '?':
                err = TRUE;
                break;
            }
        }
                                        /* Default behaviour */
    if (*display == NONE) *display = GSDEVICE;
                                        /* Bad syntax or -? specified */
    if (err) return (1);
                                        /* Extract all matching fringe files */
    if (get_filelist (argc-optind, argv+optind, 2, files) != 0)
        {
        msg ("Error extracting files to process from command line args", 2);
        return (1);
        }

    return (0);
    }
