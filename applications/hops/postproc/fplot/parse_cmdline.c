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

#if HAVE_STRCASECMP
# define CMP    strncasecmp
#else /* HAVE_STRCASECMP */
# define CMP    strncmp
#endif /* HAVE_STRCASECMP */

/*
 * synchronize with fourfit -d filename options:
 * pshardcopy -> -h, hardcopy -> -l, xwindow or psscreen -> -x
 * diskfile:<name> to -d <name> or ps2pdf:<name> to -p <name> .
 */
static void sync_fourfit(int *display, char **file_name)
{
    if        (!CMP(*file_name, "pshardcopy", strlen("pshardcopy"))) {
        *display = HARDCOPY; *file_name = "NotUsedPsHardCopy"; return;
    } else if (!CMP(*file_name, "hardcopy", strlen("hardcopy"))) {
        *display = PRINTLPR; *file_name = "NotUsedHardCopy";   return;
    } else if (!CMP(*file_name, "xwindow", strlen("xwindow"))) {
        *display = XWINDOW;  *file_name = "NotUsedXwindow";    return;
    } else if (!CMP(*file_name, "psscreen", strlen("psscreen"))) {
        *display = GSDEVICE; *file_name = "NotUsedPsScreen";   return;
    } else if (!CMP(*file_name, "diskfile", strlen("diskfile"))) {
        *display = DISKFILE, *file_name = *file_name + 9;      return;
    } else if (!CMP(*file_name, "ps2pdf", strlen("ps2pdf"))) {
        *display = PSTOPDF,  *file_name = *file_name + 7;      return;
    } /* else nothing needs to be done */
}

int
parse_cmdline (int argc, char** argv,
    fstruct** files, int* display, char** file_name, int *poln)
    {
    int err;
    char c;
    extern char *optarg;
    extern int optind, msglev;
                                        /* Interpret command line flags */
    *poln = -1;         /* no select */
    *display = NONE;
    err = FALSE;
    while((c=getopt(argc,argv,"xd:p:hlm:P:")) != -1) 
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
                sync_fourfit(display, file_name);
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

// fourfit/pass_struct.h defines:
// #define POL_LL 0
// #define POL_RR 1
// #define POL_LR 2
// #define POL_RL 3
            case 'P':
                *poln = 0x1111; /* hex read as binary */
                if (optarg[0] == 'X' || optarg[0] == 'L') *poln &= 0x1010;
                if (optarg[0] == 'Y' || optarg[0] == 'R') *poln &= 0x1011;
                if (optarg[1] == 'X' || optarg[1] == 'L') *poln &= 0x0101;
                if (optarg[1] == 'Y' || optarg[1] == 'R') *poln &= 0x0111;
                if (*poln & 0x1100) {
                    msg ("Invalid -P argument '%s'(%x)", 2, optarg, *poln);
                    err = TRUE;
                } else {
                    msg ("Selecting polarization {LL,RR,LR,RL}[b%x]", 1,*poln);
                }
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
