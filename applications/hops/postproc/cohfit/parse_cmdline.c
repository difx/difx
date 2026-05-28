/************************************************************************/
/*                                                                      */
/* Deals with everything on the command line.                           */
/*                                                                      */
/*      Inputs:         argc, argv      command line args               */
/*      Output:         fpout           output file pointer             */
/*                      display         For graphical output            */
/*                      return value    0=OK, 1=BAD, -1 examedit        */
/*                                      some BADs are from errno.h      */
/*                                                                      */
/* Created October 5 1995 by CJL                                        */
/* Hacked up April 26 2026 by GBC                                       */ 
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "mk4_util.h"
#include "cpgplot.h"
#include "cohfit.h"

/* for readability in -x and -d argument parsing below */
#define XD_BARFAGE do {\
    msg("Only one of -x and -d may be given", 3);\
    return(205);\
    } while(0)

/* this routine modifies the -d argument if /pdf is found
 * so that pgplot only sees the part it understands */
char *pdfixer(char *optarg, char **display)
    {
        char *df = NULL, *slash, *co;
        co = malloc(strlen(optarg) + MAX_TXT);
        if (!co) { perror("pdfixer:malloc1"); return(NULL); }
        strcpy(co, optarg);
        msg("Convert optarg '%s' to '%s'", 0, optarg, (*display = co));
        slash = strrchr(co, '/');
        /* are we dealing with .../something/pdf */
        if (slash[1] == 'p' && slash[2] == 'd' && slash[3] == 'f')
            {
            *slash = 0; /* optarg name/ps/pdf -> name/ps */
            msg("into an optarg %s", 0, co);
            df = malloc(strlen(co) + MAX_TXT);
            if (!df) { perror("pdfixer:malloc2"); free(co); return(NULL); }
            strcpy(df, co);
            slash = strrchr(df, '/');
            msg("slash is %s", 0, slash);
            if (!slash) { free(df); free(co); return(NULL); }
            *slash = 0;
            msg("Output converts to %s", 0, df);
            return(df);
            }
        return(NULL);
    }

int parse_cmdline (int argc, char **argv, FILE **fpout, examdata *exdp)
    {
    char c, *pcd, *examp, *devp, *display = NULL;
    int ii, ens, msglevel;
    double cotime;
                                        /* Defaults */
    *fpout = stdout;
                                        /* parse command line and read in */
                                        /* filename */
    while ((c = getopt (argc, argv, "c:d:e:f:lm:o:s:xg:")) != -1)
        {
        switch (c)
            {
            case 'c':
                if (sscanf (optarg, "%lf", &cotime) != 1)
                    {
                    msg ("Invalid -c flag argument '%s'", 3, optarg);
                    msg ("Max. allowed cotime is %g sec", 3, MAX_COTIME);
                    }
                else if (cotime <= 1.0)
                    {
                    msg ("Invalid -c flag (must be >= 1.0)", 2);
                    msg ("Max. allowed cotime is %g sec", 3, MAX_COTIME);
                    }
                else exdp->mxcotime = cotime;
                break;

            /* handle -x and -d cases together */
            case 'd':   /* wedge in a ps2pdf fix; pdfile is malloc'd and used
                         * by the caller to do the ps to pdf conversion, any
                         * /pdf construct is deleted from optarg */
                if (display) XD_BARFAGE;
                msg("-d option with %s", 0, optarg);
                exdp->pdfile = pdfixer(optarg, &display);
                if (exdp->pdfile) msg("PDFile is %s", 0, exdp->pdfile);
            case 'x':
                if (c == 'x' && display) XD_BARFAGE;
                else if (!display) display = "/XW";
                exdp->devp = malloc((ii = strlen(display)) + MAX_TXT);
                if (!exdp->devp) { perror("parse-d:malloc"); return(ENOMEM); }
                strncpy(exdp->devp, display, ii);
                msg("Device is %s, plot = %d", 0, exdp->devp, exdp->pdfile);
                break;

            case 'e':
                examp = malloc((ii = strlen(optarg)) + MAX_TXT);
                if (!examp) { perror("parse-e:malloc"); return(ENOMEM); }
                strncpy(examp, optarg, ii);
                if (!strchr(examp, '%')) strcat(examp, "-%d.data");
                exdp->epat = examp;
                exdp->elen = strlen(exdp->epat);
                msg("exam filename pattern is '%s' (%d)", 0,
                    exdp->epat, exdp->elen);
                break;
            case 'g':
                ens = exam_edit(optarg, exdp);
                /* ens >0 on error, 0 found and used or <0 for template */
                if (ens != 0) return(ens);
                break;

            case 'f':
                if (optarg[0] != '0' ||
                    (optarg[1] != 'x' && optarg[1] != 'X') ||
                    (1 != sscanf(optarg, "%x", &exdp->fitmask)))
                    {
                    msg("The fitmask %s must be 0x.. or 0X..", 3, optarg);
                    return(203);
                    }
                break;
                                        /* Verbosity control */
            case 'm':
                if (sscanf (optarg, "%d", &msglevel) != 1)
                    {
                    msg ("Invalid -m flag argument '%s'", 3, optarg);
                    msg ("Message level remains at %d", 3, msglev);
                    }
                /* bounding it between -3 and 3 */
                if (msglevel > 3) msglevel = 3;
                if (msglevel < -3) msglevel = -3;
                set_msglev(msglevel);
                break;

            case 'l':
                exdp->labels = 1;
                break;
                                        /* fpout is not stdout */
            case 'o':
                if ((*fpout = fopen (optarg, "w")) == NULL)
                    {
                    msg ("Could not open output file '%s'", 3, optarg);
                    return (204);
                    }
                break;

            case 's':
                exdp->closs = (float)atof(optarg);
                if (exdp->closs >= 1.0 || exdp->closs <= 100.0)
                    exdp->closs /= 100.0F;
                if (exdp->closs < 0 || exdp->closs > 1.0)
                    msg("Illegal fractional loss (%g) not [0,1]", 3,
                        exdp->closs);
                break;

            case '?':
                msg("option -%c not known", 3, c);
            default:
                syntax("cohfit/parse_cmdline.c");
                return (205);
            }
        }
                                        /* Input files on command line are */
                                        /* handled in the main routine. */
                                        /* Open plot device */
    if (exdp->devp)
        {
        if (cpgbeg (0, exdp->devp, 1, 1) != 1)
            {
            msg ("Could not open pgplot device '%s', abort.", 3, exdp->devp);
            exit (1);
            }
        cpgsubp (exdp->rnc[1], exdp->rnc[0]);
        }

    return (0);
    }
/*
 * eof vim:nospell
 */
