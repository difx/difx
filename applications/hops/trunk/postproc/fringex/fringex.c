/************************************************************************/
/*                                                                      */
/* Fringex - a program to segment the output of fourfit/FRNGE with a    */
/* variety of optional data averaging and geometrical parameters.       */
/* Reads type-2 (fringe) files and writes to stdout.                    */
/*                                                                      */
/* Created by AEER 14 August 1993                                       */
/* Modified to write only A-file format AEER March 1995                 */
/* Reorganized command line and internal structure CJL July 1995        */
/* Corrected time_tag calculation SSD August 1995                       */
/* Major surgery to enable multiple passes/fringe file, CJL October 1995*/
/* Modified to deal with mk4 files                rjc 2007.9.27         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "fringex.h"
#include "fstruct.h"

char progname[8] = "fringex";
int msglev = 1;

main (int argc,
      char **argv)
    {
    int i, j, nfiles, ln, nnsecs, rt, nrates, dl, ndelays, nw, vers;
    int tt_save, so_save;
    static struct fxparam fxp;
    struct loops loops;
    fstruct *files;
    double set_reffreq();
                                        /* Start accounting */
    account ("!BEGIN");
                                        /* Initialize parameter structure */
    clear_fxp (&fxp, ALL);
    clear_loops (&loops);
                                        /* Process the command line */
    if (parse_cmdline (argc, argv, &files, &fxp, &loops) != 0)
        {
        msg ("Fatal error interpreting command line", 2);
        exit(1);
        }
    if (fxp.account) account ("Get inputs");
                                        /* Count # of files requested */
    nfiles = 0;
    while (files[nfiles].order >= 0) nfiles++;
                                        /* Do one fringe file at a time */
    for (i=0; i<nfiles; i++)
        {
                                        /* Get fringe data and root info */
                                        /* into memory */
        clear_fxp (&fxp, FILES);
        if (read_binaries (files+i, &fxp) != 0) 
            continue;
                                        // save times for later passes through loops
        tt_save = fxp.adata.time_tag;
        so_save = fxp.adata.scan_offset;

        if (fxp.account) account ("Read binaries");
                                        /* Deal with automatic looping */
        if (set_loops (&fxp, &loops) != 0)
            continue;
                                        /* Loop over all segment lengths */
        for (ln=0; ln<loops.nnsec; ln++)
            {
            fxp.nsecs = loops.nsecs[ln];
                                        /* Fiddle integration times in q mode */
            if ((fxp.mode & QMODE) && (fxp.nsecs != 9999))
                fxp.nsecs /= 1000.0;
                                        /* Loop over all rates */
            for (rt=0; rt<loops.nrates; rt++)
                {
                fxp.rateoff = loops.rates[rt] * 1e-6;
                if (! (fxp.mode & CMODE))
                    fxp.rateoff += fxp.fringe->t208->resid_rate;
                                        /* Loop over all delays */
                nw = 0;
                for (dl=0; dl<loops.ndelays; dl++)
                    {
                    fxp.delayoff = loops.delays[dl] * 1e-3;
                                        // restore times to values at beginning of loop
                    fxp.adata.time_tag = tt_save;
                    fxp.adata.scan_offset = so_save;
                                        /* Make rates/delays residual unless */
                                        /* in -c mode */
                    if (! (fxp.mode & CMODE))
                        fxp.delayoff += fxp.fringe->t208->resid_mbd;
                                        /* Accumulate numbers for each segment */
                    clear_fxp (&fxp, ACCUMS);
                    accum_segs (&fxp);

                    if (fxp.account) account ("Accumulate");
                                        /* Given channel distribution, can */
                                        /* set the reference frequency parameter */
                    fxp.reffreq = set_reffreq (&fxp);
                                        /* Now loop over segments and fill in */
                                        /* afile fields.  Calculation of phases */
                                        /* is the main complication */
                    for (j=0; j<fxp.nsegs; j++)
                        {
                        if (calc_seg (&fxp, j) != 0)
                            continue;

                        if (fxp.account) account ("Calculate");
                                        /* Write out segment in A-file format */
                        if (fxp.mode & BINARYMODE) 
                            fwrite (&(fxp.adata), sizeof (fringesum), 1, stdout);
                        else 
                            write_fsumm (&(fxp.adata), stdout);

                        if (fxp.account) account ("Write results");
                        nw++;
                        }
                                        /* The average program keys on */
                                        /* version < 0 or the "endofscan" */
                                        /* comment string to trigger scan avg */
                    if (nw > 0)
                        {
                        if (fxp.mode & BINARYMODE) 
                            {
                            vers = fxp.adata.version;
                            fxp.adata.version = -1;
                            fwrite (&(fxp.adata), sizeof (fringesum), 1, stdout);
                            fxp.adata.version = vers;
                            }
                        else fprintf (stdout, "*endofscan\n");
                        }
                    }
                }
            }
        }                               /* End file loop */
    if (fxp.account) account ("!REPORT");

    exit (0);
    }
