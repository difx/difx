/****************************************************************************/
/*                                                                          */
/* This program, aedit, is intended to be a general MkIII correlator data   */
/* display and editing program, which takes advantage of the Xwindow        */
/* environment on UNIX workstations, and is built on the Caltech PGPLOT     */
/* plotting package.  It forms part of an integrated software system for    */
/* the MkIII system under UNIX in general, and HP-UX in particular          */
/*                                                                          */
/* Initial options to aedit are                                             */
/*   -r (file to read commands from on startup),                            */
/*   -f (files to read on startup), and                                     */
/*   -x (use the Xwindow system for program control)                        */
/* This file is merely the upper-level control routine, and most of the     */
/* functionality is contained in subroutines accessed from execute()        */
/*                                                                          */
/* Created 3/22/89 by CJL                                                   */
/* Version 2.0  April 18 1990 CJL                                           */
/* Version 3.0  March 12 1992 CJL                                           */
/* Version 3.1  February 23 1993 CJL                                        */
/* Version 3.2  October 18 1993 CJL                                         */
/* Version 3.3 beta  December 16 1993 CJL                                   */
/* Version 4.0 March 8 1994 CJL                                             */
/* Version 4.1 August 29 1994 CJL                                           */
/* Version 4.2 May 3 1995 CJL                                               */
/* Version 5.0 Jan 31 2001 CJL (enough changes for Mk4 to merit this)       */
/*                                                                          */
/****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "aedit.h"
#include "aedata.h"
#include "usearray.h"
#include "summary.h"

int rscan, cscan, fscan, tscan, qscan,
    rflag, cflag, fflag, tflag, qflag,
    rspace, cspace, fspace, tspace, qspace,
    batch, xwindow, up_to_date, rmdup, 
    psplot_open, plot_open, rundepth, dotype, 
    output_version, data_version;
int qsortstat[10];
int tsortstat[10];
int fsortstat[10];
int csortstat[10];
int rsortstat[10];      /* These arrays are where a record of the sort status */
                        /* of the data is maintained.  The first element is */
                        /* the number of sort passes done, and subsequent */
                        /* elements identify the sort key for each of those */
                        /* passes. See sorter() */
struct inputs inp;
struct datasumm rsumm, csumm, fsumm, tsumm, qsumm;
struct usearray user_param;


char progname[6] = "aedit";
int msglev = 1;

main (argc, argv)
int argc;
char *argv[];
    {
    extern char *optarg;
    extern int optind;
    esum data;
    int file, runfile, c, i, j, n, len, execute(), filelist, ret;
    char run_fname[100], line[257];
    struct com commands[10];    /* Structure contains parsed command info */

    environment();                      /* Set up directories according to env't */
    check_env();                        /* Aedit-specific ones */

    init_inputs();                      /* Initialize everything */
    xwindow = FALSE; batch = FALSE; rmdup = FALSE; plot_open = FALSE;
    psplot_open = FALSE; file = FALSE; runfile = FALSE; rundepth = 0;
    data_version = 0; output_version = 0;
    rscan = cscan = fscan = tscan = qscan = 0; 
    rflag = cflag = fflag = tflag = qflag = 0;
    rspace = cspace = fspace = tspace = qspace = 0;
    dotype = -1;
    for (i=0; i<10; i++) 
        fsortstat[i] = csortstat[i] = rsortstat[i] 
                        = tsortstat[i] = qsortstat[i] = 0;

                                        /* Interpret command line */
    if (parse_cmdline (argc, argv, &xwindow, run_fname, line, &filelist) != 0)
        {
        msg ("Problem parsing command line, abort", 3);
        exit (1);
        }
                                        /* Announce ourselves */
    msg ("\t\tAEDIT VERSION 5.0 --- JAN 2001\n\n", 2);
    
                                        /* Initialize the memory arrays */
/*    rspace = 100;
    cspace = 500;
    fspace = 500;
    tspace = 200;
    qspace = 200; */
    rspace = 5000;
    cspace = 25000;
    fspace = 25000;
    tspace = 10000;
    qspace = 10000;
    data.rdata = (rootarray *) calloc(rspace,sizeof(rootarray));
    data.cdata = (corelarray *) calloc(cspace,sizeof(corelarray));
    data.fdata = (fringearray *) calloc(fspace,sizeof(fringearray));
    data.tdata = (trianglearray *) calloc(tspace,sizeof(trianglearray));
    data.qdata = (quadarray *) calloc(qspace,sizeof(quadarray));

                                        /* If in -b mode, just execute */
                                        /* supplied commands and exit */
    if (strlen (line) > 0)
        {
        batch = TRUE;
        parse_commands (line, commands, &n);
        for (i=0; i<n; i++)
            if ((ret = execute (&data, &commands[i])) < 0) break;
        exit (ret);
        }
                                        /* Set up the X-window interface */
    if(xwindow) 
        {
        msg("Xwindow option not yet operational - sorry!", 3);
        exit(1);
        }
                                        /* Read in the files expected on the */
                                        /* command line based on -f flag */
    if (filelist)
        for(;optind<argc;optind++) read_data(&data,argv[optind]);

                                        /* Execute the runfile specified with */
                                        /* -r flag */
    if (strlen (run_fname) > 0) 
        {
        strcpy (commands[0].cmnd, "run");
        strcpy (commands[0].arg1, run_fname);
        commands[0].narg = 1;
        if(execute(&data,&commands[0]) < 0) 
            {
            msg("Runfile fails ... Abort!", 3);
            exit(1);
            }
        }
                                        /* Enter the main prompt loop */
                                        /* Only way out is "exit" */
    while(TRUE)
        {
        if (dotype >= 0) printf("aedit %d> ", dotype);
        else printf("aedit> ");
                                        /* Read user input and check it */
        gets(line);
                                        /* Shell escape */
        if(line[0] == '!') system(&line[1]);
        else 
            {
            parse_commands(line,commands,&n);
                                        /* Do all commands on line */
            for(i=0;i<n;i++)
                if(execute(&data,&commands[i]) < 0) break;
            }
        }

    exit (0);
    }
