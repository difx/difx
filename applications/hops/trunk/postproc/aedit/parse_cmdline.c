/************************************************************************/
/*									*/
/* This handles everything to do with the command line.			*/
/*									*/
/*	Inputs:		argc, argv		command line arguments	*/
/*						in standard form	*/
/*									*/
/*	Output:		xwindow			X-interface? (t/f)	*/
/*			run_fname		name of run file	*/
/*			batch_string		commands in -b mode	*/
/*			filelist		Is a list of data files	*/
/*						present? True or false.	*/
/*									*/
/* Created 2 March 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "aedit.h"

parse_cmdline (argc, argv, xwindow, run_fname, batch_string, filelist)
int argc;
char **argv;
int *xwindow, *filelist;
char *run_fname, *batch_string;
    {
    char c;
    extern char *optarg;
    extern int optind, msglev;
					/* Interpret command line flags */
    *xwindow = *filelist = FALSE;
    run_fname[0] = batch_string[0] = '\0';
    while((c=getopt(argc,argv,"b:r:xfm:")) != -1) 
	{
	if (*filelist)
	    {
	    msg ("Error. -f option must be the LAST option", 3);
	    syntax();
	    return (1);
	    }
	if (strlen (batch_string) > 0)
	    {
	    msg ("Error. -b option must be the ONLY option if given", 3);
	    syntax();
	    return (1);
	    }

	switch(c) 
	    {
	    case 'b':
		strcpy (batch_string, optarg);
		break;

	    case 'r':
		strcpy (run_fname, optarg);
		break;

	    case 'x':
		*xwindow = TRUE;
		break;

	    case 'f':
		*filelist = TRUE;
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


	    case '?':
		msg ("Unrecognized command-line flag '-%c'", 2, c);
		syntax();
		return (1);
		break;
	    }
	}

    return (0);
    }
