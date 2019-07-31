/************************************************************************/
/*									*/
/* This handles everything to do with the command line.  First, it	*/
/* looks for UNIX-style option flags (which must come first), then it	*/
/* interprets the remaining arguments as a specification of the 	*/
/* correlator data files to summarize, and uses the standard recursive	*/
/* filename extraction in the UTIL library to produce a list of files.	*/
/* The list is terminated by a negative number in the 'order' field	*/
/* of the fstruct structure.						*/
/*									*/
/*	Inputs:		argc, argv		command line arguments	*/
/*						in standard form	*/
/*									*/
/*	Output:		files			fstruct array		*/
/*			outfile			name of output file	*/
/*									*/
/* Created 23 September 1992 by CJL					*/
/* Modified to use updated util library, 20 January 1993 by CJL		*/
/* Added -m option 28 Feb 2011 GBC                                      */
/*									*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "fstruct.h"
#include "mk4_afio.h"
#include "mk4_util.h"

int
parse_cmdline (int argc, char **argv, fstruct **files, char *outfile)
    {
    char c;
    struct stat file_status;
    extern char *optarg;
    extern int optind, output_version, msglev;
					/* Default output filename */
    sprintf (outfile, "alist.out");
					/* Interpret command line flags */
    while((c=getopt(argc,argv,"f:o:m:v:c:")) != -1) 
	{
	switch(c) 
	    {
	    case 'f':
                msg ("Truly obsolete -f flag is ignored", 2);
		break;

	    case 'o':
		strcpy (outfile, optarg);
		break;

	    case 'm':
		msglev = atoi(optarg);
		break;

            case 'c':
                set_afile_com_char(*optarg);
                break;

	    case 'v':
		if (sscanf (optarg, "%d", &output_version) != 1)
		    {
		    msg ("Invalid -v flag argument '%s'", 2, optarg);
		    msg ("Version number remains at %d", 2, output_version);
		    }
		break;

	    case '?':
		msg ("Bad command-line flag", 2);
		return (1);
		break;
	    }
	}

    if (get_filelist (argc-optind, argv+optind, -1, files) != 0)
	{
	msg ("Error extracting files to process from command line args", 2);
	return (1);
	}

    return (0);
    }
