/************************************************************************/
/*									*/
/* Adump is a program to strip fields from A-file lines by name, and	*/
/* send them directly to the output.  A minor amount of optional data	*/
/* processing is supported.						*/
/*									*/
/*	Inputs:		flags, and fields required in the output	*/
/*									*/
/*	Output:		Stripped file, with a header line		*/
/*									*/
/* Created 2 March 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include "adump.h"
#include "mk4_util.h"
#include "mk4_afio.h"

char progname[6] = "adump";
int msglev = 1;

int
main (int argc, char* argv[])
    {
    int version, type, ret, header, filetype, nskip, first_type;
    char line[512], outline[512];
    struct flist fields[MAXFIELDS];
    FILE *fpin, *fpout;
/*  extern int errno, sys_nerr; */
/*  extern char const *sys_errlist[]; */
					/* Check for option flags, open files as */
					/* needed */
    if ((ret = parse_cmdline (argc, argv, &fpin, 
			&fpout, &header, &filetype, fields)) != 0)
	{
	if (ret > 0)
	    {
	    msg ("Fatal error interpreting command line", 2);
	    syntax("$HeadURL: https://vault.haystack.mit.edu/svn/hops/trunk/postproc/adump/adump.c $");
	    }
	exit(1);
	}
					/* Optionally write out a file header */
    if (header)
	if (write_header (fpout, fields) != 0) exit (1);
					/* Loop over input lines, process them, */
					/* and write the stripped lines back out */
    ret = nskip = 0;
    first_type = -1;
    while (fgets (line, 512, fpin) != NULL)
	{
					/* Skip comment lines */
        if (afile_comment(line)) continue;
					/* What type of line is this? */
					/* We are only interested in late-version */
					/* type 2 or 3 lines in this release of adump */
	aline_id (line, &version, &type);
					/* Skip if this line not of proper type */
	if ((version < 1) || (type != filetype))
	    {
	    nskip++;
	    continue;
	    }
					/* Construct output line (newline appended) */
	if (type == 2) ret = strip_bline (line, fields, outline);
	else if (type == 3) ret = strip_tline (line, fields, outline);
	if (ret != 0)
	    {
	    if (fpout != stdout) msg ("Error creating output string", 3);
	    break;
	    }
					/* and write it to the output */
	if (fputs (outline, fpout) == EOF)
	    {
	    if (fpout != stdout) msg ("Error writing output line", 3);
		/* {
		if (errno <= sys_nerr)
		    msg ("Error writing output line: %s", 3, sys_errlist[errno]);
		else  msg ("Error writing output line", 3);
		} */
	    ret = 1;
	    break;
	    }
	}

    if ((nskip > 0) && (fpout != stdout))
	msg ("Skipped %d records which were not type %d", 2, nskip, filetype);

    exit (ret);
    }
