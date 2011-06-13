/************************************************************************/
/*									*/
/* This handles everything to do with the command line.  It connects	*/
/* up input and output streams, then parses the user-specified fields,	*/
/* placing them in the fields structure array for later use.		*/
/*									*/
/*	Inputs:		argc, argv		command line arguments	*/
/*						in standard form	*/
/*									*/
/*	Output:		fpin			input stream		*/
/*			fpout			output stream		*/
/*			header			Do we write header?	*/
/*			filetype		2 or 3 (baseln/trngl)	*/
/*			fields			field specifications	*/
/*			return value		OK=0, bad=1, bad but	*/
/*						don't print syntax = -1	*/
/*									*/
/* Created 2 March 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "adump.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
parse_cmdline (int argc,
               char **argv,
               FILE **fpin,
               FILE **fpout,
               int *header,
               int *filetype,
               struct flist fields[])
    {
    char c;
    char infile[256], outfile[256], line[512];
    int i, nfields, version, type, printfields;
    extern struct flist type2_fields[], type3_fields[];
    extern char *optarg;
					/* Defaults */
    *fpin = stdin;
    *fpout = stdout;
    *header = TRUE;
    *filetype = 0;
    printfields = FALSE;
					/* Interpret command line flags */
    while ((c=getopt(argc,argv,"f:hi:o:t:")) != -1) 
	{
	switch(c) 
	    {
	    case 'f':
		if (sscanf (optarg, "%d", &type) != 1)
		    {
		    msg ("Bad -f option '%s'", 3, optarg);
		    return (1);
		    }
		if ((type != 2) && (type != 3))
		    {
		    msg ("Invalid -f argument, use 2 or 3", 3);
		    return (1);
		    }
		*filetype = type;
		printfields = TRUE;
		
	    case 'h':
		*header = FALSE;
		break;

	    case 'i':
		strcpy (infile, optarg);
		if ((*fpin = fopen (infile, "r")) == NULL)
		    {
		    msg ("Could not open input file '%s'", 3, infile);
		    return (-1);
		    }
		break;

	    case 'o':
		strcpy (outfile, optarg);
		if ((*fpout = fopen (outfile, "w")) == NULL)
		    {
		    msg ("Could not open output file '%s'", 3, outfile);
		    return (-1);
		    }
		break;

	    case 't':
		if (sscanf (optarg, "%d", &type) != 1)
		    {
		    msg ("Bad -t option '%s'", 3, optarg);
		    return (1);
		    }
		if ((type != 2) && (type != 3))
		    {
		    msg ("Invalid -t argument, use 2 or 3", 3);
		    return (1);
		    }
		*filetype = type;
		break;

	    case '?':
		msg ("Bad command-line flag ", 3);
		return (1);
		break;
	    }
	}
					/* No filetype specified.  If this is */
					/* normal file, get type by reading it, */
					/* and then seek back to the start.  If */
					/* reading stdin, this is an error */
    if (*filetype == 0)
	{
	if (*fpin == stdin)
	    {
	    msg ("You must specify a data type (2 or 3) when using adump", 3);
	    msg ("in a pipe (i.e. reading from stdin).  Use the -t option", 3);
	    return (1);
	    }
	while (fgets (line, 250, *fpin) != NULL)
	    {
                                        /* Skip comment lines */
	    if (line[0] == '*') continue;
	    aline_id (line, &version, &type);
					/* Found valid line, rewind the file */
	    if ((type == 2) || (type == 3))
		{
		*filetype = type;
		rewind (*fpin);
		break;
		}
	    }
					/* Check that it worked */
	if (*filetype == 0)
	    {
	    msg ("No valid type 2 or type 3 data in input file", 3);
	    return (-1);
	    }
	}
					/* Find the user-specified fields */
    if (*filetype == 2)
	nfields = parse_bfields (argc, argv, fields);
    else if (*filetype == 3)
	nfields = parse_tfields (argc, argv, fields);
					/* User needs list of fields */
    if ((nfields == 0) || printfields)
	{
	if (! printfields) msg ("No valid fields specified", 3);
	msg ("Below is a list of currently supported type-%d fields:", 3, *filetype);
	i = 0;
	msg ("", 3);
	if (*filetype == 2)
	    while (type2_fields[i].id > 0) msg ("\t\t%s", 3, type2_fields[i++].name);
	else if (*filetype == 3)
	    while (type3_fields[i].id > 0) msg ("\t\t%s", 3, type3_fields[i++].name);
	msg ("", 3);
	if (printfields)
	    {
	    syntax ();
	    return (-1);
	    }
	else return (1);
	}
    else if (nfields < 0) return (1);

    return (0);
    }
