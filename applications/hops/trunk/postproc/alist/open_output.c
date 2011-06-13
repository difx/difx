/************************************************************************/
/*									*/
/* Opens the output file, putting in a time stamp, and the standard	*/
/* A-file header line							*/
/*									*/
/*	Inputs:		outfile		Output file name		*/
/*									*/
/*	Output:		fp		opened stream			*/
/*			return value	0 for success			*/
/*									*/
/* Created 5 October 1992 by CJL					*/
/*									*/
/************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>

FILE *
open_output (outfile)
char *outfile;
    {
    struct stat statbuf;
    time_t now;
    static FILE *fp;
    extern int output_version;
					/* Open output file.  If the "outfile" string */
					/* is empty, write to stdout */
    if (strlen (outfile) == 0)
        {
        fp = stdout;
        strcpy (outfile, "stdout");
        }
    else if(stat (outfile, &statbuf) == 0)
        {
        if(statbuf.st_size != 0) 
            if(! confirm("File exists and contains data.  Proceed?")) return(NULL);
        }
    else if(errno != ENOENT)
        {
        msg ("problem %d with output file %s", 2, errno, outfile);
        return (NULL);
        }
    if ((fp = fopen (outfile, "w")) == NULL)
        {
        msg ("Failure opening output file %s", 2, outfile);
        return (NULL);
        }
                                        /* Time stamp the file and put in */
                                        /* the header */
    if (afile_header (output_version, 2, fp) != 0)
	msg ("Error writing header to '%s'", 2, outfile);
	
    return (fp);
    }

