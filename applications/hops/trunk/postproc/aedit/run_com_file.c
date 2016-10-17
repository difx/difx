/************************************************************************/
/*									*/
/* This is the routine that implements run files for aedit.  The idea 	*/
/* is simply to read commands from a file instead of from the terminal.	*/
/* Aedit is placed in batch mode before executing the commands in the	*/
/* runfile, thus disabling interactive operations like confirmation,	*/
/* point-and-shoot editing, and zooming.  Errors are not tolerated ..	*/
/* the user is dumped unceremoniously into interactive mode if any	*/
/* command in the run file returns an error code.			*/
/* An asterisk in the first column indicates a comment line.		*/
/*									*/
/*	Inputs:		arg1		filename to read from		*/
/*									*/
/*	Output:		return code	0 for success, <0 for failure	*/
/*									*/
/* Created 17 April 1990 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "aedata.h"
#include "aedit.h"

int run_com_file (esum *data, char *filename)
    {
    extern int batch, rundepth;
    FILE *fp;
    int oldbatch, retcod, i, n;
    struct stat statbuf;
    struct com commands[10];
    char line[257];
						/* Save original batch status */
    oldbatch = batch;
    batch = TRUE;
    if(rundepth >= 10) 
	{
	msg("Excessive nesting, probable loop ... aborting runfiles",2);
	return(-1);
	}
						/* Do checks on input file */
    if(stat(filename,&statbuf) != 0) 
	{
	if(errno == ENOENT) msg ("File '%s' does not exist", 2, filename);
	else msg ("Problem accessing file '%s'",2,filename);
	return (-1);
	}
						/* Open for reading */
    if((fp=fopen(filename,"r")) == NULL) 
	{
        msg("Problem opening '%s'",2,filename);
        return(-1);
        }

    rundepth++;				/* Keep track of nesting */

    confirm ("OFF");

    while(fgets(line,256,fp) != NULL)  	/* Process till EOF */
	{
	retcod = 0;
	n = strlen(line);			/* Strip trailing newline */
	if(line[n-1] == '\n') line[n-1] = '\0';
						/* Allow shell escape */
	if(line[0] == '!') system(&line[1]);
	else if(line[0] != '*') 		/* Comment character */
	    {
	    parse_commands(line,commands,&n);
	    for(i=0;i<n;i++) 		/* Do all commands on line */
		if((retcod=execute (data,&commands[i])) < 0) break;
	    }
	if(retcod != 0) 			/* Error aborts all run files */
	    {
	    msg("Error!  Runfile '%s' aborted.",3,filename);
	    break;
	    }
	}

    fclose(fp);				/* Be tidy */
    batch = oldbatch;			/* Restore batch status */
    rundepth--;				/* Nesting */

    if (rundepth == 0) confirm ("ON");
    return(retcod);
    }
