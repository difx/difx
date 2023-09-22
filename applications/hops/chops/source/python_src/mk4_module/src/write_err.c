/************************************************************************/
/*									*/
/* Trivial routine to write an error message and return an error code	*/
/* in the write file calls.  Also closes the open file.			*/
/*									*/
/*	Inputs:		fp		File open for writing		*/
/*			message		Text of error string		*/
/*									*/
/*	Output:		return value	-1				*/
/*									*/
/* Created 2 January 1997 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "mk4_dfio.h"
#include "mk4_util.h"

int
write_err (FILE *fp,
           char *message)
    {

    fclose (fp);
    msg ("File write error: %s", 2, message);
    return (-1);
    }
