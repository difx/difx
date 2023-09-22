/************************************************************************/
/*									*/
/* This routine loops over the source/freq/expt structures, and prints  */
/* out all those pertaining to the source specified in "arg"		*/
/*									*/
/*	Inputs:		arg		Name of source to summarize	*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 5 February 1992 by CJL					*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "summary.h"
#include "aedit.h"

int pr_ssumm(char *arg)
    {
    extern struct datasumm fsumm;
    extern int fscan;
    int i, j, count;

    if(fscan == 0) msg("No type 2 (fringe) data",2);

    printf ("\t\tSUMMARY FOR SOURCE %s\n", arg);
					/* Loop over all freq/expt combs */
    count = 0;
    for (i=0; i<fsumm.nfqex; i++)
	for (j=0; j<fsumm.fqex[i].nsource; j++)
	    {				/* If match, print a summary */
	    if (strcmp(arg, fsumm.fqex[i].slist[j].name) == 0)
		{
		printf("\n\n\t\tEXPERIMENT %d, FREQUENCY %c\n", 
			fsumm.fqex[i].expt_no, fsumm.fqex[i].freq_code);
		printf("\t\t----------------------------\n");
		pr_source (&(fsumm.fqex[i].slist[j]));
		count++;
		}
	    }

    if (count == 0) printf ("\n\nNO DATA FOUND!\n");
    return(0);
    }
