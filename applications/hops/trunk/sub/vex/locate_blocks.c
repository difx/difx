/************************************************************************/
/*									*/
/* Basic utility to find the statement numbers of all the $blocks	*/
/* within the memory image of the vex file.				*/
/*									*/
/*	Inputs:		stlist (extern)	List of vex statements in file	*/
/*									*/
/*	Output:		blist (extern)	Array of structs with 		*/
/*					requested information		*/
/*			return value	Number of entries in slist	*/
/*									*/
/* Created 21 August 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
locate_blocks (void)
    {
    int i;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nblock, nstmt;
    char block_name[128], junk[128];

    nblock = 0;
    for (i=0; i<nstmt; i++)
	{
					/* Ignore non-$block statements, */
					/* except to record last one in */
					/* previous block */
	if (stlist[i].str[0] != '$') 
	    {
	    if (nblock > 0) blist[nblock-1].end = i;
	    continue;
	    }

	if (sscanf (stlist[i].str, "$%s %s", block_name, junk) != 1)
	    {
	    msg ("Invalid $block statement '%s'", 2, stlist[i].str);
	    return (-1);
	    }
					/* This is valid $BLOCK statement */
					/* and we add it to list */
	if (nblock >= MAXBLOCKS)
	    {
	    msg ("Maximum number of $blocks exceeded - error", 2);
	    return (-1);
	    }
	strncpy (blist[nblock].name, block_name, MAX_NAMESIZE);

	blist[nblock].stno = i;
	nblock++;
	}

    return (nblock);
    }
