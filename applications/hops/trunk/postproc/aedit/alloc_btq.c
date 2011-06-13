/************************************************************************/
/*									*/
/* This routine manages dynamic memory allocation of arrays of polygon	*/
/* specifiers (baselines, triangles, quads) in the summary structures.	*/
/* Because the pointers are initialized to NULL, and this routine makes	*/
/* sure allocation succeeds, you can reliably determine allocation 	*/
/* status by looking to see if the pointer is NULL or not.		*/
/*									*/
/*	Inputs:		btq		array to be allocated		*/
/*			type		baselines, triangles or quads	*/
/*									*/
/*	Output:		btq		duly allocated			*/
/*			return value    0=good, -1=bad			*/
/*									*/
/* Created August 22 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "summary.h"

int
alloc_btq (btq, allocated, type)
char **btq;
int *allocated, type;
    {
    int size;
					/* Free it if already allocated */
    if (*allocated) free (*btq);
    *allocated = FALSE;
					/* Figure out how long strings will be */
    if (type == NONE) size = 1;
    else if (type == BASELINE) size = 3 * MAXBASE;
    else if (type == TRIANGLE) size = 4 * MAXCLOSE;
    else if (type == QUAD) size = 5 * MAXCLOSE;
    else
	{
	msg ("Invalid type passed to alloc_btq()", 2);
	return (-1);
	}
					/* Allocate it as one long character array */
					/* summary applications will simply calculate */
					/* offsets into it */
    if ((*btq = (char *)malloc (size)) == NULL)
	{
	msg ("Memory allocation failure in alloc_btq()", 2);
	return (-1);
	}
    *allocated = TRUE;
					/* Initialize */
    memset (*btq, '\0', size);

    return (0);
    }
