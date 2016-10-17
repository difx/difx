/************************************************************************/
/*									*/
/* Allocates memory to hold user-defined parameters.  The mallocs are	*/
/* per-parameter rather than per-Afile line in order to minimise the	*/
/* number of allocated objects (which could otherwise be many thousands)*/
/* At present, clears out all previous data on each call ... will find	*/
/* out by experience if this is too restrictive.			*/
/*									*/
/*	Inputs:		user_param	nparms, npoints and type filled	*/
/*					in, parameter arrays unready	*/
/*									*/
/*	Output:		user_param	parameter arrays duly set up,	*/
/*					ready to receive data		*/
/*									*/
/* Created 11 August 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "usearray.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

int allocate_parms (struct usearray *user_param)
    {
    int i, j;
    static int first = TRUE;
					/* Initialize allocation state */
    if (first)
	{
	for (i=0; i<MAX_PARMS; i++) user_param->allocated[i] = FALSE;
	first = FALSE;
	}
					/* Free up all memory currently */
					/* allocated */
    for (i=0; i<MAX_PARMS; i++)
	if (user_param->allocated[i])
	    {
	    if (user_param->parameter[i] == NULL)
		{
		msg ("Error, inconsistent allocation flags in allocate_parms()", 2);
		return (1);
		}
	    else 
		{
		free (user_param->parameter[i]);
		user_param->allocated[i] = FALSE;
		}
	    }
					/* Now allocate npoint floating */
					/* elements for each user-defined */
					/* parameter */
    for (i=0; i<user_param->nparms; i++)
	{
	user_param->parameter[i] = 
		(double *)malloc (user_param->npoints * sizeof (double));
	if (user_param->parameter[i] == NULL)
	    {
	    msg ("Failure allocating memory for user parameters", 2);
	    return (1);
	    }
	else 
	    {
	    user_param->allocated[i] = TRUE;
					/* Initialize */
	    for (j=0; j<user_param->npoints; j++)
		user_param->parameter[i][j] = 0.0;
	    }
	}
					/* Success */
    return (0);
    }
