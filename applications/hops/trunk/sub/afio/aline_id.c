/************************************************************************/
/*									*/
/* This routine takes what is supposed to be a line from an A-file, and	*/
/* informs the caller what type and version of A-file line this is.	*/
/*									*/
/*	Inputs:		line		The A-file line to be analyzed	*/
/*									*/
/*	Output:		version		A-file format version number	*/
/*			type		0,1,2,3 or 4, -1=not A-format	*/
/*									*/
/* Created 2 March 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "adata.h"
#include "mk4_afio.h"
#include "mk4_util.h"

void aline_id (char *line, int *version, int *type)
    {
    int n, ver, typ;
					/* Handle case of old format */
    if (isdigit (line[0])) 
	{
	if ((n = sscanf (line, "%d %*s %d", &ver, &typ)) != 2) typ = -1;
	}
    else 
	{
	if (sscanf (line, "%*s %d", &typ) != 1) typ = -1;
	ver = 1;
	}
					/* Very old formats */
    if (typ >= 50) typ -= 50;
					/* Check range of values */
    if ((ver < 1) || (ver > CURRENT_VERSION)) typ = -1;
    if ((typ < 0) || (typ > 4)) typ = -1;

    *version = ver;
    *type = typ;
    }
