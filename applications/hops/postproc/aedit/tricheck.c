/************************************************************************/
/*									*/
/* Checks to see if this record matches the triangle specified.  If it	*/
/* does, but the stations are in a different order, the data record is	*/
/* modified to reflect the order of the specified triangle.		*/
/*									*/
/*	Inputs:		datum		Triangle record			*/
/*			triangle	character array			*/
/*									*/
/*	Output:		datum		possibly modified		*/
/*			return value	0=match, 1=no match		*/
/*									*/
/* Created 31 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <string.h>
#include "aedata.h"
#include "aedit.h"

int tricheck (trianglesum *datum, char *triangle)
    {
    char pos[4], root_id[3][7];
    int length[3], extent_no[3], i, j;
    float elevation[3], azimuth[3];
					/* Simple cases first */
					/* Match with correct order */
    if (strcmp (datum->triangle, triangle) == 0) 
	{
	return (0);
	}
					/* Different station(s) */
    if ((strchr (datum->triangle, triangle[0]) == NULL)
		|| (strchr (datum->triangle, triangle[1]) == NULL)
		|| (strchr (datum->triangle, triangle[2]) == NULL)) 
	{
	return (1);
	}
					/* OK, need to figure out */
					/* relative station order */
    for (i=0; i<3; i++) 
	for (j=0; j<3; j++)
	    if (datum->triangle[j] == triangle[i]) 
		{
		pos[j] = '0' + i;
		length[i] = datum->length[j];
		extent_no[i] = datum->extent_no[j];
		strcpy (root_id[i], datum->root_id[j]);
		elevation[i] = datum->elevation[j];
		azimuth[i] = datum->azimuth[j];
		}
    pos[3] = '\0';
					/* Put back rearranged quantities */
    for (i=0; i<3; i++)
	{
datum->triangle[i] = triangle[i];
	datum->length[i] = length[i];
	datum->extent_no[i] = extent_no[i];
	strcpy (datum->root_id[i], root_id[i]);
	datum->elevation[i] = elevation[i];
	datum->azimuth[i] = azimuth[i];
	}
					/* Reverse sign if needed */
    if ((strcmp (pos, "021") == 0)
		|| (strcmp (pos, "102") == 0)
		|| (strcmp (pos, "210") == 0))
	{
	datum->bis_phas = - datum->bis_phas;
	datum->csbdelay = - datum->csbdelay;
	datum->cmbdelay = - datum->cmbdelay;
	datum->cdelay_rate = - datum->cdelay_rate;
	}

    return (0);
    }
	
