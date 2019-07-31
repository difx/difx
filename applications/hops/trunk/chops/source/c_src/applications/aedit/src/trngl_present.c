/************************************************************************/
/*									*/
/* A simple routine to discover if a triangle is represented in a list	*/
/* of baselines.  The baselines come with data array indices attached,	*/
/* and a successful search will be accompanied with the three indices	*/
/* that make up the triangle.  These indices are direct indices into	*/
/* the data array, except that if the baseline is reversed, the index	*/
/* has 1000000 (10^6) added to it.					*/
/*									*/
/*	Inputs:		triangle	The triangle we wish to test	*/
/*			blist		The list of baselines to search */
/*			n		Number of baselines in blist 	*/
/*									*/
/*	Output:		indices		Data array indices of baslines	*/
/*					which comprise this triangle,	*/
/*					plus reversal flag (see above)	*/
/*			returns TRUE if triangle found, FALSE if not	*/
/*									*/
/* Created 12 February 1992 by CJL					*/
/*									*/
/************************************************************************/
#define TRUE 1
#define FALSE 0

#include "aedit.h"

int trngl_present (char triangle[4], struct ibaselist *blist,
    int n, int indices[3])
    {
    char base1[3], base2[3], base3[3];
    int i;

					/* Initialize (zero no good, valid index) */
    indices[0] = indices[1] = indices[2] = 2000000;

    base1[0] = triangle[0]; base1[1] = triangle[1];     /* Construct baselines */
    base2[0] = triangle[1]; base2[1] = triangle[2];     /* in triangle, AB, BC, CA */
    base3[0] = triangle[2]; base3[1] = triangle[0];
    base1[2] = base2[2] = base3[2] = '\0';

    for (i=0; i<n; i++)			/* Check all b'lines for those in triangle */
	{				/* reversed b'line, index += 1000000 */
	if ((blist[i].baseline[0] == base1[0]) 
		&& (blist[i].baseline[1] == base1[1])) indices[0] = blist[i].index;
	else if ((blist[i].baseline[1] == base1[0]) 
		&& (blist[i].baseline[0] == base1[1])) 
		indices[0] = blist[i].index + 1000000;

	if ((blist[i].baseline[0] == base2[0]) 
		&& (blist[i].baseline[1] == base2[1])) indices[1] = blist[i].index;
	else if ((blist[i].baseline[1] == base2[0]) 
		&& (blist[i].baseline[0] == base2[1]))
		indices[1] = blist[i].index + 1000000;

	if ((blist[i].baseline[0] == base3[0]) 
		&& (blist[i].baseline[1] == base3[1])) indices[2] = blist[i].index;
	else if ((blist[i].baseline[1] == base3[0]) 
		&& (blist[i].baseline[0] == base3[1]))
		indices[2] = blist[i].index + 1000000;
	}
					/* All indices modified ==> cphase present */
    if ((indices[0] < 2000000) && (indices[1] < 2000000) && (indices[2] < 2000000))
	return (TRUE);
    else
	return (FALSE);
    }
