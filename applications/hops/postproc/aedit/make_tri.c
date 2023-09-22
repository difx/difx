/************************************************************************/
/*									*/
/* This routine takes a station list, and figures out a list of 	*/
/* independent closure triangles, returned as a contiguous series of	*/
/* NULL-terminated 3-character strings.  The number of triangles	*/
/* returned is placed in the ntri output argument.			*/
/*									*/
/* Inputs	stations		null-terminated station list	*/
/*									*/
/* Outputs	ntri			Number of triangles returned	*/
/*		return value		Pointer to triangle strings	*/
/*									*/
/* Created 6 February 1992 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "summary.h"
#include "aedit.h"

char *make_tri (char *stations, int *ntri)
    {
    int nstat, i, j, k, nt;
    char *trilist;
    char blines[MAXSTEXP * (MAXSTEXP-1) * 2];
    char triangle[4];
                                        /* Check for sensible inputs */  
    *ntri = 0;                                      
    nstat = strlen (stations);
    if (nstat > MAXSTEXP) 
        {
        msg ("error, too many stations in make_tri()",2);
        return (NULL);
        }
    else if (nstat < 3) return (NULL);
                                        /* Now create the space for triangles */
    *ntri = ((nstat-1) * (nstat-2)) / 2;
    trilist = (char *) malloc ((*ntri) * 4);
    if (trilist == NULL)
        {
        msg ("malloc failure for triangle list in make_tri()",2);
        *ntri = 0;
        return (NULL);
        }
    for (i=0; i<(*ntri)*4; i++) trilist[i] = '\0';
        
    nt = 0;
    triangle[3] = '\0';                 /* Generate all possible triangles */       
    for (i=0; i<nstat-2; i++)           /* numbering n(n-1)(n-2)/6  */
        {
        for (j=i+1; j<nstat-1; j++)
            {
            for (k=j+1; k<nstat; k++)
                {
                triangle[0] = stations[i];
                triangle[1] = stations[j];
                triangle[2] = stations[k];      /* Check for tri. independence */
                if (newbase (blines, triangle)) /* should only be (n-1)(n-2)/2 */
                    {
                    if (nt >= *ntri)
                        {
                        msg ("Bug in make_tri() ... getting too many triangles",2);
                        free (trilist);
                        *ntri = 0;
                        return (NULL);
                        }
                    else 
                        {
                        strcpy (trilist + (nt*4), triangle);
                        nt++;
                        }
                    }
                }       /* k loop */
            }           /* j loop */
        }               /* i loop */
                                        /* Double check for screwups */   
    if (nt != *ntri)
        {
        msg ("Error in make_tri(), got %d triangles, should get %d",2,nt,*ntri);
        free (trilist);
        *ntri = 0;
        return (NULL);
        }
                                        /* OK, this should be the list */
    return (trilist);
    }
    
    
    
int newbase (char *baselist, char *triangle)
    {
    char base1[3], base2[3], base3[3], *base;
    int nbase, ret, match1, match2, match3;
    
    ret = FALSE;			/* No new baselines yet! */
    match1 = match2 = match3 = FALSE;

    base1[0] = triangle[0]; base1[1] = triangle[1];	/* Construct baselines */
    base2[0] = triangle[1]; base2[1] = triangle[2];	/* in triangle */
    base3[0] = triangle[2]; base3[1] = triangle[0];
    base1[2] = base2[2] = base3[2] = '\0';

    nbase = 0;				/* Loop over all current baselines */
    while (baselist[nbase*3])		/* looking for matches, even if reversed */
        {
        base = baselist + (nbase*3);
        if ((base[0] == base1[0]) && (base[1] == base1[1])) match1 = TRUE;                   
        if ((base[0] == base1[1]) && (base[1] == base1[0])) match1 = TRUE;
        if ((base[0] == base2[0]) && (base[1] == base2[1])) match2 = TRUE;
        if ((base[0] == base2[1]) && (base[1] == base2[0])) match2 = TRUE;
        if ((base[0] == base3[0]) && (base[1] == base3[1])) match3 = TRUE;
        if ((base[0] == base3[1]) && (base[1] == base3[0])) match3 = TRUE;
	nbase++;
	}

    if (! match1)			/* Act on what we found.  If there were */
	{				/* any new baselines, this triangle is */
	strcpy (baselist + (nbase*3), base1);	/* independent, return TRUE */
	nbase++;
	ret = TRUE;
	}
    if (! match2)
	{
	strcpy (baselist + (nbase*3), base2);
	nbase++;
	ret = TRUE;
	}
    if (! match3)
	{
	strcpy (baselist + (nbase*3), base3);
	ret = TRUE;
	}

    return (ret);
    }
