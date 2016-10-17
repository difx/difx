/****************************************************************/
/* Test whether substring s2 is contained in string s1 		*/
/*								*/
/*	Inputs:		s1	string to search		*/
/*			s2	string to locate within s1	*/
/*								*/
/*	Output		return value  0 = no match		*/
/*				      1 = embedded match	*/
/*				      2 = space-delimited match */
/*								*/
/* Created 5 April 1989 by CJL					*/
/* Added space delimited return code to fix filter design flaw	*/
/* 15 April 1992 CJL						*/
/*								*/
/****************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedit.h"

#define TRUE 1
#define FALSE 0

int smatch(char *s1, char *s2)
    {
    int l1, l2, i, pre, post;

    l1 = strlen(s1);
    l2 = strlen(s2);
    pre = FALSE;
    post = FALSE;
    if(l1 < l2) return(0);
    for(i=0;i<(l1-l2+1);i++) 
	{
	if(s1[i] == s2[0]) 
	    {
	    if(strncmp(&(s1[i]),s2,l2) == 0)
		{
		if (i == 0) pre = TRUE;
		else if (s1[i-1] == ' ') pre = TRUE;
		if ((i+l2) == l1) post = TRUE;
		else if (s1[i+l2] == ' ') post = TRUE;
		if (pre && post) return (2);
		else return (1);
		}
	    }
	}
    return(0);
    }
