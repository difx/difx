#include <string.h>

    
    int             sindex (char *s, char *t);

/*++*********************************************************************/
int             iword (instr, delim, nword, outstr)
    char           *instr;	       /* input string */
    char           *delim;	       /* word delimiter */
    int             nword;	       /* retrieve nth word in string */
    char           *outstr;	       /* output word pointer */
/**********************************************************************/
/*
%% retrieves the nth word in a string, delimiters user specified
   The iword function mimics one of the more useful aspects of the
string manipulation language "awk". That is, it will retrieve the 
n_th word in a string where the words are separated by delimiters 
specified by the user. The delimiter may be " ".
   Iword is designed to skip over multiple consequetive occurences
of the delimiter symbol. It is fairly common to have several blanks
in a row between words in a text string.
---
LANGUAGE:C
ENVIRONMENT: Sun Unix
++$ AUDIT TRAIL
1.0  89nov07  J. Benson Initial submission
--$
-*/
{
    char           *cp;
    int             i, j, k;

    cp = instr;
    *outstr = '\0';
/*  space past consequetive beginning blanks */
    while(strncmp(cp," ",1)==0) cp++;
    for (k = 1; k < nword; k++) {
	i = sindex (cp, delim);
/*  if the delimiter is not found, sindex returns -1. */
        if (i == -1) return(0);

	for (j = 0; j < i + 1 && *cp != '\0'; j++)
	    cp++;
/*  advance through consequetive delimiters (usually blanks) */
        while(strncmp(cp,delim,1)==0) cp++;
    }

    if (*cp == '\0')
	return (0);

    while (*cp != '\0' && *cp != *delim)
	*outstr++ = *cp++;

    *outstr++ = '\0';
    return (1);
}
