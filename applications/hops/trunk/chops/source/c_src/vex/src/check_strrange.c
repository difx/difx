/************************************************************************/
/*                                                                      */
/* Given a string value and a string defining a valid values, this      */
/* routine returns true or false depending on whether the string is in  */
/* range or not.                                                        */
/*                                                                      */
/*      Inputs:         value           string to be tested             */
/*                      range           Range string                    */
/*                                                                      */
/*      Output:         return value    TRUE or FALSE                   */
/*                                                                      */
/* Created 23 November 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_vex.h"
#include "mk4_util.h"

#define TRUE 1
#define FALSE 0

#define MAXRNG 20

int
check_strrange (char *value,
                char *range)
    {
    int i, len, ncolon, nrng;
    int start, end;
    int cpos[MAXRNG-1];
    char rng_fld[128];
    char *ptr;
                                        /* Count colons */
    len = strlen (range);
    if (len == 0) return (TRUE);
    ptr = range;
    ncolon = 0;
    for (i=0; i<len; i++)
        {
        if (ptr[i] == ':')
            {
            if (ncolon >= (MAXRNG-1))
                {
                msg ("Maximum number of range fields %d exceeded", 2, MAXRNG);
                return (-1);
                }
            cpos[ncolon] = i;
            ncolon++;         
            } 
        }
    nrng = ncolon + 1;
                                        /* Loop over fields */
    for (i=0; i<nrng; i++)
        {
                                        /* Figure out start and end positions */
                                        /* of next colon-delimited value field */
        if (i == 0) start = 0;
        else start = cpos[i-1] + 1;
        if (i == nrng-1) end = len - 1;
        else end = cpos[i] - 1;
                                        /* Copy string out */
        strncpy (rng_fld, ptr+start, end-start+1);
        rng_fld[end-start+1] = '\0';
                                        /* Perform range test */
        if (strcmp (value, rng_fld) == 0) return (TRUE);
        }
                                        /* If we get here, no values matched */
    return (FALSE);
    }

