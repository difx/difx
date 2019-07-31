/************************************************************************/
/*                                                                      */
/* Given a string containing a colon-delimited list of value strings,   */
/* possibly containing quoted character strings with embedded colons    */
/* and whitespace, this routine counts and returns pointers to the      */
/* value fields themselves.                                             */
/*                                                                      */
/*      Inputs:         string          Input string                    */
/*                                                                      */
/*      Output:         vlist           array of pointers to values     */
/*                      return value    Number of values found          */
/*                                                                      */
/* Created 2 September 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
get_val_list (char *ptr,
              char *vlist[])
    {
    int i, nval, len, quote, ncolon, start, end, cpos[MAXNVAL - 1];
    char value[256], *vptr;
    static char val_buffer[MAX_PVALSIZE * MAXNVAL];
                                        /* Skip leading spaces */
    while (*ptr == ' ') ptr++;
                                        /* Locate and count colons */
    len = strlen (ptr);
    quote = 1;
    ncolon = 0;
    for (i=0; i<len; i++)
        {
                                        /* Colons in quotes don't count */
        if (ptr[i] == '\"') quote = -quote;
        if (quote < 0) continue;
        if (ptr[i] == ':')
            {
            if (ncolon >= (MAXNVAL-1))
                {
                msg ("Maximum number of value fields %d exceeded", 2, MAXNVAL);
                return (-1);
                }
            cpos[ncolon] = i;
            ncolon++;         
            } 
        }
    nval = ncolon + 1;
                                        /* Loop over fields */
    vptr = val_buffer;
    for (i=0; i<nval; i++)
        {
                                        /* Figure out start and end positions */
                                        /* of next colon-delimited value field */
        if (i == 0) start = 0;
        else start = cpos[i-1] + 1;
        if (i == nval-1) end = len - 1;
        else end = cpos[i] - 1;
                                        /* Trim at most 1 space from ends */
        if (ptr[start] == ' ') 
            if (start < end) start++;
        if (ptr[end] == ' ') 
            if (start < end) end--;
                                        /* Handle null string case */
        if ((end - start) < 0) value[0] = '\0';
                                        /* This is longer than VEX limit */
        else if ((end - start) > 255)
            {
            msg ("Value string too long", 2);
            return (-1);
            }
                                        /* Copy string out */
        else
            {
            strncpy (value, ptr+start, end-start+1);
            value[end-start+1] = '\0';
            }
                                        /* Process any quoted strings */
        if (process_qstring (value, value) != 0)
            {
            msg ("process_qstring() failure in get_val_list()", 2);
            return (-1);
            }
                                        /* Copy it into val_buffer, and */
                                        /* feed pointer back into vlist */
        strcpy (vptr, value);
        vlist[i] = vptr;
                                        /* Set up for next value string */
        vptr += strlen (value) + 1;
        }
                                        /* Strings successfully extracted, */
                                        /* but contents may be garbage ... */
    return (nval);
    }
