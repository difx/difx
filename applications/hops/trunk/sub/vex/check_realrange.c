/************************************************************************/
/*                                                                      */
/* Given a real value and a string defining a valid range, this         */
/* routine returns true or false depending on whether the value is in   */
/* range or not.                                                        */
/*                                                                      */
/*      Inputs:         value           real to be tested               */
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
#define OK 1
#define NOT_OK 0
#define LEQ 1
#define LT  2
#define GEQ 3
#define GT  4
#define EQ  5

#define MAXRNG 20

int
check_realrange (double value,
                 char *range)
    {
    int i, len, ncolon, nrng, inequality, operator;
    int start, end;
    int cpos[MAXRNG-1];
    double realval;
    char rng_fld[50], junk[50];
    char *ptr, *ptr2;
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
    inequality = NOT_OK;
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
                                        /* Check for inequalities */
        ptr2 = rng_fld;
        if (rng_fld[0] == '<')
            {
            ptr2++;
            if (rng_fld[1] == '=') 
                {
                ptr2++;
                operator = LEQ;
                }
            else operator = LT;
            }
        else if (rng_fld[0] == '>')
            {
            ptr2++;
            if (rng_fld[1] == '=') 
                {
                ptr2++;
                operator = GEQ;
                }
            else operator = GT;
            }
        else operator = EQ;
                                        /* Get integer value */
        if (sscanf (ptr2, "%lf%s", &realval, junk) != 1)
            {
            msg ("Incorrect range specifier field '%s'", 2, rng_fld);
            return (FALSE);
            }
                                        /* Perform range test */
        switch (operator)
            {
            case LEQ:
                if (value > realval) return (FALSE);
                inequality = OK;
                break;
            case LT:
                if (value >= realval) return (FALSE);
                inequality = OK;
                break;
            case GEQ:
                if (value < realval) return (FALSE);
                inequality = OK;
                break;
            case GT:
                if (value <= realval) return (FALSE);
                inequality = OK;
                break;
            case EQ:
                if (value == realval) return (TRUE);
                break;
            default:
                break;
            }

        }
                                        /* If we get here, it's either because */
                                        /* all inequalities are OK, or because */
                                        /* no explicit values matched */
    if (inequality == OK) return (TRUE);
    else return (FALSE);
    }

