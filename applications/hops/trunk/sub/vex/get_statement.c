/************************************************************************/
/*                                                                      */
/* Given an address within the memory image of a vex file, this         */
/* routine figures out the start and end points of the vex statement    */
/* to which the address belongs, and returns a pointer to a copy of     */
/* the statement.                                                       */
/*                                                                      */
/*      Inputs:         addr            Address within target statement */
/*                                                                      */
/*      Output:         first,last      Addresses of the start and end  */
/*                                      of the target vex statement     */
/*                      return value    Pointer to statement, from      */
/*                                      1st non-whitespace char up to   */
/*                                      but not including final ';',    */
/*                                      null-terminated and overwritten */
/*                                      on next call.  Whitespace is    */
/*                                      compressed.  Overwritten on the */
/*                                      next call.                      */
/*                                                                      */
/* Created 21 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

char *
get_statement (char *addr, 
               char **first, 
               char **last)
    {
    char *ptr;
    int error, found, empty;
    extern char *vexstart, *vexend;
    static char statement[STATEMENT_SIZE];

                                        /* Sanity check */
    if ((addr < vexstart) || (addr > vexend))
        {
        msg ("Invalid address passed to get_statement()", 2);
        return (NULL);
        }
                                        /* Search backwards for ';' */
    ptr = addr;
    *first = NULL;
    while (TRUE)
        {
                                        /* If at start of file, we are done */
        if (ptr < vexstart)
            {
            *first = vexstart;
            break;
            }

        if (*ptr == ';')
            {
                                        /* Ignore comments and quoted strings */
            if (in_comment (ptr)) continue;
            if (in_quote (ptr)) continue;
            *first = ptr+1;
            break;
            }

        ptr--;
        }
                                        /* Search forwards for ';' */
    ptr = addr;
    *last = NULL;
    error = FALSE;
    found = FALSE;
    empty = TRUE;
    while (TRUE)
        {
                                        /* If at end of file, missing ';' */
        switch (*ptr)
            {
            case ' ':
            case '\t':
            case '\n':
            case '\r':
                break;

            case ';':
                                        /* Ignore comments and quoted strings */
                if (in_comment (ptr) || in_quote (ptr)) break;
                *last = ptr;
                found = TRUE;
                break;

            case '\0':
                                        /* Needed terminating ';' */
                if (! empty)
                    {
                    error = TRUE;
                    found = TRUE;
                    break;
                    }
                                        /* valid EOF */
                else
                    return (NULL);
                break;
                                        /* Fragment of uncommented statement */
            default:
                if (in_comment (ptr)) break;
                empty = FALSE;
                break;
            }

        if (found) break;
        ptr++;
        }

    if (error)
        {
        msg ("Last statement in file has missing semicolon", 2);
        return (NULL);
        }
                                        /* Strip comments and excess whitespace */
                                        /* (Omit trailing semicolon) */
    strip_text (*first, (*last)-1, statement);
    if (strlen (statement) == 0) return ("null statement");

    return (statement);
    }
