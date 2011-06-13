/************************************************************************/
/*                                                                      */
/* Simple routine to decode a ref statement.  It simply records strings */
/*                                                                      */
/*      Inputs:         stno            Statement number                */
/*                      stlist (extern)                                 */
/*                                                                      */
/*      Output:         ref             filled-in structure             */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 27 October 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
parse_ref (int stno,
           struct ref *ref)
    {
    extern struct statement *stlist;
    char stmt[STATEMENT_SIZE], *ptr;
    char *val[MAXNVAL];
    int nval;
                                        /* Get the sanitized statement */
    if (strncmp (stlist[stno].str, "ref", 3) != 0)
        {
        msg ("parse_ref() was passed a non-ref statement", 2);
        return (-1);
        }
    strcpy (stmt, stlist[stno].str + 3);
                                        /* Find the '=' manually */
    ptr = strchr (stmt, '=');
    if (ptr == NULL)
        {
        msg ("Missing '=' character", 2);
        return (-1);
        }
                                        /* Replace '=' with null */
    *ptr = '\0';
    ptr++;
                                        /* Look for filename:blockname construct */
    nval = get_val_list (stmt, val);
    if (nval == 2)
        {
        msg ("Parser currently does not import externally", 2);
        msg ("referenced files", 2);
        return (-1);
        }
    else if (nval != 1)
        {
        msg ("Parsing error in parse_ref()", 2);
        print_location (stno);
        return (-1);
        }
                                        /* ref to a primitive def */
    if (val[0][0] != '$')
        {
        msg ("Syntax error in ref, missing '$'", 2);
        print_location (stno);
        return (-1);
        }
    strncpy (ref->blockname, val[0] + 1, MAX_NAMESIZE);
                                        /* Find value fields after '=' */
    nval = get_val_list (ptr, ref->args);
    if (nval <= 0) 
        {
        msg ("Parsing error in parse_ref()", 2);
        print_location (stno);
        return (-1);
        }
                                        /* This is the def name */
    strncpy (ref->keyword, ref->args[0], MAX_NAMESIZE);

    ref->nargs = nval;
    ref->stno = stno;

    return (0);
    }
