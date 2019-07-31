/************************************************************************/
/*                                                                      */
/* This routine parses a <parameter>=<list_of_values> vex statement.    */
/* It places colon-delimited fields in ascii strings in the output      */
/* structure.  Then it passes these strings, via the structure, to the  */
/* decode_pval() routine, along with the definition of what this        */
/* parameter keyword should have in its value field(s) (the p_def       */
/* structure).  That routine parses each field according to the data    */
/* type it is supposed to contain, and also performs a range check on   */
/* the parsed value.  Various other checks are done, for proper units,  */
/* data type, number of values, and so on.  The end result is passed    */
/* back to the caller in the filled-out p_val structure.                */
/*                                                                      */
/*      Inputs:         statement       Complete vex statement in ascii */
/*                      block           Name of $BLOCK of statement     */
/*                      typever         vex flavor and version # or'ed  */
/*                                      together                        */
/*                                                                      */
/*      Output:         p_val           param_val structure             */
/*                                                                      */
/* Created August 25 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
parse_pval (char *statement,
            char *block,
            int typever,
            struct param_val *p_val)
    {
    char buf[256], stcopy[STATEMENT_SIZE];
    char *ptr;
    int i;
    struct pval_format *p_def;
    extern int nparam;
    extern struct pval_format *param_value;
                                        /* Initialize the p_val struct */
    p_val->name[0] = '\0';
    p_val->nval = 0;
    p_val->dval[0].type = 0;
                                        /* Make a copy that we can chop up */
    strcpy (stcopy, statement);
                                        /* Find the '=' manually */
    ptr = strchr (stcopy, '=');
    if (ptr == NULL)
        {
        msg ("Missing '=' character", 2);
        return (-1);
        }
                                        /* Replace '=' with space */
    *ptr = ' ';

                                        /* Parameter name now space delimited */
    sscanf (stcopy, "%s ", buf);
    strncpy (p_val->name, buf, MAX_NAMESIZE);
                                        /* Retrieve sanitized value fields */
    p_val->nval = get_val_list (ptr, p_val->val);

    if (p_val->nval < 0)
        {
        msg ("Error parsing value list", 2);
        return (-1);
        }
                                        /* What is this parameter, and what */
                                        /* is its format supposed to be? */
    for (i=0; i<nparam; i++)
        {
        p_def = param_value + i;
                                        /* Check that name, block, vex */
                                        /* flavour, and version no. match */
        if (strcmp (p_val->name, p_def->param_name) != 0) continue;
        if (strcmp (p_def->block, block) != 0) continue;
        if (p_def->typever != typever) continue;
        break;
        }
    if (i >= nparam)
        {
        msg ("Unrecognized parameter keyword/version in $%s '%s'", 2, 
                block, p_val->name);
        return (-1);
        }
                                        /* Parse values themselves, and */
                                        /* perform range checking */
    if (decode_pval (p_val, p_def) != 0)
        {
        msg ("Error decoding parameter '%s'", 2, p_val->name);
        return (-1);
        }

    return (0);
    }
