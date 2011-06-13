/************************************************************************/
/*                                                                      */
/* Given a list of value fields in unparsed ascii, and a parameter      */
/* statement definition, this routine parses the text into the needed   */
/* type of variable, and performs a variety of syntax and range checks  */
/*                                                                      */
/*      Inputs:         p_val           param_val struct with ascii     */
/*                                      fields only filled in           */
/*                      p_def           pval_format struct for this     */
/*                                      statement                       */
/*                                                                      */
/*      Output:         p_val           data_value structure element    */
/*                                      filled in                       */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 3 September 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
decode_pval (struct param_val *p_val,
             struct pval_format *p_def)
    {
    int i, j, len, min_nval, max_nval, n, type, units, blank, error;
    char *range, instring[256], valstring[256], unitstring[256], junk[256];
    double factor, previous_factor;
    struct data_value *dval;
                                        /* Count mandatory value fields */
    min_nval = 0;
    for (i=0; i<p_def->nval; i++)
        if (p_def->values[i].mandatory) min_nval = i;
    max_nval = p_def->nval;
                                        /* Did we find a valid number? */
    if (p_val->nval < min_nval)
        {
        msg ("Too few value fields present (%d found, %d min. allowed)", 2,
                                p_val->nval, min_nval);
        return (-1);
        }
    else if (p_val->nval > max_nval)
        {
        msg ("Too many value fields present (%d found, %d max. allowed)", 2,
                                p_val->nval, max_nval);
        return (-1);
        }
                                        /* Loop over all actual fields */
    previous_factor = 1.0;
    for (i=0; i<p_val->nval; i++)
        {
                                        /* Get convenient pointers */
        dval = p_val->dval + i;
        strcpy (instring, p_val->val[i]);
                                        /* Is it all blank? Is that OK?  */
        blank = FALSE;
        len = strlen (instring);
        for (j=0; j<len; j++) if (instring[j] != ' ') break;
        if (j == len)
            {
            if (p_def->values[i].mandatory == MAN) 
                {
                msg ("Blank field for mandatory value - error", 2);
                return (-1);
                }
            else blank = TRUE;
            }
                                        /* What are we supposed to find? */
        type = p_def->values[i].type;
        units = p_def->values[i].units;
        range = p_def->values[i].range;
                                        /* No units, factor stays at 1.0 */
        factor = 1.0;
        if (blank) ;
        else if (units == UNIT_NONE) strcpy (valstring, instring);
                                        /* Separate the value from the units */
        else 
            {
            n = sscanf (instring, "%s %s %s", valstring, unitstring, junk);
            if (n != 2)
                {
                                        /* Special case of repeated fields */
                if (units > UNIT_OPTIONAL) factor = previous_factor;
                else
                    {
                    msg ("Incorrect number of fields, arg %d of '%s' statement",
                        2, i, p_val->name);
                    return (-1);
                    }
                }
                                        /* They put the units in anyway */
            else if (units > UNIT_OPTIONAL) units -= UNIT_OPTIONAL;
                                        /* At this point, if implied units */
                                        /* are present, units > UNIT_OPTIONAL */
                                        /* and factor has been pre-set */
                                        /* Decode the units, and derive the */
                                        /* multiplicative factor to convert */
                                        /* to MKS units */
            if (units < UNIT_OPTIONAL)
                if (parse_units (unitstring, units, &factor) != 0)
                    {
                    msg ("Invalid units specifier '%s'", 2, unitstring);
                    return (-1);
                    }
            previous_factor = factor;
            }
                                        /* Parse the value string according */
                                        /* to the data type, and apply any */
                                        /* factor needed */
        error = FALSE;
        switch (type)
            {
            case VAL_REAL:
                if (blank)
                    {
                    dval->data.realval = 0.0;
                    break;
                    }
                n = sscanf (valstring, "%lf %s", &(dval->data.realval), junk);
                dval->data.realval *= factor;
                if (! check_realrange (dval->data.realval, range)) 
                    {
                    msg ("Real value range check failed", 2);
                    error = TRUE;
                    }
                if (n != 1) error = TRUE;
                break;
            case VAL_CHAR:
                if (blank)
                    {
                    dval->data.strval[0] = '\0';
                    break;
                    }
                if (strlen (valstring) >= MAX_PVALSIZE)
                    {
                    msg ("Parameter value string too long", 2);
                    error = TRUE;
                    }
                else strcpy (dval->data.strval, valstring);
                if (! check_strrange (dval->data.strval, range))
                    {
                    msg ("String value range check failed", 2);
                    error = TRUE;
                    }
                break;
            case VAL_INT:
                if (blank)
                    {
                    dval->data.intval = 0;
                    break;
                    }
                n = sscanf (valstring, "%d %s", &(dval->data.intval), junk);
                dval->data.intval *= factor;
                if (! check_intrange (dval->data.intval, range))
                    {
                    msg ("Integer value range check failed", 2);
                    error = TRUE;
                    }
                if (n != 1) error = TRUE;
                break;
            case VAL_EPOCH:
                if (blank)
                    {
                    clear_date (&(dval->data.epochval));
                    break;
                    }
                if (parse_date (valstring, &(dval->data.epochval)) != 0)
                    error = TRUE;
                break;
            case VAL_RA:
                if (blank)
                    {
                    dval->data.raval.ra_hrs = 0;
                    dval->data.raval.ra_mins = 0;
                    dval->data.raval.ra_secs = 0.0;
                    break;
                    }
                if (parse_ra (valstring, &(dval->data.raval)) != 0)
                    error = TRUE;
                break;
            case VAL_DEC:
                if (blank)
                    {
                    dval->data.decval.dec_degs = 0;
                    dval->data.decval.dec_mins = 0;
                    dval->data.decval.dec_secs = 0.0;
                    break;
                    }
                if (parse_dec (valstring, &(dval->data.decval)) != 0)
                    error = TRUE;
                break;
            case VAL_LINK:
                if (blank)
                    {
                    dval->data.linkval[0] = '\0';
                    break;
                    }
                if (strlen (valstring) >= MAX_NAMESIZE)
                    {
                    msg ("Parameter value string too long", 2);
                    error = TRUE;
                    }
                else if (valstring[0] != '&')
                    {
                    msg ("Link values must start with '&'", 2);
                    error = TRUE;
                    }
                else strcpy (dval->data.linkval, valstring+1);
                break;
            default:
                break;
            }
        if (error)
            {
            msg ("Failure decoding value string '%s'", 2, valstring);
            return (-1);
            }
        }

    return (0);
    }
