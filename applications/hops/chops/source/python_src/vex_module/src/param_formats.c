/************************************************************************/
/*                                                                      */
/* This routine reads the vex parameter format definition file, called  */
/* "pformat.txt", and parses the information therein to fill in the     */
/* memory-allocated extern structure array "param_value".  The contents */
/* of this structure define the legal vex format of all valid vex       */
/* <parameter>=<list_of_values> statements.                             */
/*                                                                      */
/*      Inputs:         None                                            */
/*                                                                      */
/*      Output:         param_value     Filled in                       */
/*                      return value    <0=error, else number of        */
/*                                      entries in param_value          */
/*                                                                      */
/* Created 26 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

struct pval_format *param_value;
int nparam;

int
param_formats (void)
    {
    FILE *fp;
    struct pval_format *pv;
    struct value *val;
    int i, ng, lno, nval, n, error, ngroup, gstart, gstop;
    char mand[51], type[51], units[51], range[51], ver[51], junk[51];
    char line[256], block[256], nval_string[50], many[10];
    char filename[256];
    extern char textdir[];
                                        /* Open the format definition file */
    sprintf (filename, "%s/pformat.txt", textdir);
    if ((fp = fopen (filename, "r")) == NULL)
        {
        msg ("Could not open vex format definition file '%s'", 3, filename);
        return (-1);
        }
                                        /* Start with lots of slots */
    param_value = (struct pval_format *)calloc 
                        (1000, sizeof (struct pval_format));
    if (param_value == NULL)
        {
        msg ("Memory allocation failure in param_formats()", 2);
        return (-1);
        }
                                        /* Read all lines until the end */
    lno = 0;
    param_value[0].nval = -1;
    nparam = 0;
    pv = NULL;
    while (fgets (line, 255, fp) != NULL)
        {
        error = FALSE;
        lno++;
                                        /* Ignore comments */
        if (line[0] == '*') continue;
                                        /* Whitespace ... this is a value */
                                        /* definition line */
        if ((line[0] == ' ') || (line[0] == '\t'))
            {
            if (pv == NULL)
                {
                msg ("Format error in pformat.txt, line %d", 2, lno);
                return (-1);
                }
            if (nval >= pv->nval)
                {
                msg ("Format error in pformat.txt, line %d", 2, lno);
                return (-1);
                }
            val = pv->values + nval;
                                        /* Space-delimited fields */
            n = sscanf (line, "%s %s %s %s %s", mand, type, units, range, junk);
            if ((n > 4) || (n < 3)) error = TRUE;

            if (strcmp (mand, "MAN") == 0)      val->mandatory = MAN;
            else if (strcmp (mand, "OPT") == 0) val->mandatory = OPT;
            else error = TRUE;

            if (strcmp (type, "REAL") == 0)       val->type = VAL_REAL;
            else if (strcmp (type, "CHAR") == 0)  val->type = VAL_CHAR;
            else if (strcmp (type, "INT") == 0)   val->type = VAL_INT;
            else if (strcmp (type, "EPOCH") == 0) val->type = VAL_EPOCH;
            else if (strcmp (type, "RA") == 0)    val->type = VAL_RA;
            else if (strcmp (type, "DEC") == 0)   val->type = VAL_DEC;
            else if (strcmp (type, "LINK") == 0)  val->type = VAL_LINK;
            else error = TRUE;

            if (strcmp (units, "TIME") == 0)             val->units = UNIT_TIME;
            else if (strcmp (units, "FREQ") == 0)        val->units = UNIT_FREQ;
            else if (strcmp (units, "SAMPLERATE") == 0)  val->units = UNIT_SAMPLERATE;
            else if (strcmp (units, "LENGTH") == 0)      val->units = UNIT_LENGTH;
            else if (strcmp (units, "ANGLE") == 0)       val->units = UNIT_ANGLE;
            else if (strcmp (units, "FLUX") == 0)        val->units = UNIT_FLUX;
            else if (strcmp (units, "BITDENSITY") == 0)  val->units = UNIT_BITDENSITY;
            else if (strcmp (units, "ANGLE_TIME") == 0)  val->units = UNIT_ANGLE_TIME;
            else if (strcmp (units, "LENGTH_TIME") == 0) val->units = UNIT_LENGTH_TIME;
            else if (strcmp (units, "NONE") == 0)        val->units = UNIT_NONE;
            else error = TRUE;

            if (n == 4) 
                {
                if (strlen (range) >= 50) error = TRUE;
                else strcpy (val->range, range);
                }
                
            nval++;
            }

        else
            {
                                        /* Did we get all values for previous */
                                        /* parameter? */
            if (nparam > 0)
                if ((nval != pv->nval) && (pv->nval != MAXNVAL)) error = TRUE;
                                        /* We have a group repeat for the last */
                                        /* parameter, they must be filled in */
            if ((nparam > 0) && (pv->nval == MAXNVAL))
                {
                ng = pv->ngroup;
                gstart = nval - ng;
                gstop = nval;
                while (TRUE)
                    {
                    for (i=gstart; i<gstop; i++)
                        {
                        val = pv->values + nval;
                        val->mandatory = OPT;
                        val->type = pv->values[i].type;
                        val->units = pv->values[i].units;
                        strcpy (val->range, pv->values[i].range);
                                        /* Handle implied units of vex */
                                        /* definition */
                        if ((ng == 1) && (val->units != UNIT_NONE)) 
                            val->units += UNIT_OPTIONAL;
                        nval++;
                        if (nval == MAXNVAL) break;
                        }
                    if (nval == MAXNVAL) break;
                    }
                }
                                        /* Check for end of file */
            if (strncmp (line, "END", 3) == 0) break;
                                        /* Three space-delimited fields */
            pv = param_value + nparam;
            n = sscanf (line, "%s %s %s %s %s %s", 
                        pv->param_name, type, block, ver, nval_string, junk);
            if (n != 5) error = TRUE;
                                        /* Handle nval, including special MANY case */
            many[0] = '\0';
            ngroup = 1;
            n = sscanf (nval_string, "%d:%4s:%d", &nval, many, &ngroup);
            if (n >= 2) 
                {
                if (strcmp (many, "MANY") != 0)
                    {
                    msg ("Syntax error in pformat.txt, line %d", 2, lno);
                    return (-1);
                    }
                pv->nval = MAXNVAL;
                pv->ngroup = ngroup;
                }
            else pv->nval = nval;
                                        /* File type (vex flavour) */
            if (strcmp (type, "OVEX") == 0) pv->typever = OVEX;
            else if (strcmp (type, "CVEX") == 0) pv->typever = CVEX;
            else if (strcmp (type, "SVEX") == 0) pv->typever = SVEX;
            else if (strcmp (type, "IVEX") == 0) pv->typever = IVEX;
            else if (strcmp (type, "EVEX") == 0) pv->typever = EVEX;
            else if (strcmp (type, "LVEX") == 0) pv->typever = LVEX;
            else error = TRUE;
                                        /* Version (update this code, and */
                                        /* pformat.txt when revisions occur) */
            if (strcmp (ver, "1.0") == 0) pv->typever |= V_1_0;
            else if (strcmp (ver, "1.5") == 0) pv->typever |= V_1_5;
            else if (strcmp (ver, "ALL") == 0) pv->typever |= V_ALL;
            else error = TRUE;
                                        /* Which $BLOCK is this? */
            if (strlen (block) >= MAX_NAMESIZE)
                {
                msg ("Block name too long, '%s'", 2, block);
                error = TRUE;
                }
            else strcpy (pv->block, block);

            if (pv->nval <= 0) error = TRUE;
                                        /* Allocate space for values */
            if (! error)
                {
                pv->values = (struct value *)calloc (pv->nval, sizeof (struct value));
                if (pv->values == NULL)
                    {
                    msg ("Memory allocation failure in param_formats()", 2);
                    return (-1);
                    }
                }
            nval = 0;
            nparam++;
            }

        if (error)
            {
            msg ("Format error in pformat.txt, line %d", 2, lno);
            return (-1);
            }
        }
                                        /* Trim excess memory */
    param_value = (struct pval_format *)realloc
                        (param_value, nparam * sizeof (struct pval_format));
    if (param_value == NULL)
        {
        msg ("Memory allocation failure in param_formats()", 2);
        return (-1);
        }

    return (nparam);
    }
