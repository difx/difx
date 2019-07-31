/************************************************************************/
/*                                                                      */
/* Puts a header line on the fp stream identifying the baseline columns */
/*                                                                      */
/*      Inputs:         fp              open stream                     */
/*                      psarray         contains baseline info          */
/*                                                                      */
/*      Output:         on fp                                           */
/*                                                                      */
/* Created 2 May 1995 by CJL                                            */
/* Made column alignment smarter, new arg, 1 Feb 2001 by CJL            */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "psplot.h"
#include "aedit.h"

void write_pshdr (FILE *fp, struct ps_array *psarray, int len)
    {
    int i;
    char outline[2000];

    sprintf (outline, "Scan ID ");
    for (i=7; i<50; i++) outline[i] = ' ';
    outline[len+3] = '\0';

    for (i=0; i<psarray->nbaselines; i++)
        {
        strcat (outline, psarray->baseline[i].id);
        strcat (outline, " ");
        }

    fprintf (fp, "%s\n", outline);
    }
