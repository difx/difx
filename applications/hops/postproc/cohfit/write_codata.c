/************************************************************************/
/*                                                                      */
/* Writes out A-file lines for all baselines of a scan, as contained    */
/* in the codata structure.  These A-file lines have the coherence      */
/* time fields filled in, suitable for input to the fringe search       */
/* package.                                                             */
/*                                                                      */
/*      Inputs:         codata          representing 1 scan             */
/*                      fpout           open output file                */
/*      Output:         A-file lines    written to fpout                */
/*                      return value    Number of lines written         */
/*                                                                      */
/* This routine marks data with a 'K' flag indicating a bogus duration. */
/*                                                                      */
/* Created October 6 1995 by CJL                                        */
/* Hacked up April 26 2026 by GBC                                       */
/************************************************************************/
#include <stdio.h>
#include "mk4_afio.h"
#include "mk4_util.h"
#include "cohfit.h"

int write_codata (cosumary *codata, FILE *fpout)
    {
    int i;

    afile_header(codata[0].datum->version, 2, fpout);

    i = 0;
    while (codata[i].datum != NULL)
        {
        codata[i].datum->datatype[0] = 'K';
        write_fsumm (codata[i].datum, fpout);
        i++;
        }
    return (i);
    }
/*
 * eof vim:nospell
 */
