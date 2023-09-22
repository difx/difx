/************************************************************************/
/*                                                                      */
/* This routine takes a specification of a data point, either in the    */
/* main data array or in the closure arrays of the fqex summary         */
/* structures, and decides whether the "quality" is good, suspect or bad*/
/* This return value is then used to (perhaps) give a visual indication */
/* of the quality in a plot.                                            */
/*                                                                      */
/*      Inputs:         fdatum          element of baseline array       */
/*                      tdatum          element of triangle array       */
/*                                                                      */
/*      Output:         return value    GOOD, SUSPECT or BAD            */
/*                                                                      */
/* Created 21 February 1994 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedit.h"
#include "aedata.h"
#include "pstruct.h"
#include "close_flags.h"

int plot_quality (fringesum *fdatum, trianglesum *tdatum)
    {
    extern struct inputs inp;
    static char *badqf = "ABCEFG";
    static char *suspectqf = "D01234";
    int esqual, quality;

    switch (inp.plotby)
        {
                                        /* Datum from main array */
        case STATION_PLOT:
        case BASELINE_PLOT:
        case ALL_PLOT:
            esqual = esdesp_check (fdatum->esdesp);
                                        /* REMOVE NEXT LINE WHEN ESDESP FIXED IN MK4 */
            if (fdatum->version >= 5) esqual = 0;
            if ((fdatum->snr < 5.5) 
                        || (strchr (badqf, mk3_qf (fdatum)) != NULL)
                        || (esqual == 2) )
                quality = PQ_BAD;
            else if ((fdatum->snr < 6.5) 
                        || (strchr (suspectqf, mk3_qf (fdatum)) != NULL)
                        || (esqual == 1) )
                quality = PQ_SUSPECT;
            else
                quality = PQ_GOOD;
            break;
                                        /* Datum from closure array */
        case TRIANGLE_PLOT:
        case QUAD_PLOT:
            quality = PQ_GOOD;
            if (strchr (suspectqf, tdatum->scan_quality) != NULL)
                quality = PQ_SUSPECT;
            if (strchr (badqf, tdatum->scan_quality) != NULL)
                quality = PQ_BAD;
            break;
        default:
            ;
        }

    return (quality);
    }
