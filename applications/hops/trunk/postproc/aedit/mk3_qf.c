/************************************************************************/
/*                                                                      */
/* Temporary fix to make version 5 quality codes mimic old-style mk3    */
/* quality codes                                                        */
/*                                                                      */
/*      Inputs:     datum           type-2 struct                       */
/*                                                                      */
/*      Output:     return value    Mk3-style quality code              */
/*                                                                      */
/* Created March 20 2000 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include "adata.h"

char
mk3_qf (/* datum) */
fringesum *datum)
    {
    char qf;

    if (datum->version >= 5)
        {
        if (datum->errcode != ' ') qf = datum->errcode;
        else qf = datum->quality;
        }
    else qf = datum->quality;
    return (qf);
    }
