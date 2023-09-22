// do an Endian flip and write out a type 303 record
//
// initial code                    rjc  2010.8.31

#include <string.h>
#include "difx2mark4.h"

void write_t303 (struct type_303 *pt303,
                 FILE *fout)
    {
    int i;
    struct type_303 t303;

    memcpy (&t303, pt303, sizeof (struct type_303));

    t303.interval = short_reverse (pt303->interval);

    for (i=0; i<6; i++)
        {
        t303.azimuth[i]           = double_reverse (pt303->azimuth[i]);
        t303.elevation[i]         = double_reverse (pt303->elevation[i]);
        t303.parallactic_angle[i] = double_reverse (pt303->parallactic_angle[i]);
        t303.u[i]                 = double_reverse (pt303->u[i]);
        t303.v[i]                 = double_reverse (pt303->v[i]);
        t303.w[i]                 = double_reverse (pt303->w[i]);
        }

    fwrite (&t303, sizeof (struct type_303), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
