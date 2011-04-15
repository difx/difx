// do an Endian flip and write out a type 302 record
//
// initial code                    rjc  2010.8.31

#include <string.h>
#include "difx2mark4.h"

void write_t302 (struct type_302 *pt302,
                 FILE *fout)
    {
    int i;
    struct type_302 t302;

    memcpy (&t302, pt302, sizeof (struct type_302));

    t302.interval = short_reverse (pt302->interval);

    for (i=0; i<6; i++)
        t302.phase_spline[i] = double_reverse (pt302->phase_spline[i]);

    fwrite (&t302, sizeof (struct type_302), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
