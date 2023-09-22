// do an Endian flip and write out a type 301 record
//
// initial code                    rjc  2010.8.31

#include <string.h>
#include "difx2mark4.h"

void write_t301 (struct type_301 *pt301,
                 FILE *fout)
    {
    int i;
    struct type_301 t301;

    memcpy (&t301, pt301, sizeof (struct type_301));

    t301.interval = short_reverse (pt301->interval);

    for (i=0; i<6; i++)
        t301.delay_spline[i] = double_reverse (pt301->delay_spline[i]);

    fwrite (&t301, sizeof (struct type_301), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
