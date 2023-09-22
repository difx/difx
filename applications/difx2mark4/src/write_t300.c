// do an Endian flip and write out a type 300 record
//
// initial code                    rjc  2010.8.31

#include <string.h>
#include "difx2mark4.h"

void write_t300 (struct type_300 *pt300,
                 FILE *fout)
    {
    struct type_300 t300;

    memcpy (&t300, pt300, sizeof (struct type_300));

    t300.model_start.year    = short_reverse (pt300->model_start.year);
    t300.model_start.day     = short_reverse (pt300->model_start.day);
    t300.model_start.hour    = short_reverse (pt300->model_start.hour);
    t300.model_start.minute  = short_reverse (pt300->model_start.minute);
    t300.model_start.second  = float_reverse (pt300->model_start.second);

    t300.model_interval = float_reverse (pt300->model_interval);
    t300.nsplines = short_reverse (pt300->nsplines);

    fwrite (&t300, sizeof (struct type_300), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
