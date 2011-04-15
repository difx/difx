// do an Endian flip and write out a type 100 record
//
// initial code                    rjc  2010.3.11

#include <string.h>
#include "difx2mark4.h"

void write_t100 (struct type_100 *pt100,
                 FILE *fout)
    {
    struct type_100 t100;

    memcpy (&t100, pt100, sizeof (struct type_100));

    t100.procdate.year   = short_reverse (pt100->procdate.year);
    t100.procdate.day    = short_reverse (pt100->procdate.day);
    t100.procdate.hour   = short_reverse (pt100->procdate.hour);
    t100.procdate.minute = short_reverse (pt100->procdate.minute);
    t100.procdate.second = float_reverse (pt100->procdate.second);

    t100.pct_done = float_reverse (pt100->pct_done);

    t100.start.year   = short_reverse (pt100->start.year);
    t100.start.day    = short_reverse (pt100->start.day);
    t100.start.hour   = short_reverse (pt100->start.hour);
    t100.start.minute = short_reverse (pt100->start.minute);
    t100.start.second = float_reverse (pt100->start.second);

    t100.stop.year   = short_reverse (pt100->stop.year);
    t100.stop.day    = short_reverse (pt100->stop.day);
    t100.stop.hour   = short_reverse (pt100->stop.hour);
    t100.stop.minute = short_reverse (pt100->stop.minute);

    t100.stop.second = float_reverse (pt100->stop.second);

    t100.ndrec   = int_reverse (pt100->ndrec);
    t100.nindex  = int_reverse (pt100->nindex);
    t100.nlags   = short_reverse (pt100->nlags);
    t100.nblocks = short_reverse (pt100->nblocks);

    fwrite (&t100, sizeof (struct type_100), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
