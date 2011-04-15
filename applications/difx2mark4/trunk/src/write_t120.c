// do an Endian flip and write out a type 100 record
//
// initial code                    rjc  2010.3.11

#include <string.h>
#include "difx2mark4.h"

void write_t120 (struct type_120 *pt120,
                 FILE *fout)
    {
    int i;
    long long int nbytes;

    union u_tag
        {
        struct type_120 t120;
        float dummy[2*MAX_VIS+10];  // large enough for MAX_VIS lags
        } u;

    nbytes = (char *) &u.t120.ld.spec[0].re - (char *) &u + 8 * pt120->nlags;
    memcpy (&u.t120, pt120, nbytes);

    u.t120.nlags = short_reverse (pt120->nlags);

    u.t120.index      = int_reverse (pt120->index);
    u.t120.ap         = int_reverse (pt120->ap);
    u.t120.flag       = int_reverse (pt120->flag);
    u.t120.status     = int_reverse (pt120->status);
    u.t120.fr_delay   = int_reverse (pt120->fr_delay);
    u.t120.delay_rate = int_reverse (pt120->delay_rate);

    for (i=0; i<pt120->nlags; i++)
        {
        u.t120.ld.spec[i].re = float_reverse (pt120->ld.spec[i].re);
        u.t120.ld.spec[i].im = float_reverse (pt120->ld.spec[i].im);
        }
                                    // everything flipped, now write the record
    fwrite (&u.t120, (int) nbytes, 1, fout);
    return;
    }
