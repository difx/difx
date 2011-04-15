// do an Endian flip and write out a type 309 record
//
// initial code                    rjc  2010.8.31

#include <string.h>
#include "difx2mark4.h"

void write_t309 (struct type_309 *pt309,
                 FILE *fout)
    {
    int i,
        j;
    struct type_309 t309;

    memcpy (&t309, pt309, sizeof (struct type_309));

    t309.su          = int_reverse (pt309->su);
    t309.ntones      = int_reverse (pt309->ntones);
    t309.rot         = double_reverse (pt309->rot);
    t309.acc_period  = double_reverse (pt309->acc_period);

    for (i=0; i<64; i++)
        {
        t309.chan[i].freq = double_reverse (pt309->chan[i].freq);
        for (j=0; j<64; j++)
            {
            t309.chan[i].acc[j][0] = int_reverse (t309.chan[i].acc[j][0]);
            t309.chan[i].acc[j][1] = int_reverse (t309.chan[i].acc[j][1]);
            }
        }
    fwrite (&t309, sizeof (struct type_309), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
