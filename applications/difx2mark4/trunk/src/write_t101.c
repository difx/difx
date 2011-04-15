// do an Endian flip and write out a type 101 record
//
// initial code                    rjc  2010.3.11

#include <string.h>
#include "difx2mark4.h"

void write_t101 (struct type_101 *pt101,
                 FILE *fout)
    {
    struct type_101 t101;

    memcpy (&t101, pt101, sizeof (struct type_101));

    t101.nblocks     = short_reverse (pt101->nblocks);
    t101.index       = short_reverse (pt101->index);
    t101.primary     = short_reverse (pt101->primary);
    t101.corr_board  = short_reverse (pt101->corr_board);
    t101.corr_slot   = short_reverse (pt101->corr_slot);
    t101.ref_chan    = short_reverse (pt101->ref_chan);
    t101.rem_chan    = short_reverse (pt101->rem_chan);

    t101.post_mortem = int_reverse (pt101->post_mortem);
    t101.blocks[0] = int_reverse (pt101->blocks[0]);

    fwrite (&t101, sizeof (struct type_101), 1, fout);
    return;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
