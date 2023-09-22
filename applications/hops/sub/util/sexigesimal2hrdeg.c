/*
 * Convert from sexigesimal sky coordinates
 * to slightly more useful decimal versions.
 *
 * gbc jul 20 2016
 */

#include "mk4_util.h"

void   sexigesimal2hrdeg(const struct sky_coord *c, float *ra, float *dec)
{
    double rah, dcg, sgn;

    rah = (double)c->ra_hrs;
    rah += (double)c->ra_mins / 60.0;
    rah += (double)c->ra_secs / 3600.0;

    /* peculiar:  convention is in vex:parse_dec.c */
    if (c->dec_degs == 0) {
        sgn = 1.0;
    } else {
        sgn = (c->dec_degs > 0) ? 1.0 : -1.0;
    }

    dcg = (double)c->dec_degs;
    dcg += sgn * (double)c->dec_mins / 60.0;
    dcg += sgn * (double)c->dec_secs / 3600.0;

    *ra = (float)rah;
    *dec = (float)dcg;
}

/*
 * eof
 */
