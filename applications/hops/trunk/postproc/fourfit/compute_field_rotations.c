/*
 * This function computes the field rotation angles
 * gbc, sep 14, 2021
 *
 * This version uses fields from the param struct which are indexed by
 * ref and rem, but it would be a minor change to add indexing to the
 * station coefficients to allow this to be truly a function of ap.
 *
 * See parallactic-angle-correction.txt for discussion.
 */

#include <math.h>
#include <stdlib.h>
#include "hops_complex.h"

#include "param_struct.h"
#include "pass_struct.h"
#include "apply_funcs.h"

void
compute_field_rotations_fixed(hops_complex cpolvalue[4],
    double *par_angle, double *elevation, int *mount_type)
{
    double elmult[4], radangle;
    int pp;
    elmult[NO_MOUNT_TYPE] =
    elmult[CASSEGRAIN]    = 0.0;
    elmult[NASMYTHLEFT]   = - 1.0;
    elmult[NASMYTHRIGHT]  = + 1.0;
    for (pp = 0; pp < 4; pp++) {
        switch (pp) {
        case POL_LL: radangle =
            (+1) * (par_angle[0] + elmult[mount_type[0]]*elevation[0]) +
            (-1) * (par_angle[1] + elmult[mount_type[1]]*elevation[1]) ;
            break;
        case POL_RR: radangle =
            (-1) * (par_angle[0] + elmult[mount_type[0]]*elevation[0]) +
            (+1) * (par_angle[1] + elmult[mount_type[1]]*elevation[1]) ;
            break;
        case POL_LR: radangle =
            (+1) * (par_angle[0] + elmult[mount_type[0]]*elevation[0]) +
            (+1) * (par_angle[1] + elmult[mount_type[1]]*elevation[1]) ;
            break;
        case POL_RL: radangle =
            (-1) * (par_angle[0] + elmult[mount_type[0]]*elevation[0]) +
            (-1) * (par_angle[1] + elmult[mount_type[1]]*elevation[1]) ;
            break;
        default:
            msg ("this cannot happen", 3);
        }
        cpolvalue[pp] = cexp( - I * radangle );
    }
}
/*
 * eof
 */
