/*
 * This is a placeholder for a routine that will eventually apply a complex
 * bandpass to every visibility.  The correction itself must be determined
 * separately and stored by station in a set of files; this needs to be loaded
 * at the first ap for any baseline and then appplied to all aps and all sbs.
 */

#include "pass_struct.h"
#include "param_struct.h"
#include "apply_funcs.h"
#include "ff_misc_if.h"

void apply_cmplxbp(int sb, struct freq_corel *fdata,
    hops_complex *xp_spectrum, int npts, struct type_pass *pass)
{
    msg ("no work", -3);
    return;
}

/*
 * eof
 */
