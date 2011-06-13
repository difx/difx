#include "mk4_typedefs.h"
#include "mk4_dfio.h"

void
clear_coord (struct sky_coord *coord)
    {
    coord->ra_hrs = 0;
    coord->ra_mins = 0;
    coord->ra_secs = 0.0;
    coord->dec_degs = 0;
    coord->dec_mins = 0;
    coord->dec_secs = 0.0;
    }
