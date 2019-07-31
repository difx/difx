/******************************************************************/
/*                                                                */
/* This little routine initializes the "inp" structure from which */
/* aedit decides how to behave                                    */
/*                                                                */
/* Created 31 March 1989 by CJL                                   */
/*                                                                */
/******************************************************************/
#include <string.h>
#include "aedit.h"
#include "pstruct.h"

int init_inputs(void)
    {
    extern struct inputs inp;

    inp.begin = 0;
    inp.end = 0;
    inp.proc_begin = 0;
    inp.proc_end = 0;
    inp.stations[0] = '\0';
    inp.baselines[0] = '\0';
    inp.triangles[0] = '\0';
    inp.quads[0] = '\0';
    inp.frequencies[0] = '\0';
    inp.polarizations[0] = '\0';
    inp.experiment = 0;
    inp.qcodes[0] = '\0';
    strcpy (inp.type, "0,1,2,3,4");
    inp.snr[0] = 0.0;
    inp.snr[1] = 100000.0;
    inp.bsnr[0] = 0.0;
    inp.bsnr[1] = 1000000.0;
    inp.length = 0;
    inp.fraction = 0;
    inp.nfreq[0] = 1;
    inp.nfreq[1] = MAXFREQ;
    inp.parameter[0] = 0.0;
    inp.parameter[1] = -1.0e30;
    inp.parameter[2] = 1.0e30;
    inp.sources[0] = '\0';
    strcpy (inp.x_axis, "time_tag");
    inp.x_units[0] = '\0';
    inp.xaind = AX_TIMETAG;
    inp.x_aux = -1;
    strcpy (inp.y_axis,"amplitude");
    strcpy (inp.y_units, "e-4");
    inp.yaind = AX_AMPLITUDE;
    inp.y_aux = -1;
    inp.plotby = BASELINE_PLOT;
    inp.refrem = REFERENCE;
    inp.xscale[0] = 0.0;
    inp.xscale[1] = 0.0;
    inp.yscale[0] = 0.0;
    inp.yscale[1] = 0.0;
    inp.grid[0] = 1;
    inp.grid[1] = 1;
    inp.mode = 0;
    strcpy(inp.device,"?");
    inp.dev_auto = 0;
    }

