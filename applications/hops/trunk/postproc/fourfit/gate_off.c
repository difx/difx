/************************************************************************/
/*                                                                      */
/* This routine takes a specification of the frequency channel and ap   */
/* number of a cell in the main time/frequency array, and uses the      */
/* switched-mode specification in the control block to figure out       */
/* whether or not this datum should be included in the fit.  It returns */
/* TRUE if the datum should be skipped, FALSE if not.  Called from      */
/* apply_filter()                                                       */
/*                                                                      */
/*      Inputs:         pass            contains control block          */
/*                      frq             frequency number                */
/*                      ap              ap number                       */
/*                      param           via extern (for time info on    */
/*                                      scan nominal start)             */
/*                                                                      */
/*      Output:         return value    TRUE=skip, FALSE=don't skip     */
/*                                                                      */
/* Created January 10 1994 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "ff_misc_if.h"

int
gate_off (struct type_pass* pass, int frq, int ap)
    {
    int freq_no, mode;
    int period, start_gate, end_gate, duration, acc_period, ap_start, ap_stop;
    int pstart, offset;
    extern struct type_param param;
                                        /* Get appropriate values from */
                                        /* control blk */
                                        /* Do all in milliseconds for integer */
                                        /* comparisons */
    freq_no = fcode(pass->pass_data[frq].freq_code, pass->control.chid);
    mode = pass->control.switched_mode;

    period = (int)(pass->control.switched_period * 1000.0);
    start_gate = (int)(pass->control.gates[freq_no].on_delay * 1000.0 + 0.5);
    duration = (int)(pass->control.gates[freq_no].duration * 1000.0 + 0.5);
    end_gate = start_gate + duration;
    acc_period = (int)(param.acc_period * 1000.0 + 0.5);
                                        /* ap times depend on mode */
    switch (mode)
        {
        case SCAN_START:
                                        /* ap number is relative to param.start */
            offset = (int)((param.start - param.start_nom) * 1000.0 + 0.5);
            ap_start = ((ap - 1) * acc_period + offset) % period;
            ap_stop = ap_start + acc_period;
            break;

        case EACH_MINUTE:
            pstart = (int)(param.start * 1000.0 + 0.5);
            ap_start = ((ap - 1) * acc_period + pstart) % 60000;
            ap_start %= period;
            ap_stop = ap_start + acc_period;
            break;

        default:
            ;
        }

    if ((ap_start >= start_gate) && (ap_stop <= end_gate)) 
        return (FALSE);
    else
        return (TRUE);
    }
