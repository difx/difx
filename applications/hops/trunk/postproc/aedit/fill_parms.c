/************************************************************************/
/*                                                                      */
/* This routine takes a filled fringe structure and the usearray        */
/* structure, and fills in the usearray parameter slots for the current */
/* scan with the requested information from the fringe structure.       */
/*                                                                      */
/*      Inputs:         fringe          filled in by read_fringe_quick  */
/*                      user_param      specifies parameters to be      */
/*                                      extracted                       */
/*                                                                      */
/*      Output:         user_param      parameters filled in for this   */
/*                                      scan                            */
/*                                                                      */
/* Created 11 August 1993 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include "summary.h"
#include "aedata.h"
#include "data.h"
#include "usearray.h"

int
fill_parms (fringe, data, user_param)
struct data_fringe *fringe;
fringesum *data;
struct usearray *user_param;
    {
    double value;
    int i, day, index, channels, error, temp, utc_int;
    struct datec tempdate;
    struct udat *type;
    extern struct datasumm fsumm;

    error = FALSE;
                                        /* Loop over all parameters for this */
                                        /* fringe file */
    channels = fringe->t4000.channels;
    for (i=0; i<user_param->nparms; i++)
        {
        type = user_param->type + i;
                                        /* These are 1-relative */
        index = type->parameter_index - 1;
                                        /* Switch laboriously for each */
                                        /* possible parameter id and index */
        switch (type->parameter_id)
            {
            case REF_PCAL_AMP:
                value = (double)(fringe->t4100.pcal_data[index].refampl);
                break;
            case REF_PCAL_PHASE:
                value = (double)(fringe->t4100.pcal_data[index].refphase)/100.0;
                break;
                                        /* For pcal diffs just set extras */
                                        /* equal to last valid channel */
            case REF_PCAL_DIFF:
                if (index >= channels) index = channels - 1;
                value = (double)(fringe->t4100.pcal_data[index].refphase
                                - fringe->t4100.pcal_data[0].refphase) / 100.0;
                break;
            case REF_PCAL_FREQ:
                value = (double)(fringe->t4100.pcal_data[index].reffreq);
                break;
            case REF_PCAL_RATE:
                value = fringe->t4500.pcalrate.ref;
                break;
            case REM_PCAL_AMP:
                value = (double)(fringe->t4100.pcal_data[index].remampl);
                break;
            case REM_PCAL_PHASE:
                value = (double)(fringe->t4100.pcal_data[index].remphase)/100.0;
                break;
            case REM_PCAL_DIFF:
                if (index >= channels) index = channels - 1;
                value = (double)(fringe->t4100.pcal_data[index].remphase
                                - fringe->t4100.pcal_data[0].remphase) / 100.0;
                break;
            case REM_PCAL_FREQ:
                value = (double)(fringe->t4100.pcal_data[index].remfreq);
                break;
            case REM_PCAL_RATE:
                value = fringe->t4500.pcalrate.rem;
                break;
            case TRKNO_REF_USB:
                value = fringe->t4000.rectrack[index].rsb.ref;
                break;
            case TRKNO_REF_LSB:
                value = fringe->t4000.rectrack[index].psb.ref;
                break;
            case TRKNO_REM_USB:
                value = fringe->t4000.rectrack[index].rsb.rem;
                break;
            case TRKNO_REM_LSB:
                value = fringe->t4000.rectrack[index].psb.rem;
                break;
            case ERRATE_REF_USB:
                temp = fringe->t4200.errorate[index].rsb.ref;
                if (temp == -32000) value = 0.0;
                else if (temp == 0) value = -1.0;
                else value = pow (10.0,((double)temp)/1000.0);
                break;
            case ERRATE_REF_LSB:
                temp = fringe->t4200.errorate[index].psb.ref;
                if (temp == -32000) value = 0.0;
                else if (temp == 0) value = -1.0;
                else value = pow (10.0,((double)temp)/1000.0);
                break;
            case ERRATE_REM_USB:
                temp = fringe->t4200.errorate[index].rsb.rem;
                if (temp == -32000) value = 0.0;
                else if (temp == 0) value = -1.0;
                else value = pow (10.0,((double)temp)/1000.0);
                break;
            case ERRATE_REM_LSB:
                temp = fringe->t4200.errorate[index].psb.rem;
                if (temp == -32000) value = 0.0;
                else if (temp == 0) value = -1.0;
                else value = pow (10.0,((double)temp)/1000.0);
                break;
            case NAP_USB:
                value = fringe->t4000.accum_per[index].rsb;
                break;
            case NAP_LSB:
                value = fringe->t4000.accum_per[index].psb;
                break;
            case COREL_AMP:
                value = fringe->t4500.ampphas[index].ampl;
                break;
            case COREL_PHASE:
                value = fringe->t4500.ampphas[index].phase;
                break;
            case RATE_ERROR:
                value = fringe->t4500.rate_error;
                break;
            case MBDELAY_ERROR:
                value = fringe->t4500.delay_sigma;
                break;
            case SBDELAY_ERROR:
                value = fringe->t4500.sbd_error;
                break;
            case TOTAL_PHASE:
                value = fringe->t4500.totphase;
                break;
            case TOT_PHASE_MID:
                value = fringe->t4500.totphase_cen;
                break;
            case INCOH_AMP_SEG:
                value = fringe->t4500.amplitude;
                break;
            case INCOH_AMP_FREQ:
                value = fringe->t4500.aamplitude;
                break;
            case MHZ_ARCSEC_NS:
                value = fringe->t4500.vr_deriv;
                break;
            case MHZ_ARCSEC_EW:
                value = fringe->t4500.ur_deriv;
                break;
            case PCNT_DISCARD:
                value = fringe->t4500.discard;
                break;
            case MIN_MAX_RATIO:
                value = fringe->t4500.accept_ratio;
                break;
            case LO_FREQUENCY:
                value = fringe->t4400.lo_freqs[index];
                break;
            case XPERROR:
                value = fringe->t4200.xperror;
                break;
            case YPERROR:
                value = fringe->t4200.yperror;
                break;
            case SUPPRESS:
                value = fringe->t4200.suppress;
                break;
            case PPUPDATE:
                value = fringe->t4200.ppupdate;
                break;
            case XSLIP:
                value = fringe->t4200.xslip;
                break;
            case YSLIP:
                value = fringe->t4200.yslip;
                break;
            case BADSYNC:
                value = fringe->t4200.badsync;
                break;
            case REF_DRIVE:
                value = fringe->t4000.ref_drive;
                break;
            case REM_DRIVE:
                value = fringe->t4000.rem_drive;
                break;
            case UTC_CENTRAL:
                datef_to_datec (&(fringe->t4000.utc_central), &tempdate);
                utc_int = time_to_int (tempdate.year, tempdate.day_of_year,
                                tempdate.hour, tempdate.minute, tempdate.second);
                value = (double)utc_int / 86400.0;
                break;
            case UTC_EPOCH:
                datef_to_datec (&(fringe->t4000.utc_tag), &tempdate);
                utc_int = time_to_int (tempdate.year, tempdate.day_of_year,
                                tempdate.hour, tempdate.minute, tempdate.second);
                value = (double)utc_int / 86400.0;
                break;
            case CLOCK_DELAY:
                value = fringe->t4400.clock;
                break;
            case PROB_FALSE:
                value = fringe->t4500.prob_false;
                break;
                                        /* The rest come from the A-file directly */
            case LENGTH:
                value = data->length;
                break;
                                        /* This sets first day to day 0 */
            case TIMETAG:
                day = fsumm.begin / 86400;
                value = (double)(data->time_tag - (day * 86400)) / 86400.0;
                break;
            case AMPLITUDE:
                value = data->amp;
                break;
            case SNR:
                value = data->snr;
                break;
            case PHASE:
                value = data->resid_phas;
                break;
            case RESID_SBD:
                value = data->sbdelay;
                break;
            case RESID_MBD:
                value = data->mbdelay;
                break;
            case AMBIGUITY:
                value = data->ambiguity;
                break;
            case RESID_RATE:
                value = data->delay_rate;
                break;
            case REF_ELEVATION:
                value = data->ref_elev;
                break;
            case REM_ELEVATION:
                value = data->rem_elev;
                break;
            case REF_AZIMUTH:
                value = data->ref_az;
                break;
            case REM_AZIMUTH:
                value = data->rem_az;
                break;
            case U:
                value = data->u;
                break;
            case V:
                value = data->v;
                break;
            case REF_FREQUENCY:
                value = data->ref_freq;
                break;
            case TOTAL_EC_PHASE:
                value = data->total_phas;
                break;
            case TOTAL_RATE:
                value = data->total_rate;
                break;
            case TOTAL_MBD:
                value = data->total_mbdelay;
                break;
            case TOTAL_SBD_MBD:
                value = data->total_sbresid;
                break;
                                        /* This sets first day of observation to day 0 */
            case PROCDATE:
                day = fsumm.begin / 86400;
                value = (double)(data->procdate - (day * 86400)) / 86400.0;
                break;
            case QCODE:
                if (data->quality <= '9') value = (double)(data->quality - '0');
                else value = (double)(data->quality - 'G');
                break;
            default:
                error = TRUE;
                break;
            }
        user_param->parameter[i][user_param->npoints] = value;
        }
                                        /* End loop and exit */
    if (error) return (1);
    return (0);
    }
