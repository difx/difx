/************************************************************************/
/*                                                                      */
/* This routine takes a filled fringe structure and the usearray        */
/* structure, and fills in the usearray parameter slots for the current */
/* scan with the requested information from the fringe structure.       */
/*                                                                      */
/*      Inputs:         fringe4         filled in by read_mk4fringe()   */
/*                      user_param      specifies parameters to be      */
/*                                      extracted                       */
/*                                                                      */
/*      Output:         user_param      parameters filled in for this   */
/*                                      scan                            */
/*                                                                      */
/* Mk4 version of fill_parms()                                          */
/* created by CJL April 20, 2000                                        */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include "summary.h"
#include "aedata.h"
#include "mk4_data.h"
#include "usearray.h"

int
fill4_parms (fringe4, data, user_param)
struct mk4_fringe *fringe4;
fringesum *data;
struct usearray *user_param;
    {
    double value;
    int i, day, index, channels, error, temp, utc_int, chan;
    struct udat *type;
    extern struct datasumm fsumm;

    error = FALSE;
                                        /* Loop over all parameters for this */
                                        /* fringe file */
    channels = data->no_freq;
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
                value = (double)(fringe4->t207->ref_pcamp[index].usb);
                break;
            case REF_PCAL_PHASE:
                value = (double)(fringe4->t207->ref_pcphase[index].usb);
                break;
                                        /* For pcal diffs just set extras */
                                        /* equal to last valid channel */
            case REF_PCAL_DIFF:
                if (index >= channels) index = channels - 1;
                value = (double)(fringe4->t207->ref_pcphase[index].usb
                                - fringe4->t207->ref_pcphase[0].usb);
                break;
            case REF_PCAL_FREQ:
                value = (double)(fringe4->t207->ref_pcfreq[index].usb);
                break;
            case REF_PCAL_RATE:
                value = fringe4->t207->ref_pcrate;
                break;
            case REM_PCAL_AMP:
                value = (double)(fringe4->t207->rem_pcamp[index].usb);
                break;
            case REM_PCAL_PHASE:
                value = (double)(fringe4->t207->rem_pcphase[index].usb);
                break;
            case REM_PCAL_DIFF:
                if (index >= channels) index = channels - 1;
                value = (double)(fringe4->t207->rem_pcphase[index].usb
                                - fringe4->t207->rem_pcphase[0].usb);
                break;
            case REM_PCAL_FREQ:
                value = (double)(fringe4->t207->rem_pcfreq[index].usb);
                break;
            case REM_PCAL_RATE:
                value = fringe4->t207->rem_pcrate;
                break;
            case TRKNO_REF_USB:
                value = 0;
                break;
            case TRKNO_REF_LSB:
                value = 0;
                break;
            case TRKNO_REM_USB:
                value = 0;
                break;
            case TRKNO_REM_LSB:
                value = 0;
                break;
            case ERRATE_REF_USB:
                value = fringe4->t207->ref_errate[index];
                break;
            case ERRATE_REF_LSB:
                value = fringe4->t207->ref_errate[index];
                break;
            case ERRATE_REM_USB:
                value = fringe4->t207->rem_errate[index];
                break;
            case ERRATE_REM_LSB:
                value = fringe4->t207->rem_errate[index];
                break;
            case NAP_USB:
                value = fringe4->t206->accepted[index].usb;
                break;
            case NAP_LSB:
                value = fringe4->t206->accepted[index].lsb;
                break;
            case COREL_AMP:
                value = fringe4->t210->amp_phas[index].ampl;
                break;
            case COREL_PHASE:
                value = fringe4->t210->amp_phas[index].phase;
                break;
            case RATE_ERROR:
                value = fringe4->t208->rate_error;
                break;
            case MBDELAY_ERROR:
                value = fringe4->t208->mbd_error;
                break;
            case SBDELAY_ERROR:
                value = fringe4->t208->sbd_error;
                break;
            case TOTAL_PHASE:
                value = fringe4->t208->totphase;
                break;
            case TOT_PHASE_MID:
                value = 0.0;
                break;
            case INCOH_AMP_SEG:
                value = fringe4->t208->inc_seg_ampl;
                break;
            case INCOH_AMP_FREQ:
                value = fringe4->t208->inc_chan_ampl;
                break;
            case MHZ_ARCSEC_NS:
                value = fringe4->t202->v;
                break;
            case MHZ_ARCSEC_EW:
                value = fringe4->t202->u;
                break;
            case PCNT_DISCARD:
                value = fringe4->t206->discard;
                break;
            case MIN_MAX_RATIO:
                value = fringe4->t206->accept_ratio;
                break;
            case LO_FREQUENCY:
                chan = fringe4->t205->ffit_chan[index].channels[0];
                value = fringe4->t203->channels[chan].ref_freq;
                break;
            case XPERROR:
                value = 0.0;
                break;
            case YPERROR:
                value = 0.0;
                break;
            case SUPPRESS:
                value = 0.0;
                break;
            case PPUPDATE:
                value = 0.0;
                break;
            case XSLIP:
                value = 0.0;
                break;
            case YSLIP:
                value = 0.0;
                break;
            case BADSYNC:
                value = 0.0;
                break;
            case REF_DRIVE:
                value = 0.0;
                break;
            case REM_DRIVE:
                value = 0.0;
                break;
            case UTC_CENTRAL:
/*                 datef_to_datec (&(fringe->t4000.utc_central), &tempdate); */
/*                 utc_int = time_to_int (tempdate.year, tempdate.day_of_year, */
/*                                 tempdate.hour, tempdate.minute, tempdate.second); */
/*                 value = (double)utc_int / 86400.0; */
                value = 0.0;
                break;
            case UTC_EPOCH:
/*                 datef_to_datec (&(fringe->t4000.utc_tag), &tempdate); */
/*                 utc_int = time_to_int (tempdate.year, tempdate.day_of_year, */
/*                                 tempdate.hour, tempdate.minute, tempdate.second); */
/*                 value = (double)utc_int / 86400.0; */
                value = 0.0;
                break;
                                                /* Sign? */
            case CLOCK_DELAY:
                value = fringe4->t202->rem_clock - fringe4->t202->ref_clock;
                break;
            case PROB_FALSE:
                value = fringe4->t208->prob_false;
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
