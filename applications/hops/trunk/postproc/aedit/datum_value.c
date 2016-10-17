/************************************************************************/
/*									*/
/* Low level routine to figure out the axis value for a data point	*/
/* for a particular axis type.  For amplitudes and phases, the SNR-	*/
/* based error bars are also calculated.				*/
/*									*/
/*	Inputs:		index		axis type number		*/
/*			aux		auxiliary axis specifier	*/
/*			toffset		For time axes only		*/
/*			refrem		for stations in baseline plots	*/
/*			fdatum		Pointer to data array structure */
/*			tdatum		Ptr to closure array structure  */
/*									*/
/*	Output:		value		Pointer to receive result	*/
/*			errh, errl	Pointers to error bar extrema	*/
/*									*/
/* Created 22 February 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "usearray.h"

void datum_value (int index, int aux, int toffset, int refrem,
    fringearray *fdatum, float *value, float *errh, float *errl)
    {
    extern struct inputs inp;
    extern struct usearray user_param;
    int temp, ambig, pindex, pptr;
    fringesum *fdat;

    pptr = fdatum->param_ptr;
    fdat = &(fdatum->data);

    switch (index)
	{
	case AX_TIMETAG:
	    *value = fdat->time_tag - toffset;
	    break;
	case AX_PCAL_PHASE:
	case AX_PCAL_DIFF:
	    if (aux < 0)
		{
		msg ("Axis id auxiliary field corrupt", 2);
		break;
		}
	    if (refrem == REMOTE) pindex = aux % MAX_PARMS;
	    else pindex = aux / MAX_PARMS;
	    *value = user_param.parameter[pindex][pptr];
	    if (*value < 0.0) *value += 360.0;
	    if (*value > 360.0) *value -= 360.0;
	    break;
	case AX_PCAL_AMP:
	    if (aux < 0)
		{
		msg ("Axis id auxiliary field corrupt", 2);
		break;
		}
	    if (refrem == REMOTE) pindex = aux % MAX_PARMS;
	    else pindex = aux / MAX_PARMS;
	    *value = user_param.parameter[pindex][pptr];
	    break;
	case AX_ERRORATE:
	    break;
	case AX_ELEVATION:
	    if (refrem == REMOTE) *value = fdat->rem_elev;
	    else *value = fdat->ref_elev;
	    break;
	case AX_AZIMUTH:
	    if (refrem == REMOTE) *value = fdat->rem_az;
	    else *value = fdat->ref_az;
	    break;
	case AX_SNR:
	    *value = fdat->snr;
	    *errh = *value + 1.0;
	    *errl = *value - 1.0;
	    break;
	case AX_AMPLITUDE:
	    *value = fdat->amp;
	    if (fdat->snr != 0.0)
		{
		*errl = *value - *value/(float)fdat->snr;
		*errh = *value + *value/(float)fdat->snr;
		}
	    else
		{
		*errl = 0.0;
		*errh = 2.0 * *value;
		}
	    break;
	case AX_PHASE:
	    *value = fdat->resid_phas;
	    if (fdat->snr != 0.0)
		{
		*errl = *value - 57.2/(float)fdat->snr;
		*errh = *value + 57.2/(float)fdat->snr;
		}
	    else
		{
		*errl = *value - 180.0;
		*errh = *value + 180.0;
		}
	    break;
	case AX_SBDELAY:
	    *value = fdat->sbdelay;
	    break;
	case AX_MBDELAY:
					/* Must remove ambiguities here */
	    temp = (int)(fdat->mbdelay * 100000.0);
	    ambig = (int)(fdat->ambiguity * 100000.0);
					/* Precautionary */
	    if (ambig == 0) ambig = 50000;
	    temp %= ambig;
	    if(temp > (ambig/2)) temp -= ambig;
	    if(temp < -(ambig/2)) temp += ambig;
	    *value = ((float)temp)/100.0;
	    break;
	case AX_DRATE:
	    *value = fdat->delay_rate;
	    break;
	case AX_NCOTIME:
	    *value = fdat->noloss_cotime;
	    break;
	case AX_SCOTIME:
	    *value = fdat->srch_cotime;
	    break;
	case AX_U:
	    *value = fdat->u;
	    break;
	case AX_V:
	    *value = fdat->v;
	    break;
	case AX_UVDIST:
	    *value = sqrt (fdat->u * fdat->u + fdat->v * fdat->v);
	    break;
	case AX_PARAMETER:
	    *value = user_param.parameter[aux][pptr];
	    break;
	default:
	    msg ("Bad axis in datum_value(), %d", 2, index);
	}
    return;
    }
