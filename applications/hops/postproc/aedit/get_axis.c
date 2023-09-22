/************************************************************************/
/*									*/
/* This routine is designed to recognize an ascii axis identifier	*/
/* typed by the user in the "axis" command, and fills in the full name	*/
/* and id number of the corresponding axis.  The job is complicated	*/
/* somewhat by the special handling necessary for phasecals, and the	*/
/* fact that some extracted parameters are arrays, requiring some extra	*/
/* syntax parsing.							*/
/*									*/
/*	Inputs:		string		User input			*/
/*									*/
/*	Output:		ax_name		Official name (goes on plots)	*/
/*			ax_index	Used in switch statements later	*/
/*									*/
/* Created 15 April 1989 by CJL						*/
/* Added yaind element to inp struct, supported here 7 Sep 1993 by CJL	*/
/* Made into generic axis selection routine for either X or Y in 	*/
/* support of flexible parameter plotting, 4 February 1994 by CJL	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "pstruct.h"
#include "usearray.h"
#include "aedit.h"

struct
    {
    int		axis_index;
    int		plotby;
    char	*axisname;
    char	*axisunits;
    } 
    axes[] =
	{
	AX_TIMETAG, ANY_PLOT, "timetag", "",
	AX_PCAL_PHASE, STATION_PLOT | BASELINE_PLOT, "pcal_phase", "deg",
	AX_PCAL_DIFF, STATION_PLOT | BASELINE_PLOT, "pcal_diff", "deg",
	AX_PCAL_AMP, STATION_PLOT | BASELINE_PLOT, "pcal_amp", "",
	AX_ERRORATE, STATION_PLOT | BASELINE_PLOT, "error_rate", "",
	AX_ELEVATION, STATION_PLOT | BASELINE_PLOT, "elevation", "deg",
	AX_AZIMUTH, STATION_PLOT | BASELINE_PLOT, "azimuth", "deg",
	AX_SNR, BASELINE_PLOT, "snr", "",
	AX_AMPLITUDE, BASELINE_PLOT, "amplitude", "e-4",
	AX_PHASE, BASELINE_PLOT, "phase", "deg",
	AX_SBDELAY, BASELINE_PLOT, "sbdelay", "us",
	AX_MBDELAY, BASELINE_PLOT, "mbdelay", "ns",
	AX_DRATE, BASELINE_PLOT, "drate", "ps/s",
	AX_NCOTIME, BASELINE_PLOT, "ncotime", "sec",
	AX_SCOTIME, BASELINE_PLOT, "scotime", "sec",
	AX_CPHASE, TRIANGLE_PLOT, "cphase", "deg",
	AX_CRATE, TRIANGLE_PLOT, "crate", "ps/s",
	AX_CSBDELAY, TRIANGLE_PLOT, "csbdelay", "us",
	AX_CMBDELAY, TRIANGLE_PLOT, "cmbdelay", "ns",
	AX_CAMP, QUAD_PLOT, "campl", "",
	AX_U, ALL_PLOT, "u ", "Mlambda",
	AX_V, ALL_PLOT, "v ", "Mlambda",
	AX_UVDIST, ALL_PLOT | BASELINE_PLOT, "uvdist", "Mlambda",
	AX_PARAMETER, BASELINE_PLOT, "param", "",
	NO_AXIS, ALL_PLOT, NULL, NULL
	};

int get_axis (char *string, char *ax_name, int *ax_index, char *ax_units, int *aux, int *plotby)
    {
    int n, len, i, j, match, array_index, param_no, pindex;
    int ref_phase, rem_phase, ref_diff, rem_diff, ref_amp, rem_amp;
    char c, name[20], dummy[80];
    extern struct usearray user_param;
					/* Pull out array index syntax if present */
					/* Only supported for phasecal quantities */
    array_index = -1;
    strcpy (dummy, string);
    len = strlen (dummy);
    for (i = 0; i < len; i++) 
	if ((dummy[i] == '(') || (dummy[i] == ')')) dummy[i] = ' ';
    n = sscanf (dummy, "%s %d", name, &array_index);
					/* Case-insensitive matchup ... */
					/* This is the easy part. */
    len = strlen (name);
					/* Avoid ambiguity for u, v */
    if (len == 1) 
	{
	strcat (name, " ");
	len = 2;
	}

    i = 0;
    match = -1;
    while (axes[i].axis_index != NO_AXIS)
	{
					/* "param" must be followed by id number */
	if (axes[i].axis_index == AX_PARAMETER) n = 5;
	else n = len;
	if (strncasecmp (name, axes[i].axisname, n) == 0) 
	    {
	    if (match >= 0)
		{
		msg ("Ambiguous axis specifier '%s', see 'help axis'", 2, name);
		return (-1);
		break;
		}
	    else match = i;
	    }
	i++;
	}
    if (match < 0)
	{
	msg ("Unrecognized axis specifier '%s', see 'help axis'", 2, name);
	return (-1);
	}
					/* Copy it to output. */
    strcpy (ax_name, axes[match].axisname);
    strcpy (ax_units, axes[match].axisunits);
    *ax_index = axes[match].axis_index;
    *aux = 0;
    *plotby = axes[match].plotby;
					/* Special case handling, pain in the neck */
					/* Figure out which parameter, get name */
    if (*ax_index == AX_PARAMETER)
	{
	*ax_index = NO_AXIS;
	if (sscanf (name, "param%d", &param_no) != 1)
	    msg ("Incorrect parameter axis specification, see 'help axis'", 2);
	else if (param_no > user_param.nparms)
	    msg ("'param%d' specification incorrect, only %d parameters present",
		2, param_no, user_param.nparms);
					/* OK. User sees these 1-relative */
	else
	    {
	    *ax_index = AX_PARAMETER;
	    pindex = user_param.type[param_no-1].parameter_index;
	    if (pindex == 0)
		strcpy (name, user_param.type[param_no-1].parameter_name);
	    else
		sprintf (name, "%s(%d)", 
			user_param.type[param_no-1].parameter_name, pindex);
	    strcpy (ax_name, name);
	    *aux = param_no-1;
	    }
	}
					/* Now do the phasecals */
    if ((*ax_index == AX_PCAL_PHASE) || (*ax_index == AX_PCAL_DIFF)
		|| (*ax_index == AX_PCAL_AMP))
	{
	if (array_index < 1)
	    {
            msg ("pcal axes must be accompanied by array index, see 'help plot'", 2);
	    *ax_index = NO_AXIS;
	    }
					/* First, see what parameters are there */
	ref_phase = rem_phase = ref_diff = rem_diff = ref_amp = rem_amp = -1;
	for (j=0; j<user_param.nparms; j++)
	    {
					/* Get requested element only */
	    if (user_param.type[j].parameter_index != array_index) continue;
	    if (user_param.type[j].parameter_id == REF_PCAL_PHASE) ref_phase = j;
	    if (user_param.type[j].parameter_id == REM_PCAL_PHASE) rem_phase = j;
	    if (user_param.type[j].parameter_id == REF_PCAL_DIFF) ref_diff = j;
	    if (user_param.type[j].parameter_id == REM_PCAL_DIFF) rem_diff = j;
	    if (user_param.type[j].parameter_id == REF_PCAL_AMP) ref_amp = j;
	    if (user_param.type[j].parameter_id == REM_PCAL_AMP) rem_amp = j;
	    }
					/* Now have parameter array indices of ref */
					/* and remote quantities.  We encode these */
					/* into ax_index.  Recognition of pcal quantities */
					/* when plotting has to be kludged through */
					/* string recognition of ax_name */
	switch (*ax_index)
	    {
	    case AX_PCAL_PHASE:
		if ((ref_phase < 0) || (rem_phase < 0))
		    {
		    msg ("Extract ref and remote pc phases before specifying axis", 2);
		    *ax_index = NO_AXIS;
		    }
		else
		    *aux = MAX_PARMS*ref_phase + rem_phase;
		break;
	    case AX_PCAL_DIFF:
		if ((ref_diff < 0) || (rem_diff < 0))
		    {
		    msg ("Extract ref and remote pc diff before specifying axis", 2);
		    *ax_index = NO_AXIS;
		    }
		else
		    *aux = MAX_PARMS*ref_diff + rem_diff;
		break;
	    case AX_PCAL_AMP:
		if ((ref_amp < 0) || (rem_amp < 0))
		    {
		    msg ("Extract ref and remote pc amp before specifying axis", 2);
		    *ax_index = NO_AXIS;
		    }
		else
		    *aux = MAX_PARMS*ref_amp + rem_amp;
		break;
	    default:
		;
	    }
	sprintf (name, "%s(%d)", ax_name, array_index);
	strcpy (ax_name, name);
	}
	
    if (*ax_index == NO_AXIS) return (-1);
    else return (0);
    }
