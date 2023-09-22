/************************************************************************/
/*									*/
/* This routine parses the input line, then formats user-selected	*/
/* fields into an output line.						*/
/*									*/
/*	Inputs:		line		Input A-file line		*/
/*			fields		filled flist struct array	*/
/*									*/
/*	Output:		outline		ready to be written out		*/
/*			return value	0=success			*/
/*									*/
/* Created April 4 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adata.h"
#include "adump.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
strip_tline (char *line,
             struct flist fields[],
             char *outline)
    {
    int i, hour_boundary, epoch, len, ret;
    int syear, sday, shour, smin, ssec;
    double value;
    char buf[50], fld_value[50], roots[50], elevations[50], azimuths[50];
    char extents[50], lengths[50];
    trianglesum tdata;
					/* Parse the input */
    if ((ret = parse_tsumm (line, &tdata)) != 0)
	{
	msg ("Failure parsing A-file line, ret=%d", 3, ret);
	return (1);
	}
    int_to_time (tdata.time_tag, &syear, &sday, &shour, &smin, &ssec);
					/* Loop over all requested fields */
    i = 0;
    outline[0] = '\0';
    while (fields[i].id != 0)
	{
	switch (fields[i].id)
	    {
	    case T_VERSION:
		break;
	    case T_EXPT_NO:
		sprintf (fld_value, fields[i].format, tdata.expt_no);
		break;
	    case T_TYPE:
		sprintf (fld_value, fields[i].format, 3);
		break;
	    case T_SCANYEAR:
		sprintf (fld_value, fields[i].format, syear);
		break;
	    case T_TIMETAG:
		sprintf (buf, "%03d-%02d%02d%02d", sday, shour, smin, ssec);
		sprintf (fld_value, fields[i].format, buf);
		break;
	    case T_SOURCE:
		sprintf (fld_value, fields[i].format, tdata.source);
		break;
	    case T_FREQ_CODE:
		sprintf (fld_value, fields[i].format, tdata.freq_code);
		break;
	    case T_MODE:
		sprintf (fld_value, fields[i].format, tdata.mode);
		break;
	    case T_TRIANGLE:
		sprintf (fld_value, fields[i].format, tdata.triangle);
		break;
	    case T_ROOT:
		strcpy (roots, tdata.root_id[0]);
		strcat (roots, ",");
		if (strcmp (tdata.root_id[0], tdata.root_id[1]) != 0)
		    strcat (roots, tdata.root_id[1]);
		strcat (roots, ",");
		if (strcmp (tdata.root_id[1], tdata.root_id[2]) != 0)
		    strcat (roots, tdata.root_id[2]);
		sprintf (fld_value, fields[i].format, roots);
		break;
	    case T_EXTENT:
		sprintf (extents, "%d,%d,%d", tdata.extent_no[0], 
						tdata.extent_no[1],
						tdata.extent_no[2]);
		sprintf (fld_value, fields[i].format, extents);
		break;
	    case T_LENGTH:
		sprintf (lengths, "%d,%d,%d", tdata.length[0], 
						tdata.length[1],
						tdata.length[2]);
		sprintf (fld_value, fields[i].format, lengths);
		break;
	    case T_DURATION:
		sprintf (fld_value, fields[i].format, tdata.duration); 
		break;
	    case T_OFFSET:
		sprintf (fld_value, fields[i].format, tdata.offset); 
		break;
	    case T_SQUALITY:
		sprintf (fld_value, fields[i].format, tdata.scan_quality);
		break;
	    case T_DQUALITY:
		sprintf (fld_value, fields[i].format, tdata.data_quality);
		break;
	    case T_ESDESP:
		sprintf (fld_value, fields[i].format, tdata.esdesp);
		break;
	    case T_BISAMP:
		sprintf (fld_value, fields[i].format, tdata.bis_amp);
		break;
	    case T_BISSNR:
		sprintf (fld_value, fields[i].format, tdata.bis_snr);
		break;
	    case T_BISPHASE:
		sprintf (fld_value, fields[i].format, tdata.bis_phas);
		break;
	    case T_DATATYPE:
		sprintf (fld_value, fields[i].format, tdata.datatype);
		break;
	    case T_CSBDELAY:
		sprintf (fld_value, fields[i].format, tdata.csbdelay);
		break;
	    case T_CMBDELAY:
		sprintf (fld_value, fields[i].format, tdata.cmbdelay);
		break;
	    case T_AMBIGUITY:
		sprintf (fld_value, fields[i].format, tdata.ambiguity);
		break;
	    case T_CDRATE:
		sprintf (fld_value, fields[i].format, tdata.cdelay_rate);
		break;
	    case T_ELEVATION:
		sprintf (elevations, "%.1f,%.1f,%.1f", tdata.elevation[0],
				tdata.elevation[1], tdata.elevation[2]);
		sprintf (fld_value, fields[i].format, elevations);
		break;
	    case T_AZIMUTH:
		sprintf (azimuths, "%d,%d,%d", (int)tdata.azimuth[0],
			(int)tdata.azimuth[1], (int)tdata.azimuth[2]);
		sprintf (fld_value, fields[i].format, azimuths);
		break;
	    case T_EPOCH:
		sprintf (buf, "%02d%02d", tdata.epoch[0], tdata.epoch[1]);
		sprintf (fld_value, fields[i].format, buf);
		break;
	    case T_REF_FREQ:
		sprintf (fld_value, fields[i].format, tdata.ref_freq);
		break;
					/* These are derived quantities */
	    case T_SCANDAY:
		value = (double)tdata.time_tag / 86400.0;
		sprintf (fld_value, fields[i].format, value);
		break;
	    case T_EPOCHDAY:
					/* Compute ref. epoch within 30 min of */
					/* scan time */
		epoch = 60 * tdata.epoch[0] + tdata.epoch[1];
		hour_boundary = (tdata.time_tag / 3600) * 3600;
		epoch += hour_boundary;
		if ((epoch - tdata.time_tag) > 1800) epoch -= 3600;
		if ((epoch - tdata.time_tag) < -1800) epoch += 3600;
		sprintf (fld_value, fields[i].format, (double)epoch / 86400.0);
		break;
	    default:
		msg ("Unrecognized field in strip_tline()", 3);
		return (1);
	    }
					/* Add new field to output */
					/* and loop for next field */
	strcat (outline, fld_value);
	strcat (outline, " ");
	i++;
	}
					/* Append newline and quit */
    len = strlen (outline);
    outline[len-1] = '\n';
    return (0);
    }
