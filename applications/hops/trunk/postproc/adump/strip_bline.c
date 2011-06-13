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
/* Created March 3 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adata.h"
#include "adump.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
strip_bline (char *line,
             struct flist fields[],
             char *outline)
    {
    int i, j, hour_boundary, epoch, len;
    int pyear, pday, phour, pmin, psec, syear, sday, shour, smin, ssec;
    double value;
    char buf[50], fld_value[50], parents[50];
    fringesum fdata;
					/* Parse the input */
    if (parse_fsumm (line, &fdata) != 0)
	{
	msg ("Failure parsing A-file line", 3);
	return (1);
	}
    int_to_time (fdata.procdate, &pyear, &pday, &phour, &pmin, &psec);
    int_to_time (fdata.time_tag, &syear, &sday, &shour, &smin, &ssec);
					/* Loop over all requested fields */
    i = 0;
    outline[0] = '\0';
    while (fields[i].id != 0)
	{
	switch (fields[i].id)
	    {
	    case VERSION:
		break;
	    case ROOTCODE:
		sprintf (fld_value, fields[i].format, fdata.root_id);
		break;
	    case TYPE:
		sprintf (fld_value, fields[i].format, 2);
		break;
	    case EXTENT:
		sprintf (fld_value, fields[i].format, fdata.extent_no);
		break;
	    case DURATION:
		sprintf (fld_value, fields[i].format, fdata.duration); 
		break;
	    case LENGTH:
		sprintf (fld_value, fields[i].format, fdata.length);
		break;
	    case OFFSET:
		sprintf (fld_value, fields[i].format, fdata.offset); 
		break;
	    case EXPT_NO:
		sprintf (fld_value, fields[i].format, fdata.expt_no);
		break;
	    case PROCDATE:
		sprintf (buf, "%2d%03d%c%02d%02d", 
				pyear, pday, fdata.corel_vers, phour, pmin);
		sprintf (fld_value, fields[i].format, buf);
		break;
	    case SCANYEAR:
		sprintf (fld_value, fields[i].format, syear);
		break;
	    case TIMETAG:
		sprintf (buf, "%03d-%02d%02d%02d", sday, shour, smin, ssec);
		sprintf (fld_value, fields[i].format, buf);
		break;
	    case SOURCE:
		sprintf (fld_value, fields[i].format, fdata.source);
		break;
	    case BASELINE:
		sprintf (fld_value, fields[i].format, fdata.baseline);
		break;
	    case QUALITY:
		sprintf (fld_value, fields[i].format, fdata.quality);
		break;
	    case FREQ_CODE:
		sprintf (fld_value, fields[i].format, fdata.freq_code);
		break;
	    case MODE:
		sprintf (fld_value, fields[i].format, fdata.mode);
		break;
	    case NFREQ:
		sprintf (fld_value, fields[i].format, fdata.no_freq);
		break;
	    case AMP:
		sprintf (fld_value, fields[i].format, fdata.amp);
		break;
	    case SNR:
		sprintf (fld_value, fields[i].format, fdata.snr);
		break;
	    case PHASE:
		sprintf (fld_value, fields[i].format, fdata.resid_phas);
		break;
	    case DATATYPE:
		sprintf (fld_value, fields[i].format, fdata.datatype);
		break;
	    case SBDELAY:
		sprintf (fld_value, fields[i].format, fdata.sbdelay);
		break;
	    case MBDELAY:
		sprintf (fld_value, fields[i].format, fdata.mbdelay);
		break;
	    case AMBIGUITY:
		sprintf (fld_value, fields[i].format, fdata.ambiguity);
		break;
	    case DRATE:
		sprintf (fld_value, fields[i].format, fdata.delay_rate);
		break;
	    case REF_ELEVATION:
		sprintf (fld_value, fields[i].format, fdata.ref_elev);
		break;
	    case REM_ELEVATION:
		sprintf (fld_value, fields[i].format, fdata.rem_elev);
		break;
	    case REF_AZIMUTH:
		sprintf (fld_value, fields[i].format, fdata.ref_az);
		break;
	    case REM_AZIMUTH:
		sprintf (fld_value, fields[i].format, fdata.rem_az);
		break;
	    case U:
		sprintf (fld_value, fields[i].format, fdata.u);
		break;
	    case V:
		sprintf (fld_value, fields[i].format, fdata.v);
		break;
	    case ESDESP:
		sprintf (fld_value, fields[i].format, fdata.esdesp);
		break;
	    case EPOCH:
		sprintf (buf, "%02d%02d", fdata.epoch[0], fdata.epoch[1]);
		sprintf (fld_value, fields[i].format, buf);
		break;
	    case REF_FREQ:
		sprintf (fld_value, fields[i].format, fdata.ref_freq);
		break;
	    case TOT_PHASE:
		sprintf (fld_value, fields[i].format, fdata.total_phas);
		break;
	    case TOT_DRATE:
		sprintf (fld_value, fields[i].format, fdata.total_rate);
		break;
	    case TOT_MBDELAY:
		sprintf (fld_value, fields[i].format, fdata.total_mbdelay);
		break;
	    case TOT_MBDSBD:
		sprintf (fld_value, fields[i].format, fdata.total_sbresid);
		break;
	    case SCOTIME:
		sprintf (fld_value, fields[i].format, fdata.srch_cotime);
		break;
	    case NCOTIME:
		sprintf (fld_value, fields[i].format, fdata.noloss_cotime);
		break;
	    case PARENTS:
		parents[0] = '0';
		parents[1] = '\0';
		while (fdata.parents[j] > 0)
		    {
		    if (j == 0) sprintf (parents, "%d", fdata.parents[j]);
		    else
			{
			sprintf (buf, ",%d", fdata.parents[j]);
			strcat (parents, buf);
			}
		    j++;
		    }
		sprintf (fld_value, fields[i].format, parents);
		break;
					/* These are derived quantities */
	    case PROCDAY:
		value = (double)fdata.procdate / 86400.0;
		sprintf (fld_value, fields[i].format, value);
		break;
	    case SCANDAY:
		value = (double)fdata.time_tag / 86400.0;
		sprintf (fld_value, fields[i].format, value);
		break;
	    case EPOCHDAY:
					/* Compute ref. epoch within 30 min of */
					/* scan time */
		epoch = 60 * fdata.epoch[0] + fdata.epoch[1];
		hour_boundary = (fdata.time_tag / 3600) * 3600;
		epoch += hour_boundary;
		if ((epoch - fdata.time_tag) > 1800) epoch -= 3600;
		if ((epoch - fdata.time_tag) < -1800) epoch += 3600;
		sprintf (fld_value, fields[i].format, (double)epoch / 86400.0);
		break;
	    default:
		msg ("Unrecognized field in strip_line()", 3);
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
