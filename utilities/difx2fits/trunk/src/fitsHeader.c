#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include "difx2fits.h"

#define SEC_DAY         86400.0             /* seconds in a mean solar day */
#define MUSEC_DAY       86400000000.0       /* mus in a mean solar day */
#define MJD_UNIX0       40587.0             /* MJD at beginning of unix time */

double timeMjd()
{
	struct timeval t;

	gettimeofday(&t, 0);
	
	return MJD_UNIX0 + t.tv_sec/SEC_DAY + t.tv_usec/MUSEC_DAY;
}

const DifxInput *DifxInput2FitsHeader(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char ref_date[12];
	char str[64], strng[132];
	char local_time[48];

	if(D == 0)
	{
		return 0;
	}

	mjd2fits((int)(D->mjdStart), ref_date);

	fitsWriteLogical(out, "SIMPLE", 1, "Standard FITS format");
	fitsWriteInteger(out, "BITPIX", 8, "");
	fitsWriteInteger(out, "NAXIS",  0, "");


	fitsWriteLogical(out, "EXTEND", 1, "");
	fitsWriteLogical(out, "BLOCKED", 1, "");
	fitsWriteString(out, "OBJECT", "BINARYTB", "");
	fitsWriteString(out, "TELESCOP", "VLBA", "");
	fitsWriteString(out, "DIFX", "1.0-VLBA", "");
	fitsWriteString(out, "OBSERVER", D->obsCode, "");
	fitsWriteString(out, "ORIGIN", "VLBA Correlator", "");
	fitsWriteString(out, "DATE-OBS", ref_date, "");
	mjd2fits ((int)timeMjd (), strng);
	fitsWriteString(out, "DATE-MAP", strng, "Correlation date");
	fitsWriteLogical(out, "GROUPS", 1, "");
	fitsWriteInteger(out, "GCOUNT", 0, "");
	fitsWriteInteger(out, "PCOUNT", 0, "");

	/* get current local date and time */
	timeMjd2str (timeMjd (), local_time);
	strcpy(strng, "OPENED FITS FILE : ");
	strcat(strng, local_time);
	fitsWriteComment(out, "HISTORY", strng);

	sprintf (strng, "LOG FILE : /To/be/implemented");
	fitsWriteComment(out, "HISTORY", strng);

	sprintf (strng, "OBSCODE : %s", D->obsCode);
	fitsWriteComment(out, "HISTORY", strng);

	if(D->obsSession[0])
	{
		sprintf (strng, "SESSION : %s", D->obsSession);
		fitsWriteComment(out, "HISTORY", strng);
	}
	
	sprintf (strng, "JOBNUM : %d", D->jobId);
	fitsWriteComment(out, "HISTORY", strng);

	sprintf (strng, "DIFXJOB : %d.%d.%d", 
		D->jobId, D->subjobId, D->subarrayId);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->jobStart, "", str);
	sprintf (strng, "JOBSTART : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->jobStop, "", str);
	sprintf (strng, "JOBSTOP : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStart, "", str);
	sprintf (strng, "FILESTART : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	time2str(D->mjdStart+D->duration/86400.0, "", str);
	sprintf (strng, "FILESTOP  : %s", str);
	fitsWriteComment(out, "HISTORY", strng);

	fitsWriteEnd(out);

	return D;
}
