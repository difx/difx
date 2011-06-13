/************************************************************************/
/*									*/
/* Summarizes the state of the user parameter arrays for the information*/
/* of the user.  Analyze consistency of the parameters with the state	*/
/* of main memory.							*/
/*									*/
/*	Inputs:		data		Needed to do the job properly	*/
/*			user_param	via extern			*/
/*									*/
/*	Output:		Printout on terminal				*/
/*			return value		0=OK, !=0 BAD		*/
/*									*/
/* Created December 22 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "usearray.h"

int
param_list (data)
esum *data;
    {
    int i, npflag, npunflag;
    extern struct usearray user_param;
    extern int fscan, fflag;
					/* No point continuing if no parameters exist */
    if (user_param.npoints == 0)
	{
	msg ("No extracted parameters present!", 2);
	return (0);
	}
					/* Count up scans in various states */
    npflag = npunflag = 0;
    for (i=0; i<fscan; i++) 
	{
	if (data->fdata[i].param_ptr >= 0) 
	    {
	    if (data->fdata[i].flag == 0) npunflag++;
	    else npflag++;
	    }
	}
					/* These should always match */
    if ((npflag+npunflag) != user_param.npoints)
	{
	msg ("Error: number of parameter pointers in main array (%d)", 
		3, npflag+npunflag);
	msg ("does not match number listed in parameter arrays (%d)", 
		3, user_param.npoints);
	return (1);
	}
					/* Tell user general state of memory */
    msg ("Extracted parameters exist for %d fringe records in memory,", 
	2, user_param.npoints);
    if (npunflag == user_param.npoints)
	msg ("all of which are currently unflagged\n", 2);
    else if (npflag > 0)
	msg ("of which %d are unflagged\n", 2, npunflag);
					/* Some good data with no parameters ... */
    if ((fscan-fflag) > npunflag)
	{
	msg ("NOTE: There are %d unflagged fringe records WITHOUT extracted parameters",
		2, fscan - fflag - npunflag);
	msg ("You must rerun 'parameter' if you want extracted parameter information", 2);
	msg (" for these scans\n", 2);
	}
    else msg ("All unflagged fringe records have extracted parameters\n", 2);
					/* This will be very common */
    if (fflag > npflag)
	{
	msg ("There are %d flagged fringe records without extracted parameters",
                2, fflag - npflag);
	msg ("(unflagging these data will result in incomplete parameter information)\n", 2);
	}
					/* Unless user already got warned above ... */
    if ((fscan-fflag) == npunflag)
	{
	msg ("If you read in more data, the extracted parameter information will be", 2);
	msg ("incomplete.  You must then rerun 'parameter' if complete parameter", 2);
	msg ("information is desired\n", 2);
	}

    msg ("Below is a list of extracted parameters in memory.  Use the numbers at", 2);
    msg ("left to identify parameters for the PRANGE filter command.\n", 2);
					/* Loop over user_param array */
    for (i=0; i<user_param.nparms; i++)
	{
	if (user_param.type[i].parameter_index > 0) 
	    msg ("%d\t%s(%d)", 2, i+1, user_param.type[i].parameter_name,
						user_param.type[i].parameter_index);
	else msg ("%d\t%s", 2, i+1, user_param.type[i].parameter_name);
	}

    return (0);
    }
