/************************************************************************/
/*									*/
/* Generalized mechanism for tracking real and CPU time used by various	*/
/* segments of a program.  The user calls account() whenever he/she	*/
/* wishes to learn how long has elapsed in real, system and user time	*/
/* since the last call.  A running total of each type of time is kept	*/
/* for each program segment in a static time_account structure.		*/
/* The caller passes a string, which is used to identify the portion	*/
/* of code just executed.  If this string is not recognized, it is	*/
/* remembered, and a new entry is made in the time_account structure	*/
/* for it.  								*/
/*									*/
/* There are three "special" strings which cause special		*/
/* actions.  They begin with "!".  "!BEGIN" starts the accounting	*/
/* process.  Its main function is to fix the starting time for the	*/
/* real time counter returned by the times() call.  "!REPORT" causes	*/
/* the contents of the structure array are printed out on the screen	*/
/* in an understandable format.  "!SYSTEM" causes the grand totals	*/
/* plus date and program id information to be written to an external	*/
/* file for subsequent detailed tracking of system usage (NYI).		*/
/*									*/
/* This whole system is less flexible and comprehensive than		*/
/* full-function profiling, but simple, easy to use, and informative	*/
/*									*/
/*	Inputs:		string		Identifies program segment	*/
/*					executed since last call	*/
/*					Can have special values (see	*/
/*					above)				*/
/*									*/
/*	Output:		return value	previous segment name or 0(BAD) */
/*									*/
/* Created 12 January 1994 by CJL					*/
/* output changed to previous segment gbc 4 Nov 2011                    */
/*									*/
/************************************************************************/
#include <unistd.h>
#include <string.h>
#include <sys/times.h>
#include "account.h"
#include "mk4_util.h"

char *
account (char *segment_name)
    {
    static struct time_account t_acc[MAX_PSEGS];
    static int prior = MAX_PSEGS - 1;	/* aka "Other" */
    static int current_real, start_real = 0, nsegment = 0;
    static int current_system = 0, current_user = 0, first = TRUE;
    static double time_unit;
    struct tms buf;
    int i, new_real, real, ret, begin, oprior;
    double elapsed_real, elapsed_user, elapsed_system;

    ret = 0;
					/* First, get the time */
    new_real = times (&buf);
    if (new_real == -1) return (0);
					/* Now decide who to charge it to */
					/* First, check for special keywords */
					/* Speed is important here, so we'll */
					/* test the first char instead of a */
					/* more expensive strcmp() operation. */
					/* This will produce gains if the */
					/* comparisons usually fail */
    begin = FALSE;
    if (segment_name[0] == '!')
	{
	if (strcmp (segment_name+1, "BEGIN") == 0) begin = TRUE;
	else if (strcmp (segment_name+1, "REPORT") == 0) 
	    {
	    if (start_real == 0) real = -1;
	    else real = new_real - start_real;
	    ret = report_times (t_acc, nsegment, &buf, real, time_unit);
	    return (ret ? 0 : t_acc[prior].segment_name);
	    }
					/* System-level accounting NYI */
	else if (strcmp (segment_name+1, "SYSTEM") == 0) return (0);
	}
					/* Initialize things if 1st call */
    if (first)
	{
	for (i=0; i<MAX_PSEGS; i++)
	    memset (t_acc+i, 0, sizeof (struct time_account));
	time_unit = 1.0 / (double)sysconf (_SC_CLK_TCK);
	current_real = new_real;
					/* If 1st call not !BEGIN, don't */
					/* know the real start time for */
					/* wall-clock accounting */
	if (begin) 
	    {
	    start_real = new_real;
	    current_user = buf.tms_utime;
	    current_system = buf.tms_stime;
	    i = MAX_PSEGS - 1;
	    }
	else start_real = 0;
	first = FALSE;
	}
					/* Continue with ID of segment */
    if (! begin)
	{
	for (i = 0; i < nsegment; i++)
	    {
					/* For speed as described above */
	    if (segment_name[0] != t_acc[i].segment_name[0]) continue;
	    if (strcmp (segment_name, t_acc[i].segment_name) == 0) break;
	    }
					/* Not found, add to list */
	if (i == nsegment)
	    {
					/* Too many, dump it in "other" */
	    if (nsegment == MAX_PSEGS)
		{
		i--;
		strcpy (t_acc[i].segment_name, "Other");
		t_acc[i].namlen = 5;
		}
					/* Truncate new string if necessary */
					/* Null terminated ab initio */
	    else
		{
		nsegment++;
		strncpy (t_acc[i].segment_name, segment_name, NAME_LEN - 1);
		t_acc[i].namlen = strlen (t_acc[i].segment_name);
		}
	    }
					/* Times (s) elapsed since last call */
	elapsed_real = (new_real - current_real) * time_unit;
	elapsed_user = (buf.tms_utime - current_user) * time_unit;
	elapsed_system = (buf.tms_stime - current_system) * time_unit;

					/* Add to totals for this segment */
	t_acc[i].real_time += elapsed_real;
	t_acc[i].user_time += elapsed_user;
	t_acc[i].system_time += elapsed_system;
	t_acc[i].times_called++;
	}
					/* Reset times so as not to count */
					/* time expended in this routine */
    current_real = times (&buf);
    current_user = buf.tms_utime;
    current_system = buf.tms_stime;
    oprior = prior;
    prior = i;

    return (t_acc[oprior].segment_name);
    }
