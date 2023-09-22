/************************************************************************/
/*									*/
/* Produces a nice formatted summary of execution times gathered by	*/
/* the account() routine.						*/
/*									*/
/*	Inputs:		t_acc		Array of structures with times	*/
/*			nseg		# of program segments in t_acc	*/
/*			buf		tms struct from times() call	*/
/*			real		elapsed real time to this point	*/
/*			time_unit	10 millisec on HP735		*/
/*									*/
/*	Output:		Screen output summary of times used		*/
/*									*/
/* Created January 13 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <sys/times.h>
#include <string.h>
#include "account.h"
#include "math.h"
#include "mk4_util.h"

static double wall_clock = 0.0;

void report_wallclock(int npass, int totpass)
    {
    msg ("Wallclock %g on %d passes %d total", 3, wall_clock, npass, totpass);
    msg ("Estimate %g seconds for total processing time", 3,
        (double)(totpass) * wall_clock / (double)npass);
    }

int report_times(struct time_account *t_acc, int nseg, struct tms *buf,
    int real, double time_unit)
    {
    size_t max_len, i, j, total_calls, len;
    char line[101];
    double total_real, total_user, total_system;
    double acc_user, acc_system, acc_real;
    static int called = FALSE;

    wall_clock = 0.0;
					/* Should be called only once */
    if (called)
	{
	msg ("Tried to do accounting report multiple times", 3);
	return (1);
	}
					/* Find longest string */
    max_len = 15;   /* title */
    for (i = 0; i < nseg; i++)
	if (t_acc[i].namlen > max_len) max_len = t_acc[i].namlen;
					/* Do title */
    msg ("", 3);
    msg ("Time usage summary (all times are in seconds)", 3);
    for (j=0; j<max_len+4+39; j++) line[j] = '-';
    line[max_len+4+39] = '\0';
    msg ("%s", 3, line);
    for (j=0; j<100; j++) line[j] = ' ';
    line[100] = '\0';
    strncpy (line, "Program segment", 15);
    strcpy (line+max_len+4, "  UserCPU SystemCPU ClockTime    #Calls");
    msg ("%s", 3, line);
					/* Now loop over all segments */
					/* printing out statistics as we go */
    total_real = 0.0; total_user = 0.0; total_system = 0.0;
    total_calls = 0;
    i = 0;
    for (i = 0; i < nseg; i++)
	{
	len = strlen (line);
	for (j=0; j<len; j++) line[j] = ' ';
	strncpy (line, t_acc[i].segment_name, t_acc[i].namlen);
	sprintf (line+max_len+4, "%9.3f %9.3f %9.3f %9d",
	    t_acc[i].user_time, t_acc[i].system_time,
	    t_acc[i].real_time, t_acc[i].times_called);
	msg ("%s", 3, line);
					/* Accum. totals excluding account */
	total_real += t_acc[i].real_time;
	total_user += t_acc[i].user_time;
	total_system += t_acc[i].system_time;
	total_calls += t_acc[i].times_called;
	}
					/* Account must have used the rest */
    acc_user = buf->tms_utime * time_unit - total_user;
    acc_system = buf->tms_stime * time_unit - total_system;
    acc_real = real * time_unit - total_real;
    for (j=0; j<strlen(line); j++) line[j] = ' ';
    strncpy (line, "Accounting", 10);
    sprintf (line+max_len+4, "%9.3f %9.3f %9.3f %9ld",
            fabs(acc_user), fabs(acc_system), fabs(acc_real), total_calls);
    if (real < 0) strncpy (line+max_len+28, "       ???", 10);
    msg ("%s", 3, line);
					/* Grand totals, use system-reported */
					/* vals to avoid cumulative rounding */
					/* errors in total_*** above */
    len = strlen (line);
    for (j=0; j<len; j++) line[j] = '-';
    msg ("%s", 3, line);
    for (j=0; j<len; j++) line[j] = ' ';
    strncpy (line, "Totals", 6);
    wall_clock = (real * time_unit);
    sprintf (line+max_len+4, "%9.3f %9.3f %9.3f",
            buf->tms_utime * time_unit, buf->tms_stime * time_unit, wall_clock);
    msg ("%s", 3, line);
    msg ("", 3);

    called = TRUE;
    return (0);
    }
