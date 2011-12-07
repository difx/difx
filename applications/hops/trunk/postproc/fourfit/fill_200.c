/************************************************************************/
/*                                                                      */
/*  Fills in a type_200 record, based on root and param information     */
/*                                                                      */
/*      Inputs:         root        scan_info struct                    */
/*                      param       lots of parameters                  */
/*                                                                      */
/*      Output:         t200        Filled in type_200 record           */
/*                                                                      */
/* Created 31 August 1999 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include "vex.h"
#include "mk4_data.h"
#include "param_struct.h"
#ifdef HAVE_CONFIG_H
#include "hops_config.h"
#endif


int
fill_200 (/* root, param, t200) */
struct scan_struct *root,
struct type_param *param,
struct type_200 *t200)
    {
    extern struct mk4_corel cdata; 
    time_t tm;
    struct tm *utc_now, *gmtime();
    int int_reftime, year, day, hour, minute, second;

    clear_200 (t200);

    #ifdef HAVE_CONFIG_H
    t200->software_rev[0] = HOPS_SVN_REV;
    #endif
    t200->expt_no = root->exper_num;
    strcpy (t200->exper_name, root->exper_name);
    strcpy (t200->scan_name, root->scan_name);
    strncpy (t200->correlator, root->correlator, 8);
    memcpy (&(t200->scantime), &(root->start_time), sizeof (struct date));
    t200->start_offset = param->start_offset;
    t200->stop_offset = param->stop_offset;
                                        /* Extract correlation date */
                                        /* from type-1 file in memory */
    if (sscanf (cdata.id->date, "%4d%3d-%2d%2d%2d", 
                    &year, &day, &hour, &minute, &second) != 5)
        msg ("Warning: unable to get correlation date from type-1 file", 2);
    else
        {
        t200->corr_date.year = year;
        t200->corr_date.day = day;
        t200->corr_date.hour = hour;
        t200->corr_date.minute = minute;
        t200->corr_date.second = second;
        }
                                        /* Get current time (redundant with */
                                        /* time in id record, no harm done) */
    tm = time (NULL);
    utc_now = gmtime (&tm);
    t200->fourfit_date.year = utc_now->tm_year + 1900;
    t200->fourfit_date.day = utc_now->tm_yday + 1;
    t200->fourfit_date.hour = utc_now->tm_hour;
    t200->fourfit_date.minute = utc_now->tm_min % 100;
    t200->fourfit_date.second = utc_now->tm_sec % 100;
                                        /* Convert fourfit reference time to */
                                        /* standard date format */
    t200->frt.year = t200->scantime.year;
    t200->frt.second = fmod ((double)param->reftime,  60.0);
    int_reftime = param->reftime;       /* In seconds */
    int_reftime /= 60;                  /* Now in minutes */
    t200->frt.minute = int_reftime % 60;
    int_reftime /= 60;                  /* Now in hours */
    t200->frt.hour = int_reftime % 24;
    t200->frt.day = int_reftime / 24 + 1; /* days start with 001 */

    return (0);
    }
