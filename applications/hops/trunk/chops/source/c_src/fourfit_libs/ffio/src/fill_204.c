/************************************************************************/
/*                                                                      */
/*  Fills in a type_204 record                                          */
/*                                                                      */
/*      Inputs:         via externs                                     */
/*                                                                      */
/*      Output:         t204        Filled in type_204 record           */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include "mk4_data.h"

int
fill_204 (
struct type_204 *t204)
    {
    extern char control_filename[], *control_string, version_no[];
    char *dummy;
    struct stat buf;
    struct tm *mod_time, *gmtime();

    clear_204 (t204);
                                        /* Insert ff_version number */
    t204->ff_version[0] = version_no[0] - '0';;
    t204->ff_version[1] = version_no[2] - '0';
                                        /* Assume this is run from a */
                                        /* standard mk4 environment */
    dummy = getenv ("ARCH");
    if (dummy != NULL) strncpy (t204->platform, dummy, 8);
    else strcpy (t204->platform, "unknown");

    strncpy (t204->control_file, control_filename, 96);
                                        /* Look up modification date */
    if (stat (control_filename, &buf) != 0)
        msg ("Failure statting control file '%s'", 1, control_filename);
    else
        {
        mod_time = gmtime (&(buf.st_mtime));
        t204->ffcf_date.year = mod_time->tm_year + 1900;
        t204->ffcf_date.day = mod_time->tm_yday + 1;
        t204->ffcf_date.hour = mod_time->tm_hour;
        t204->ffcf_date.minute = mod_time->tm_min % 100;
        t204->ffcf_date.second = mod_time->tm_sec % 100;
        }
                                        /* From parse_cmdline() */
    strncpy (t204->override, control_string, 128);
    if (strlen (control_string) > 127)
        {
        msg ("Warning, command line override string in type 204 record", 1);
        msg ("truncated at 128 characters", 1);
        t204->override[124] = t204->override[125] = t204->override[126] = '.';
        t204->override[127] = 0;
        }

    return (0);
    }
