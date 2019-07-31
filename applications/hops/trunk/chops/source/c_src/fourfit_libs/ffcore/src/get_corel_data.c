/************************************************************************/
/*                                                                      */
/* This routine is responsible for reading the relevant corel file into */
/* the main data array.  In Mk4 we are mandating that all data for a    */
/* given fringe fit must be placed in a single corel file, which is a   */
/* significant simplification from Mk3.  It first checks that the       */
/* combination of scan, baseline frequency group and source are         */
/* actually needed.                                                     */
/*                                                                      */
/*      Inputs:         fs              fstruct entry for current       */
/*                                      baseline of interest            */
/*                      ovex            Parsed ovex portion of root,    */
/*                                      with full pathname of root file */
/*                                                                      */
/*      Output:         cdata           mk4 corel memory structure,     */
/*                                      filled in with data             */
/*                      return value    0 = OK, 1 = problem             */
/*                                                                      */
/* Created 15 March 1993 by CJL                                         */
/* Added support for -u option, CJL 3 Jan 1994                          */
/* Modified for Mk4 17 February 1997 by CJL                             */
/* Modified to also use Mk4 root 31 March 1998 by CJL                   */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "mk4_data.h"
#include "vex.h"
#include "control.h"
#include "fstruct.h"

int
get_corel_data (fstruct* fs, struct scan_struct* ovex, char* filename, struct mk4_corel* cdata)
    {
    int scantime;
    char corelname[256], scandir[256];
    char *ptr, *strrchr();
                                        /* Strip off source.rootcode from */
                                        /* rootname, and leave directory */
                                        /* only in scandir */
    strcpy (scandir, filename);
    ptr = strrchr (scandir, '/');
    *ptr = '\0';
                                        /* Integer format for scantime from */
                                        /* UTIL library.  Make it relative */
                                        /* to start of year. */
    scantime = time_to_int (0, (int) ovex->start_time.day,
                               (int) ovex->start_time.hour,
                               (int) ovex->start_time.minute,
                               (int) ovex->start_time.second);

                                        /* If control information excludes all */
                                        /* such data, no point reading it in */
    if (skip_data (scantime, fs->baseline, ovex->src.source_name, WILDCARD))
        {
        msg ("data rejected for baseline %s and source %s",
                                0, fs->baseline, ovex->src.source_name);
        return (1);
        }
    msg ("data accepted for baseline %s and source %s",
                                1, fs->baseline, ovex->src.source_name);

                                        /* All that remains is to read in */
                                        /* the corel file */
    sprintf (corelname, "%s/%s", scandir, fs->name);
    if (read_mk4corel (corelname, cdata) != 0)
        {
        msg ("Error reading corel file '%s'", 2, corelname);
        return (-1);
        }

    return (0);
    }
