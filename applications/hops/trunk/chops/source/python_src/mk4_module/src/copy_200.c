/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_200().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t200            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_200.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_200 (struct type_200 *t200,
          char **ptr)
    {
    int version;
    int i;
    struct type_200_v0 *t200_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t200->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T200_VERSION) *ptr = (char *)t200;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_200_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_200()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_200()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t200_v0 = (struct type_200_v0 *) *ptr;
        strncpy (t200_v0->record_id, "200", 3);
        strncpy (t200_v0->version_no, "00", 2);
        for (i=0; i<10; i++)
            {  /* note - following macro line is actually 2 stmts. */
            cp_short (t200_v0->software_rev[i], t200->software_rev[i]);
            }
        cp_int (t200_v0->expt_no, t200->expt_no);
        strncpy (t200_v0->exper_name, t200->exper_name, 32);
        strncpy (t200_v0->scan_name, t200->scan_name, 32);
        strncpy (t200_v0->correlator, t200->correlator, 8);
        cp_short (t200_v0->scantime.year, t200->scantime.year);
        cp_short (t200_v0->scantime.day, t200->scantime.day);
        cp_short (t200_v0->scantime.hour, t200->scantime.hour);
        cp_short (t200_v0->scantime.minute, t200->scantime.minute);
        cp_float (t200_v0->scantime.second, t200->scantime.second);
        cp_int (t200_v0->start_offset, t200->start_offset);
        cp_int (t200_v0->stop_offset, t200->stop_offset);
        cp_short (t200_v0->corr_date.year, t200->corr_date.year);
        cp_short (t200_v0->corr_date.day, t200->corr_date.day);
        cp_short (t200_v0->corr_date.hour, t200->corr_date.hour);
        cp_short (t200_v0->corr_date.minute, t200->corr_date.minute);
        cp_float (t200_v0->corr_date.second, t200->corr_date.second);
        cp_short (t200_v0->fourfit_date.year, t200->fourfit_date.year);
        cp_short (t200_v0->fourfit_date.day, t200->fourfit_date.day);
        cp_short (t200_v0->fourfit_date.hour, t200->fourfit_date.hour);
        cp_short (t200_v0->fourfit_date.minute, t200->fourfit_date.minute);
        cp_float (t200_v0->fourfit_date.second, t200->fourfit_date.second);
        cp_short (t200_v0->frt.year, t200->frt.year);
        cp_short (t200_v0->frt.day, t200->frt.day);
        cp_short (t200_v0->frt.hour, t200->frt.hour);
        cp_short (t200_v0->frt.minute, t200->frt.minute);
        cp_float (t200_v0->frt.second, t200->frt.second);

        return (sizeof (struct type_200_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_200()", 2, version);
        return (-1);
        }
    }
