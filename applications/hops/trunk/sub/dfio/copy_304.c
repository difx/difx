/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_304().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t304            application structure pointer   */
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
#include <string.h>
#include "bytflp.h"
#include "type_304.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_304 (struct type_304 *t304,
          char **ptr)
    {
    int version;
    int i;
    struct type_304_v0 *t304_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t304->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T304_VERSION) *ptr = (char *)t304;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_304_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_304()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_304()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t304_v0 = (struct type_304_v0 *) *ptr;
        strncpy (t304_v0->record_id, "304", 3);
        strncpy (t304_v0->version_no, "00", 2);
        cp_short (t304_v0->time.year, t304->time.year);
        cp_short (t304_v0->time.day, t304->time.day);
        cp_short (t304_v0->time.hour, t304->time.hour);
        cp_short (t304_v0->time.minute, t304->time.minute);
        cp_float (t304_v0->time.second, t304->time.second);
        cp_float (t304_v0->duration, t304->duration);
        for (i=0; i<64; i++)
            {
            cp_int (t304_v0->trackstats[i].error_count, t304->trackstats[i].error_count);
            cp_int (t304_v0->trackstats[i].frames, t304->trackstats[i].frames);
            cp_int (t304_v0->trackstats[i].bad_frames, t304->trackstats[i].bad_frames);
            cp_int (t304_v0->trackstats[i].slip_sync, t304->trackstats[i].slip_sync);
            cp_int (t304_v0->trackstats[i].missing_sync, t304->trackstats[i].missing_sync);
            cp_int (t304_v0->trackstats[i].crc_error, t304->trackstats[i].crc_error);
            }

        return (sizeof (struct type_304_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_304()", 2, version);
        return (-1);
        }
    }
