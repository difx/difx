/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_308().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t308            application structure pointer   */
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
#include "type_308.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_308 (struct type_308 *t308,
          char **ptr)
    {
    int version;
    int i;
    struct type_308_v0 *t308_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t308->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T308_VERSION) *ptr = (char *)t308;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_308_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_308()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_308()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t308_v0 = (struct type_308_v0 *) *ptr;
        strncpy (t308_v0->record_id, "308", 3);
        strncpy (t308_v0->version_no, "00", 2);
        cp_short (t308_v0->time.year, t308->time.year);
        cp_short (t308_v0->time.day, t308->time.day);
        cp_short (t308_v0->time.hour, t308->time.hour);
        cp_short (t308_v0->time.minute, t308->time.minute);
        cp_float (t308_v0->time.second, t308->time.second);
        cp_float (t308_v0->duration, t308->duration);
        for (i=0; i<32; i++)
            {
            strcpy (t308_v0->pcal[i].chan_id, t308->pcal[i].chan_id);
            cp_float (t308_v0->pcal[i].frequency, t308->pcal[i].frequency);
            cp_float (t308_v0->pcal[i].real, t308->pcal[i].real);
            cp_float (t308_v0->pcal[i].imaginary, t308->pcal[i].imaginary);
            }

        return (sizeof (struct type_308_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_308()", 2, version);
        return (-1);
        }
    }
