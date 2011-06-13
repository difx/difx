/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_202().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t202            application structure pointer   */
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
#include "type_202.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_202 (struct type_202 *t202,
          char **ptr)
    {
    int version;
    struct type_202_v0 *t202_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t202->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T202_VERSION) *ptr = (char *)t202;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_202_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_202()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_202()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t202_v0 = (struct type_202_v0 *) *ptr;
        strncpy (t202_v0->record_id, "202", 3);
        strncpy (t202_v0->version_no, "00", 2);
        strncpy (t202_v0->baseline, t202->baseline, 2);
        strncpy (t202_v0->ref_intl_id, t202->ref_intl_id, 2);
        strncpy (t202_v0->rem_intl_id, t202->rem_intl_id, 2);
        strncpy (t202_v0->ref_name, t202->ref_name, 8);
        strncpy (t202_v0->rem_name, t202->rem_name, 8);
        strncpy (t202_v0->ref_tape, t202->ref_tape, 8);
        strncpy (t202_v0->rem_tape, t202->rem_tape, 8);
        cp_short (t202_v0->nlags, t202->nlags);
        cp_double (t202_v0->ref_xpos, t202->ref_xpos);
        cp_double (t202_v0->rem_xpos, t202->rem_xpos);
        cp_double (t202_v0->ref_ypos, t202->ref_ypos);
        cp_double (t202_v0->rem_ypos, t202->rem_ypos);
        cp_double (t202_v0->ref_zpos, t202->ref_zpos);
        cp_double (t202_v0->rem_zpos, t202->rem_zpos);
        cp_double (t202_v0->u, t202->u);
        cp_double (t202_v0->v, t202->v);
        cp_double (t202_v0->uf, t202->uf);
        cp_double (t202_v0->vf, t202->vf);
        cp_float (t202_v0->ref_clock, t202->ref_clock);
        cp_float (t202_v0->rem_clock, t202->rem_clock);
        cp_float (t202_v0->ref_clockrate, t202->ref_clockrate);
        cp_float (t202_v0->rem_clockrate, t202->rem_clockrate);
        cp_float (t202_v0->ref_idelay, t202->ref_idelay);
        cp_float (t202_v0->rem_idelay, t202->rem_idelay);
        cp_float (t202_v0->ref_zdelay, t202->ref_zdelay);
        cp_float (t202_v0->rem_zdelay, t202->rem_zdelay);
        cp_float (t202_v0->ref_elev, t202->ref_elev);
        cp_float (t202_v0->rem_elev, t202->rem_elev);
        cp_float (t202_v0->ref_az, t202->ref_az);
        cp_float (t202_v0->rem_az, t202->rem_az);

        return (sizeof (struct type_202_v0));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_202()", 2, version);
        return (-1);
        }
    }
