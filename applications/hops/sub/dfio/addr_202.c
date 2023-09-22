/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.  Either way, byte flipping is performed */
/* as necessary by the architecture-dependent macros cp_xxxx() defined  */
/* in bytflp.h                                                          */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         size            number of bytes read from input */
/*                                      address                         */
/*                      Return value    Address of filled app structure */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 17 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_202.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_202 *
addr_202 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_202 *t202;
    struct type_202_v0 *t202_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T202_VERSION) t202 = (struct type_202 *)address;
    else
        {
        t202 = (struct type_202 *)malloc (sizeof (struct type_202));
        if (t202 == NULL)
            {
            msg ("Memory allocation failure in addr_202()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t202);
        clear_202 (t202);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_202_v0);
        t202_v0 = (struct type_202_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t202->record_id, "202", 3);
        strncpy (t202->version_no, "00", 2);
        strncpy (t202->baseline, t202_v0->baseline, 2);
        strncpy (t202->ref_intl_id, t202_v0->ref_intl_id, 2);
        strncpy (t202->rem_intl_id, t202_v0->rem_intl_id, 2);
        strncpy (t202->ref_name, t202_v0->ref_name, 8);
        strncpy (t202->rem_name, t202_v0->rem_name, 8);
        strncpy (t202->ref_tape, t202_v0->ref_tape, 8);
        strncpy (t202->rem_tape, t202_v0->rem_tape, 8);
        cp_short (t202->nlags, t202_v0->nlags);
        cp_double (t202->ref_xpos, t202_v0->ref_xpos);
        cp_double (t202->rem_xpos, t202_v0->rem_xpos);
        cp_double (t202->ref_ypos, t202_v0->ref_ypos);
        cp_double (t202->rem_ypos, t202_v0->rem_ypos);
        cp_double (t202->ref_zpos, t202_v0->ref_zpos);
        cp_double (t202->rem_zpos, t202_v0->rem_zpos);
        cp_double (t202->u, t202_v0->u);
        cp_double (t202->v, t202_v0->v);
        cp_double (t202->uf, t202_v0->uf);
        cp_double (t202->vf, t202_v0->vf);
        cp_float (t202->ref_clock, t202_v0->ref_clock);
        cp_float (t202->rem_clock, t202_v0->rem_clock);
        cp_float (t202->ref_clockrate, t202_v0->ref_clockrate);
        cp_float (t202->rem_clockrate, t202_v0->rem_clockrate);
        cp_float (t202->ref_idelay, t202_v0->ref_idelay);
        cp_float (t202->rem_idelay, t202_v0->rem_idelay);
        cp_float (t202->ref_zdelay, t202_v0->ref_zdelay);
        cp_float (t202->rem_zdelay, t202_v0->rem_zdelay);
        cp_float (t202->ref_elev, t202_v0->ref_elev);
        cp_float (t202->rem_elev, t202_v0->rem_elev);
        cp_float (t202->ref_az, t202_v0->ref_az);
        cp_float (t202->rem_az, t202_v0->rem_az);

        return (t202);
        }
    else 
        {
        msg ("Unrecognized type 202 record version number %d", 2, version);
        if (malloced) 
            {
            free (t202);
            msg ("Freed memory block %d", -1, t202);
            }
        return (NULL);
        }
    }
