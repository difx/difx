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
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_201.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_201 *
addr_201 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_201 *t201;
    struct type_201_v0 *t201_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T201_VERSION) t201 = (struct type_201 *)address;
    else
        {
        t201 = (struct type_201 *)malloc (sizeof (struct type_201));
        if (t201 == NULL)
            {
            msg ("Memory allocation failure in addr_201()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t201);
        clear_201 (t201);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_201_v0);
        t201_v0 = (struct type_201_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t201->record_id, "201", 3);
        strncpy (t201->version_no, "00", 2);
        strncpy (t201->source, t201_v0->source, 32);
        cp_short (t201->coord.ra_hrs, t201_v0->coord.ra_hrs);
        cp_short (t201->coord.ra_mins, t201_v0->coord.ra_mins);
        cp_float (t201->coord.ra_secs, t201_v0->coord.ra_secs);
        cp_short (t201->coord.dec_degs, t201_v0->coord.dec_degs);
        cp_short (t201->coord.dec_mins, t201_v0->coord.dec_mins);
        cp_float (t201->coord.dec_secs, t201_v0->coord.dec_secs);
        cp_short (t201->coord_date.year, t201_v0->coord_date.year);
        cp_short (t201->coord_date.day, t201_v0->coord_date.day);
        cp_short (t201->coord_date.hour, t201_v0->coord_date.hour);
        cp_short (t201->coord_date.minute, t201_v0->coord_date.minute);
        cp_float (t201->coord_date.second, t201_v0->coord_date.second);
        cp_short (t201->epoch, t201_v0->epoch);
        cp_double (t201->ra_rate, t201_v0->ra_rate);
        cp_double (t201->dec_rate, t201_v0->dec_rate);
        for (i=0; i<4; i++)
            cp_double (t201->pulsar_phase[i], t201_v0->pulsar_phase[i]);
        cp_double (t201->pulsar_epoch, t201_v0->pulsar_epoch);
        cp_double (t201->dispersion, t201_v0->dispersion);

        return (t201);
        }
    else 
        {
        msg ("Unrecognized type 201 record version number %d", 2, version);
        if (malloced) 
            {
            free (t201);
            msg ("Freed memory block %d", -1, t201);
            }
        return (NULL);
        }
    }
