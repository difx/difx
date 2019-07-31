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
#include "type_307.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_307 *
addr_307 (short version,
          void *address,
          int *size)
    {
    int i, j, malloced;
    struct type_307 *t307;
    struct type_307_v0 *t307_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T307_VERSION) t307 = (struct type_307 *)address;
    else
        {
        t307 = (struct type_307 *)malloc (sizeof (struct type_307));
        if (t307 == NULL)
            {
            msg ("Memory allocation failure in addr_307()", 2);
            return (NULL);
            }
        clear_307 (t307);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_307_v0);
        t307_v0 = (struct type_307_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t307->record_id, "307", 3);
        strncpy (t307->version_no, "00", 2);

        cp_int (t307->su, t307_v0->su);
        cp_double (t307->tot, t307_v0->tot);
        cp_double (t307->rot, t307_v0->rot);
        cp_double (t307->accum_period, t307_v0->accum_period);
        cp_int (t307->frame_count, t307_v0->frame_count);
        for (i=0; i<16; i++)
            {
            for (j=0; j<8; j++) cp_int (t307->counts[i].count[j],
                                                t307_v0->counts[i].count[j]);
            cp_int (t307->counts[i].val_count, t307_v0->counts[i].val_count);
            }

        return (t307);
        }
    else 
        {
        msg ("Unrecognized type 307 record version number %d", 2, version);
        if (malloced) free (t307);
        return (NULL);
        }
    }
