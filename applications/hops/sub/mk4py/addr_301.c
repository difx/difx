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
#include "type_301.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_301 *
addr_301 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_301 *t301;
    struct type_301_v0 *t301_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T301_VERSION) t301 = (struct type_301 *)address;
    else
        {
        t301 = (struct type_301 *)malloc (sizeof (struct type_301));
        if (t301 == NULL)
            {
            msg ("Memory allocation failure in addr_301()", 2);
            return (NULL);
            }
        clear_301 (t301);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_301_v0);
        t301_v0 = (struct type_301_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t301->record_id, "301", 3);
        strncpy (t301->version_no, "00", 2);
        cp_short (t301->interval, t301_v0->interval);
        if (t301 != t301_v0)
            strncpy (t301->chan_id, t301_v0->chan_id , 32);
        for (i=0; i<6; i++)
            {
            cp_double (t301->delay_spline[i], t301_v0->delay_spline[i]);
            }

        return (t301);
        }
    else 
        {
        msg ("Unrecognized type 301 record version number %d", 2, version);
        if (malloced) free (t301);
        return (NULL);
        }
    }
