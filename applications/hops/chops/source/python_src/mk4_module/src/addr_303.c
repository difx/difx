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
/* Used for az, el, and pa           rjc   2012.2.21                    */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_303.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_303 *
addr_303 (short version,
          void *address,
          int *size)
    {
    int malloced,
        i;
    struct type_303 *t303;
    struct type_303_v0 *t303_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T303_VERSION) 
        t303 = (struct type_303 *)address;
    else
        {
        t303 = (struct type_303 *)malloc (sizeof (struct type_303));
        if (t303 == NULL)
            {
            msg ("Memory allocation failure in addr_303()", 2);
            return (NULL);
            }
        clear_303 (t303);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_303_v0);
        t303_v0 = (struct type_303_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t303->record_id, "303", 3);
        strncpy (t303->version_no, "00", 2);
        cp_short (t303->interval, t303->interval);

        if (t303 != t303)
            strncpy (t303->chan_id, t303->chan_id , 32);
        for (i=0; i<6; i++)
            {
            cp_double (t303->azimuth[i], t303->azimuth[i]);
            cp_double (t303->elevation[i], t303->elevation[i]);
            cp_double (t303->parallactic_angle[i], t303->parallactic_angle[i]);
            cp_double (t303->u[i], t303->u[i]);
            cp_double (t303->v[i], t303->v[i]);
            cp_double (t303->w[i], t303->w[i]);
            }

        return (t303);
        }
    else 
        {
        msg ("Unrecognized type 303 record version number %d", 2, version);
        if (malloced) free (t303);
        return (NULL);
        }
    }
