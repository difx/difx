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
#include "type_204.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_204 *
addr_204 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_204 *t204;
    struct type_204_v0 *t204_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T204_VERSION) t204 = (struct type_204 *)address;
    else
        {
        t204 = (struct type_204 *)malloc (sizeof (struct type_204));
        if (t204 == NULL)
            {
            msg ("Memory allocation failure in addr_204()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t204);
        clear_204 (t204);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_204_v0);
        t204_v0 = (struct type_204_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t204->record_id, "204", 3);
        strncpy (t204->version_no, "00", 2);
        cp_short (t204->ff_version[0], t204_v0->ff_version[0]);
        cp_short (t204->ff_version[1], t204_v0->ff_version[1]);
        strncpy (t204->platform, t204_v0->platform, 8);
        strncpy (t204->control_file, t204_v0->control_file, 96);
        cp_short (t204->ffcf_date.year, t204_v0->ffcf_date.year);
        cp_short (t204->ffcf_date.day, t204_v0->ffcf_date.day);
        cp_short (t204->ffcf_date.hour, t204_v0->ffcf_date.hour);
        cp_short (t204->ffcf_date.minute, t204_v0->ffcf_date.minute);
        cp_float (t204->ffcf_date.second, t204_v0->ffcf_date.second);
        strncpy (t204->override, t204_v0->override, 128);

        return (t204);
        }
    else 
        {
        msg ("Unrecognized type 204 record version number %d", 2, version);
        if (malloced) 
            {
            free (t204);
            msg ("Freed memory block %d", -1, t204);
            }
        return (NULL);
        }
    }
