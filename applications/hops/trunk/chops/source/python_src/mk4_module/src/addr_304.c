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
#include "type_304.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_304 *
addr_304 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_304 *t304;
    struct type_304_v0 *t304_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T304_VERSION) t304 = (struct type_304 *)address;
    else
        {
        t304 = (struct type_304 *)malloc (sizeof (struct type_304));
        if (t304 == NULL)
            {
            msg ("Memory allocation failure in addr_304()", 2);
            return (NULL);
            }
        clear_304 (t304);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_304_v0);
        t304_v0 = (struct type_304_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t304->record_id, "304", 3);
        strncpy (t304->version_no, "00", 2);
        cp_short (t304->time.year, t304_v0->time.year);
        cp_short (t304->time.day, t304_v0->time.day);
        cp_short (t304->time.hour, t304_v0->time.hour);
        cp_short (t304->time.minute, t304_v0->time.minute);
        cp_float (t304->time.second, t304_v0->time.second);
        cp_float (t304->duration, t304_v0->duration);
        for (i=0; i<32; i++)
            {
            cp_int (t304->trackstats[i].error_count, t304_v0->trackstats[i].error_count);
            cp_int (t304->trackstats[i].frames, t304_v0->trackstats[i].frames);
            cp_int (t304->trackstats[i].bad_frames, t304_v0->trackstats[i].bad_frames);
            cp_int (t304->trackstats[i].slip_sync, t304_v0->trackstats[i].slip_sync);
            cp_int (t304->trackstats[i].missing_sync, t304_v0->trackstats[i].missing_sync);
            cp_int (t304->trackstats[i].crc_error, t304_v0->trackstats[i].crc_error);
            }

        return (t304);
        }
    else 
        {
        msg ("Unrecognized type 304 record version number %d", 2, version);
        if (malloced) free (t304);
        return (NULL);
        }
    }
