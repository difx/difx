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
#include "type_300.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_300 *
addr_300 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_300 *t300;
    struct type_300_v0 *t300_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T300_VERSION) t300 = (struct type_300 *)address;
    else
        {
        t300 = (struct type_300 *)malloc (sizeof (struct type_300));
        if (t300 == NULL)
            {
            msg ("Memory allocation failure in addr_300()", 2);
            return (NULL);
            }
        clear_300 (t300);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_300_v0);
        t300_v0 = (struct type_300_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t300->record_id, "300", 3);
        strncpy (t300->version_no, "00", 2);
        t300->SU_number = t300_v0->SU_number;
        t300->id = t300_v0->id;
        if (t300 != t300_v0)
            {
            strncpy (t300->intl_id, t300_v0->intl_id, 2);
            strcpy (t300->name, t300_v0->name);
            }
        cp_short (t300->model_start.year, t300_v0->model_start.year);
        cp_short (t300->model_start.day, t300_v0->model_start.day);
        cp_short (t300->model_start.hour, t300_v0->model_start.hour);
        cp_short (t300->model_start.minute, t300_v0->model_start.minute);
        cp_float (t300->model_start.second, t300_v0->model_start.second);
        cp_float (t300->model_interval, t300_v0->model_interval);
        cp_short (t300->nsplines, t300_v0->nsplines);

        return (t300);
        }
    else 
        {
        msg ("Unrecognized type 300 record version number %d", 2, version);
        if (malloced) free (t300);
        return (NULL);
        }
    }
