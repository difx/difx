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
/* Created 8 March 2000 by CJL                                          */
/* created version 1     2010.1.5      rjc                              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_210.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_210 *
addr_210 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_210 *t210;
    struct type_210_v0 *t210_v0;
    struct type_210_v1 *t210_v1;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T210_VERSION) t210 = (struct type_210 *)address;
    else
        {
        t210 = (struct type_210 *)malloc (sizeof (struct type_210));
        if (t210 == NULL)
            {
            msg ("Memory allocation failure in addr_210()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t210);
        clear_210 (t210);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_210_v0);
        t210_v0 = (struct type_210_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t210->record_id, "210", 3);
        strncpy (t210->version_no, "00", 2);
        for (i=0; i<16; i++)
            {
            cp_float (t210->amp_phas[i].ampl, t210_v0->amp_phas[i].ampl);
            cp_float (t210->amp_phas[i].phase, t210_v0->amp_phas[i].phase);
            }
        return (t210);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_210_v1);
        t210_v1 = (struct type_210_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t210_v1->record_id, "210", 3);
        strncpy (t210_v1->version_no, "01", 2);
        for (i=0; i<64; i++)
            {
            cp_float (t210->amp_phas[i].ampl, t210_v1->amp_phas[i].ampl);
            cp_float (t210->amp_phas[i].phase, t210_v1->amp_phas[i].phase);
            }
        return (t210);
        }
    else 
        {
        msg ("Unrecognized type 210 record version number %d", 2, version);
        if (malloced) 
            {
            free (t210);
            msg ("Freed memory block %d", -1, t210);
            }
        return (NULL);
        }
    }
