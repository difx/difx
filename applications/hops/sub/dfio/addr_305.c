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
/*      Output:         size		number of bytes read from input	*/
/*					address				*/
/*			Return value    Address of filled app structure */
/*									*/
/* Created 25 September 1995 by CJL					*/
/* Redesigned 17 September 1997 by CJL                                  */
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_305.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_305 *
addr_305 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_305 *t305;
    struct type_305_v0 *t305_v0;
					/* Create application structure, which */
					/* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T305_VERSION) t305 = (struct type_305 *)address;
    else
	{
	t305 = (struct type_305 *)malloc (sizeof (struct type_305));
	if (t305 == NULL)
	    {
	    msg ("Memory allocation failure in addr_305()", 2);
	    return (NULL);
	    }
	clear_305 (t305);
	malloced = TRUE;
	}
					/* Handle each version number */
					/* individually. */
    if (version == 0)
	{
					/* Overlay version-specific structure */
					/* noting size so we can maintain */
					/* pointer in file image */
	*size = sizeof (struct type_305_v0);
	t305_v0 = (struct type_305_v0 *)address;
					/* Copy structure elements, */
					/* with hidden byte flipping if needed */
					/* (see bytflp.h) */
	strncpy (t305->record_id, "305", 3);
	strncpy (t305->version_no, "00", 2);

/* COPY RECORD_SPECIFIC DATA HERE */

	return (t305);
	}
    else 
	{
	msg ("Unrecognized type 305 record version number %d", 2, version);
	if (malloced) free (t305);
	return (NULL);
	}
    }
