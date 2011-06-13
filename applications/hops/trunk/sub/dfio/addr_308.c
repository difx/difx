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
#include "type_308.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_308 *
addr_308 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_308 *t308;
    struct type_308_v0 *t308_v0;
					/* Create application structure, which */
					/* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T308_VERSION) t308 = (struct type_308 *)address;
    else
	{
	t308 = (struct type_308 *)malloc (sizeof (struct type_308));
	if (t308 == NULL)
	    {
	    msg ("Memory allocation failure in addr_308()", 2);
	    return (NULL);
	    }
	clear_308 (t308);
	malloced = TRUE;
	}
					/* Handle each version number */
					/* individually. */
    if (version == 0)
	{
					/* Overlay version-specific structure */
					/* noting size so we can maintain */
					/* pointer in file image */
	*size = sizeof (struct type_308_v0);
	t308_v0 = (struct type_308_v0 *)address;
					/* Copy structure elements, */
					/* with hidden byte flipping if needed */
					/* (see bytflp.h) */
	strncpy (t308->record_id, "308", 3);
	strncpy (t308->version_no, "00", 2);
	cp_short (t308->time.year, t308_v0->time.year);
	cp_short (t308->time.day, t308_v0->time.day);
	cp_short (t308->time.hour, t308_v0->time.hour);
	cp_short (t308->time.minute, t308_v0->time.minute);
	cp_float (t308->time.second, t308_v0->time.second);
	cp_float (t308->duration, t308_v0->duration);
	for (i=0; i<32; i++)
	    {
	    strcpy (t308->pcal[i].chan_id, t308_v0->pcal[i].chan_id);
	    cp_float (t308->pcal[i].real, t308_v0->pcal[i].real);
	    cp_float (t308->pcal[i].imaginary, t308_v0->pcal[i].imaginary);
	    cp_float (t308->pcal[i].frequency, t308_v0->pcal[i].frequency);
	    }

	return (t308);
	}
    else 
	{
	msg ("Unrecognized type 308 record version number %d", 2, version);
	if (malloced) free (t308);
	return (NULL);
	}
    }
