/************************************************************************/
/*									*/
/* Standard record version control.  This routine returns the address	*/
/* of a structure containing the desired record information.  This can	*/
/* either be the address of the raw memory image of the disk record	*/
/* that was read in, or a memory-allocated structure filled in element	*/
/* by element, depending on whether or not the disk format and the	*/
/* structure definitions match.						*/
/*									*/
/*	Inputs:		version		Version number of disk image	*/
/*			address		Memory address of disk image	*/
/*			size		True size of structure (bytes)	*/
/*									*/
/*	Output:		Return value	Address of filled app structure	*/
/*									*/
/* Created 23 January 1997 by CJL					*/
/* Revised 13 March 1998 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_110.h"
#include "mk4_dfio.h"

#define FALSE 0
#define TRUE 1

struct type_110 *
addr_110 (short version,
          void *address,
          int *size)
    {
    int malloced, size_110, i, j;
    short *dummy, nblocks;
    struct type_110 *t110;
    struct type_110_v0 *t110_v0;
					/* Need number of blocks up front */
    dummy = (short *)address;
    cp_short (nblocks, dummy[3]);
					/* Create application structure, which */
					/* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T110_VERSION) t110 = (struct type_110 *)address;
    else
	{
	size_110 = sizeof (struct type_110) + sizeof (raw_data) * (nblocks-1);
	t110 = (struct type_110 *) malloc (size_110);
	if (t110 == NULL)
	    {
	    msg ("Memory allocation failure in addr_110()", 2);
	    return (NULL);
	    }
	clear_110 (t110);
	malloced = TRUE;
	}
					/* Handle each version number */
					/* individually.  First overlay */
					/* a version-specific structure, */
					/* then copy structure elements */
    if (version == 0)
	{
					/* Must calculate true structure size */
					/* since sizeof() doesn't know we are */
					/* tricking compiler with variable numbers */
					/* of raw data blocks */
	*size = sizeof (struct type_110_v0) 
			+ sizeof (raw_data) * (nblocks - 1);
	t110_v0 = (struct type_110_v0 *)address;
					/* Start copying structure elements, */
					/* with hidden byte flipping if needed */
					/* (see bytflp.h) */
	strncpy (t110->record_id, "110", 3);
	strncpy (t110->version_no, "00", 2);
	cp_short (t110->nblocks, t110_v0->nblocks);
	strncpy (t110->baseline, t110_v0->baseline, 2);
	cp_short (t110->filenum, t110_v0->filenum);
	strncpy (t110->rootcode, t110_v0->rootcode, 6);
	cp_int (t110->index, t110_v0->index);
	cp_int (t110->ap, t110_v0->ap);
	cp_int (t110->flag, t110_v0->flag);
	cp_int (t110->status, t110_v0->status);
	cp_float (t110->bitshift, t110_v0->bitshift);
	cp_float (t110->fbit, t110_v0->fbit);
	for (i=0; i<nblocks; i++)
	    for (j=0; j<33; j++)
		{
		cp_int (t110->data[i].r_cell[j], t110_v0->data[i].r_cell[j]);
		cp_int (t110->data[i].l_cell[j], t110_v0->data[i].l_cell[j]);
		}
	return (t110);
	}
    else
	{
	msg ("Unrecognized type 110 record version number %d", 2, version);
	if (malloced) free (t110);
	return (NULL);
	}
    }
