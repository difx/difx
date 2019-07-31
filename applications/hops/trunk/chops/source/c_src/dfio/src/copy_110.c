/************************************************************************/
/*									*/
/* This is the inverse of addr_110().  It takes an application		*/
/* structure and a target address, and places an overlay structure	*/
/* of the appropriate version into the target address.  Sometimes this	*/
/* will require a field-by-field copying operation, sometimes it will	*/
/* be a simple pointer assignment operation, depending on version 	*/
/* control status							*/
/*									*/
/*	Inputs:		t110		application structure pointer	*/
/*									*/
/*	Output:		ptr		overlay structure address	*/
/*					with data filled in		*/
/*			return value	number of bytes filled in	*/
/*									*/
/* Created 3 January 1997 by CJL					*/
/* Modified 13 March 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_110.h"
#include "mk4_dfio.h"

int
copy_110 (struct type_110 *t110,
          char **ptr)
    {
    int version;
    int i, j, nblocks, size;
    struct type_110_v0 *t110_v0;
					/* What version is requested for */
					/* the disk format? */
    sscanf (t110->version_no, "%2d", &version);
					/* Used for size calculations */
    nblocks = t110->nblocks;
					/* Disk format same as app struct, */
					/* simple pointer assignment. */
    if (version == T110_VERSION) *ptr = (char *)t110;
    else if (version == 0)
	{
	size = sizeof (struct type_110_v0) + sizeof (raw_data) * (nblocks-1);
	*ptr = (char *)malloc (size);
	if (*ptr == NULL)
	    {
	    msg ("Memory allocation failure in copy_110()", 2);
	    return (-1);
	    }
	}
    else
	{
	msg ("Unrecognized version number %d in copy_110()", 2, version);
	return (-1);
	}
					/* Handle each version number */
					/* individually. */
    if (version == 0)
	{
	t110_v0 = (struct type_110_v0 *) *ptr;
	strncpy (t110_v0->record_id, "110", 3);
	strncpy (t110_v0->version_no, "00", 2);
	cp_short (t110_v0->nblocks, t110->nblocks);
	strncpy (t110_v0->baseline, t110->baseline, 2);
	cp_short (t110_v0->filenum, t110->filenum);
	strncpy (t110_v0->rootcode, t110->rootcode, 6);
	cp_int (t110_v0->index, t110->index);
	cp_int (t110_v0->ap, t110->ap);
	cp_int (t110_v0->flag, t110->flag);
	cp_int (t110_v0->status, t110->status);
	cp_float (t110_v0->bitshift, t110->bitshift);
	cp_float (t110_v0->fbit, t110->fbit);
	for (i=0; i<nblocks; i++)
	    for (j=0; j<33; j++)
		{
		cp_int (t110_v0->data[i].r_cell[j], t110->data[i].r_cell[j]);
		cp_int (t110_v0->data[i].l_cell[j], t110->data[i].l_cell[j]);
		}
	size = sizeof (struct type_110_v0) + sizeof (raw_data) * (nblocks-1);
	return (size);
	}
    else
	{
	msg ("Unrecognized version number %d in copy_110()", 2, version);
	return (-1);
	}
    }
