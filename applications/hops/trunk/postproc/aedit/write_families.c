/************************************************************************/
/*									*/
/* Special sort mode, writes out data by root family to simulate the	*/
/* way "true A-files" used to be written out on the A-900.		*/
/*									*/
/*	Inputs:		data		Can't do much without this!	*/
/*			fp		Open output file descriptor	*/
/*									*/
/*	Output:		return value	0=OK, non-0 = bad		*/
/*									*/
/* Created March 7 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedit.h"
#include "aedata.h"
#include "sort.h"

int
write_families (data, fp)
esum *data;
FILE *fp;
    {
    extern struct inputs inp;
    extern int fscan, cscan, rscan, output_version;
    extern int data_version;
    int ret, i, ri, ci, fi, rindex, cindex, findex, keyval, version;
    int ckey, fkey, nr, nc, nf;
    fringearray *fdata;
    corelarray *cdata;
    rootarray *rdata;
					/* Convenience */
    rdata = data->rdata;
    cdata = data->cdata;
    fdata = data->fdata;
					/* Remake key values to be sure */
    ret = makekey (rdata, S_ROOTCODE, 0);
    ret += makekey (cdata, S_ROOTCODE, 1);
    ret += makekey (fdata, S_ROOTCODE, 2);
    if (ret != 0)
	{
	msg ("Error making keys in write_families()", 2);
	return (-1);
	}
					/* Do main loop over roots */
    ci = fi = 0;
    nr = nc = nf = 0;
    for (ri=0; ri<rscan; ri++)
	{
					/* First figure out the root keyvalue */
	rindex = rdata[ri].order;
	keyval = rdata[rindex].keyval;
					/* Data are sorted, so just need to */
					/* write until rootcode value is */
					/* reached */
	for (i=ci; i<cscan; i++)
	    {
	    cindex = cdata[i].order;
	    if (cdata[cindex].flag) continue;

	    ckey = cdata[cindex].keyval;
	    if (ckey >= keyval) break;

	    version = cdata[cindex].data.version;
	    if (output_version > 0) cdata[cindex].data.version = output_version;
	    ret += write_csumm (&(cdata[cindex].data), fp);
	    if (ret == 0) nc++;
	    cdata[cindex].data.version = version;
	    }
	ci = i;

	for (i=fi; i<fscan; i++)
	    {
	    findex = fdata[i].order;
	    if (fdata[findex].flag) continue;

	    fkey = fdata[findex].keyval;
	    if (fkey >= keyval) break;

	    version = fdata[findex].data.version;
	    if (output_version > 0) fdata[findex].data.version = output_version;
	    ret += write_fsumm (&(fdata[findex].data), fp);
	    if (ret == 0) nf++;
	    fdata[findex].data.version = version;
	    }
	fi = i;
					/* Write out the root record */
	if (rdata[rindex].flag) continue;
	version = rdata[rindex].data.version;
	if (output_version > 0) rdata[rindex].data.version = output_version;
	ret += write_rsumm (&(rdata[rindex].data), fp);
	if (ret == 0) nr++;
	rdata[rindex].data.version = version;
	}
					/* Tidy up leftovers */
    for (i=ci; i<cscan; i++)
	{
	cindex = cdata[i].order;
	if (cdata[cindex].flag) continue;

	version = cdata[cindex].data.version;
	if (output_version > 0) cdata[cindex].data.version = output_version;
	ret += write_csumm (&(cdata[cindex].data), fp);
	if (ret == 0) nc++;
	cdata[cindex].data.version = version;
	}
    for (i=fi; i<fscan; i++)
	{
	findex = fdata[i].order;
	if (fdata[findex].flag) continue;

	version = fdata[findex].data.version;
	if (output_version > 0) fdata[findex].data.version = output_version;
	ret += write_fsumm (&(fdata[findex].data), fp);
	if (ret == 0) nf++;
	fdata[findex].data.version = version;
	}
					/* Report errors */
    if (ret != 0)
	msg ("Warning: errors ocurred while writing out the data", 2);

    if (nf > 0) msg ("Wrote %d fringe records successfully", 2, nf);
    if (nc > 0) msg ("Wrote %d corel records successfully", 2, nc);
    if (nr > 0) msg ("Wrote %d root records successfully", 2, nr);

    return (ret);
    }
