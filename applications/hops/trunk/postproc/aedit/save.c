/************************************************************************/
/*									*/
/* Simple utility to save a copy of the sort and flag status of the	*/
/* data arrays.								*/
/*									*/
/*	Inputs:		data		pointer to main arrays		*/
/*			mode		SAVE or RESTORE			*/
/*									*/
/*	Output:		data		saved or restored as requested	*/
/*									*/
/* Created 29 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdlib.h>
#include "aedata.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

int save (esum *data, int mode)
    {
    extern int rscan, cscan, fscan, tscan, qscan;
    extern int rsortstat[], csortstat[], fsortstat[],
					tsortstat[], qsortstat[];
    static int saved = FALSE;
    static int nrsaved, ncsaved, nfsaved, ntsaved, nqsaved;
    static int rsstat[10], csstat[10], fsstat[10], tsstat[10], qsstat[10];
    typedef struct 
	{
	int flag;
	int order;
	int lastorder;
	} savearray;
    static savearray *rsave, *csave, *fsave, *tsave, *qsave;
    int i;

    if (mode == SAVE)
	{
					/* Have to start with a clean slate */
	if (saved)
	    {
	    msg ("Warning, discarding copy of flag/sort information", 2);
	    free (rsave);
	    free (csave);
	    free (fsave);
	    free (tsave);
	    free (qsave);
	    }
					/* Allocate one extra in case there */
					/* are zero scans (calloc behaviour not */
					/* defined in that case) */
	rsave = (savearray *)calloc (rscan+1, sizeof (savearray));
	csave = (savearray *)calloc (cscan+1, sizeof (savearray));
	fsave = (savearray *)calloc (fscan+1, sizeof (savearray));
	tsave = (savearray *)calloc (tscan+1, sizeof (savearray));
	qsave = (savearray *)calloc (qscan+1, sizeof (savearray));
					/* Store information, very dull */
	for (i=0; i<rscan; i++)
	    {
	    rsave[i].flag = data->rdata[i].flag;
	    rsave[i].order = data->rdata[i].order;
	    rsave[i].lastorder = data->rdata[i].lastorder;
	    }
	for (i=0; i<cscan; i++)
	    {
	    csave[i].flag = data->cdata[i].flag;
	    csave[i].order = data->cdata[i].order;
	    csave[i].lastorder = data->cdata[i].lastorder;
	    }
	for (i=0; i<fscan; i++)
	    {
	    fsave[i].flag = data->fdata[i].flag;
	    fsave[i].order = data->fdata[i].order;
	    fsave[i].lastorder = data->fdata[i].lastorder;
	    }
	for (i=0; i<tscan; i++)
	    {
	    tsave[i].flag = data->tdata[i].flag;
	    tsave[i].order = data->tdata[i].order;
	    tsave[i].lastorder = data->tdata[i].lastorder;
	    }
	for (i=0; i<qscan; i++)
	    {
	    qsave[i].flag = data->qdata[i].flag;
	    qsave[i].order = data->qdata[i].order;
	    qsave[i].lastorder = data->qdata[i].lastorder;
	    }

	nrsaved = rscan;
	ncsaved = cscan;
	nfsaved = fscan;
	ntsaved = tscan;
	nqsaved = qscan;

	for (i=0; i<10; i++)
	    {
	    rsstat[i] = rsortstat[i];
	    csstat[i] = csortstat[i];
	    fsstat[i] = fsortstat[i];
	    tsstat[i] = tsortstat[i];
	    qsstat[i] = qsortstat[i];
	    }

	saved = TRUE;
	}

    else if ((mode == RESTORE) || (mode == RESTORE_NOFLAG))
	{
					/* If nothing already saved, this */
					/* is an error */
	if (! saved)
	    {
	    msg ("Error in save() ... no information to restore!", 2);
	    return (-1);
	    }
					/* Cannot restore unless arrays are same */
					/* size as when saved */
					/* Do the restore (boring again!) */
	if ((nrsaved == rscan) && (rscan > 0))
	    for (i=0; i<rscan; i++)
		{
		if (mode == RESTORE) data->rdata[i].flag = rsave[i].flag;
		data->rdata[i].order = rsave[i].order;
		data->rdata[i].lastorder = rsave[i].lastorder;
		}
	if ((ncsaved == cscan) && (cscan > 0))
	    for (i=0; i<cscan; i++)
		{
		if (mode == RESTORE) data->cdata[i].flag = csave[i].flag;
		data->cdata[i].order = csave[i].order;
		data->cdata[i].lastorder = csave[i].lastorder;
		}
	if ((nfsaved == fscan) && (fscan > 0))
	    for (i=0; i<fscan; i++)
		{
		if (mode == RESTORE) data->fdata[i].flag = fsave[i].flag;
		data->fdata[i].order = fsave[i].order;
		data->fdata[i].lastorder = fsave[i].lastorder;
		}
	if ((ntsaved == tscan) && (tscan > 0))
	    for (i=0; i<tscan; i++)
		{
		if (mode == RESTORE) data->tdata[i].flag = tsave[i].flag;
		data->tdata[i].order = tsave[i].order;
		data->tdata[i].lastorder = tsave[i].lastorder;
		}
	if ((nqsaved == qscan) && (qscan > 0))
	    for (i=0; i<qscan; i++)
		{
		if (mode == RESTORE) data->qdata[i].flag = qsave[i].flag;
		data->qdata[i].order = qsave[i].order;
		data->qdata[i].lastorder = qsave[i].lastorder;
		}
					/* Release memory and set flag */
	free (rsave);
	free (csave);
	free (fsave);
	free (tsave);
	free (qsave);

	for (i=0; i<10; i++)
	    {
	    rsortstat[i] = rsstat[i];
	    csortstat[i] = csstat[i];
	    fsortstat[i] = fsstat[i];
	    tsortstat[i] = tsstat[i];
	    qsortstat[i] = qsstat[i];
	    }

	saved = FALSE;
	}

    return (0);
    }
