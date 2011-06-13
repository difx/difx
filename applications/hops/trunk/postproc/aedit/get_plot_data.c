/************************************************************************/
/*									*/
/* This routine extracts plottable information from the main data array	*/
/* for a single frequency/experiment/source/baseline, as specified in	*/
/* the input arguments fqex, sptr and bas.				*/
/* It fills the pp data arrays for later passage to pgplot		*/
/* routines, and maintains a semi-permanent record of which data is	*/
/* plotted where (the index array in the pdata structure).  Where	*/
/* appropriate, it also calculates error bars sizes based on the snr.	*/
/* It also determines various quantities used in the actual plotting.	*/
/* The end product is a filled plot_info structure (see pstruct.h)	*/
/* ready for setup_plot() and the PGPLOT routines pg_point() etc. 	*/
/*									*/
/*	Inputs:		data		main data structure pointer	*/
/*			pd		Plot information structure ptr	*/
/*			fqex		freq/expt summary structure	*/
/*			sptr		Pointer to srcsum struct in fqex*/
/*			bas		Baseline specification		*/
/*									*/
/*	Output:		pp		Pointer to plot data arrays	*/
/*									*/
/* Created 17 April 1989 by CJL						*/
/* Completely redesigned and overhauled 25 February 1992 by CJL 	*/
/* Modified to support improved y axis selection mechanism		*/
/* 7 September 1993 by CJL						*/
/* Added full closure support, 30 August 1994 CJL			*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>		
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "flags.h"
#include "summary.h"

#define NOSPLIT 0
#define SPLIT 1

int
get_plot_data (data, pd, fqex, sptr, plot_id, pp, symbol)
esum *data;
struct plot_info *pd;
srcsum *sptr;
struct frqexp fqex;
char *plot_id;
struct plot_points *pp;
int symbol[3];
    {
    extern struct inputs inp;
    extern struct plot_info pdata[];
    extern int fscan, tscan, qscan;
    struct plot_ptqual *pt;
    float xhigh, xlow, yhigh, ylow;
    int i, offset, nbadmbd, nloop, discard;
    int *npt;
    fringesum *fdatum;
    trianglesum *tdatum;
    quadsum *qdatum;
    char sym;

					/* Can errors be calculated? */
    if ((inp.xaind == AX_SNR) || (inp.xaind == AX_AMPLITUDE)
	|| (inp.xaind == AX_PHASE) || (inp.xaind == AX_CPHASE)
	|| (inp.xaind == AX_CAMP)) pd->xebar = TRUE;
    if ((inp.yaind == AX_SNR) || (inp.yaind == AX_AMPLITUDE)
	|| (inp.yaind == AX_PHASE) || (inp.yaind == AX_CPHASE)
	|| (inp.yaind == AX_CAMP)) pd->yebar = TRUE;
					/* Note that initialization of pd is */
					/* handled by calling routine */
					/* (plot_fqex() or plot_source()) */
    if (pd->npts != (pd->ngood + pd->nsusp + pd->nbad))	/* Current location */
	{
	msg ("Error in get_plot_data ... counters out of step!",2);
	return (-1);
	}
					/* Need to loop over all data, whether */
					/* for stations/baselines/all (fringe */
					/* array) or triangles/quads */
    if (inp.plotby == TRIANGLE_PLOT) nloop = tscan;
    else if (inp.plotby == QUAD_PLOT) nloop = qscan;
    else nloop = fscan;
					/* For station-based plots, need to */
					/* reduce to one datum per time tag */
					/* to avoid plot overflows */
    if (inp.plotby == STATION_PLOT)
	station_reduce (data->fdata, plot_id[0], sptr->name, 
				fqex.expt_no, fqex.freq_code);
			 		/* Loop over all data */
    nbadmbd = 0;
    for (i=0; i<nloop; i++)
	{
					/* Point to data for this point */
	fdatum = &(data->fdata[i].data);
	tdatum = &(data->tdata[i].data);
	qdatum = &(data->qdata[i].data);
					/* Skip over non-matching data */
	if ((inp.plotby == STATION_PLOT) || (inp.plotby == BASELINE_PLOT)
		|| (inp.plotby == ALL_PLOT))
	    {
	    if (data->fdata[i].flag != 0) continue;
					/* Skip if source/freq/exp are wrong */
	    if (strcmp (fdatum->source, sptr->name) != 0) continue;
	    if (fdatum->expt_no != fqex.expt_no) continue;
	    if (fdatum->freq_code != fqex.freq_code) continue;
	    }
	else if (inp.plotby == TRIANGLE_PLOT)
	    {
	    if (data->tdata[i].flag != 0) continue;
					/* Skip if source/freq/exp are wrong */
	    if (strcmp (tdatum->source, sptr->name) != 0) continue;
	    if (tdatum->expt_no != fqex.expt_no) continue;
	    if (tdatum->freq_code != fqex.freq_code) continue;
	    }
	else if (inp.plotby == QUAD_PLOT)
	    {
	    if (data->qdata[i].flag != 0) continue;
					/* Skip if source/freq/exp are wrong */
	    if (strcmp (qdatum->source, sptr->name) != 0) continue;
	    if (qdatum->expt_no != fqex.expt_no) continue;
	    if (qdatum->freq_code != fqex.freq_code) continue;
	    }
					/* Is this datum destined for this plot? */
	discard = TRUE;
	switch (inp.plotby)
	    {
	    case STATION_PLOT:
		if (strchr (fdatum->baseline, plot_id[0]) != NULL) discard = FALSE;
		break;
	    case BASELINE_PLOT:
		if (strcmp (fdatum->baseline, plot_id) == 0) discard = FALSE;
		break;
	    case TRIANGLE_PLOT:
		if (strcmp (tdatum->triangle, plot_id) == 0) discard = FALSE;
		break;
	    case QUAD_PLOT:
		if (strcmp (qdatum->quad, plot_id) == 0) discard = FALSE;
		break;
	    default:
		discard = FALSE;
	    }
	if (discard) continue;
					/* Ignore single channel points for mbdelay */
					/* Also, if ambiguity unknown, should skip */
	if ((inp.yaind == AX_MBDELAY) || (inp.xaind == AX_MBDELAY))
	    if ((fdatum->no_freq == 1) || (fdatum->ambiguity == 0.0))
		{
		nbadmbd++;
		continue;
		}
					/* Overflow ... just jump out */
	if (pd->npts >= MAXPLT)
	    {
	    msg("Plot id %s, freq %c ... too many", 2, plot_id, fqex.freq_code);
	    msg("data points to plot ... truncate at %d", 2, MAXPLT);
	    break;
	    }
					/* Establish general quality level */
					/* and point to corresponding part */
					/* of plot data structures */
	switch (plot_quality (fdatum, tdatum))
	    {
	    case PQ_BAD:
		pt = &(pp->bad);
		npt = &(pd->nbad);
		sym = symbol[2];
		break;
	    case PQ_SUSPECT:
		pt = &(pp->suspect);
		npt = &(pd->nsusp);
		sym = symbol[1];
		break;
	    case PQ_GOOD:
		pt = &(pp->good);
		npt = &(pd->ngood);
		sym = symbol[0];
		break;
	    default:
		;
	    }
					/* get_plot_datum() fills in the next */
					/* element in the pt plot data arrays */
					/* It must be fed both closure and normal */
					/* datum information to do its job */
					/* so we need main data array */
	get_plot_datum (plot_id, data, sptr, i, *npt, pd->toffset, pt);

					/* Outside user limits, don't plot */
	if ((pt->x[*npt] < inp.xscale[0] || pt->x[*npt] > inp.xscale[1]) 
	    && (inp.xscale[0] != inp.xscale[1])) pd->nbadscale++;
	else if ((pt->y[*npt] < inp.yscale[0] || pt->y[*npt] > inp.yscale[1]) 
	    && (inp.yscale[0] != inp.yscale[1])) pd->nbadscale++;
	else 
	    {				/* Record index, and plot symbol */
	    pd->index[pd->npts] = i;
	    pd->symbol[pd->npts] = sym;
					/* Axis extrema */
					/* If error bars present, make sure */
					/* plot big enough to accommodate them */
	    if (pd->xebar)
		{
		xhigh = pt->xerrh[*npt];
		xlow = pt->xerrl[*npt];
		}
	    else xhigh = xlow = pt->x[*npt];
	    if (pd->yebar)
		{
		yhigh = pt->yerrh[*npt];
		ylow = pt->yerrl[*npt];
		}
	    else yhigh = ylow = pt->y[*npt];
					/* Init */
	    if (pd->npts == 0) 
		{
		pd->xmax = xhigh;
		pd->xmin = xlow;
		pd->ymax = yhigh;
		pd->ymin = ylow;
		}

	    if (xhigh > pd->xmax) pd->xmax = xhigh;
	    if (xlow < pd->xmin) pd->xmin = xlow;
	    if (yhigh > pd->ymax) pd->ymax = yhigh;
	    if (ylow < pd->ymin) pd->ymin = ylow;
	    *npt += 1;			/* Update point count */
	    (pd->npts)++;			/* Total point count */
	    }
	}	/* End of main loop */
					/* Undo station-based trimming */
					/* done in station_reduce() */
    if (inp.plotby == STATION_PLOT)
	for (i=0; i<fscan; i++) data->fdata[i].flag &= ~(TAGGED);
					/* Fill in pdata stuff now */
    pd->frq = fqex.freq_code;
    pd->xaind = inp.xaind;
    pd->x_aux = inp.x_aux;
    if (strlen (inp.x_units) > 0)
	sprintf (pd->xtype, "%s (%s)", inp.x_axis, inp.x_units);
    else strcpy (pd->xtype, inp.x_axis);
    pd->yaind = inp.yaind;
    pd->y_aux = inp.y_aux;
    if (strlen (inp.y_units) > 0)
	sprintf (pd->ytype, "%s (%s)", inp.y_axis, inp.y_units);
    else strcpy (pd->ytype, inp.y_axis);

    pd->plotby = inp.plotby;
    if (inp.plotby == STATION_PLOT) pd->station = plot_id[0];
    else if (inp.plotby == BASELINE_PLOT) strcpy (pd->bas, plot_id);
    else if (inp.plotby == TRIANGLE_PLOT) strcpy (pd->triangle, plot_id);
    else if (inp.plotby == QUAD_PLOT) strcpy (pd->ampcl, plot_id);

    if (strlen (pd->source) == 0) 
	strcpy (pd->source, sptr->name);
    else strcpy (pd->source, "all sources");	/* Must be NOSPLIT mode */
					/* Report mbdelay problems, if any */
    if (nbadmbd > 0) 
	msg ("Ignored %d 1-channel/unknown ambig. pts in mbd plot %s, %c-band, source %s", 2,
				nbadmbd, plot_id, pd->frq, sptr->name);
    return(0);
    }
