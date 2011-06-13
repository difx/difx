/************************************************************************/
/*									*/
/* This routine prints the contents of the three summary structures	*/
/* in a nice format. It assumes summ_data() has been called		*/
/*									*/
/*	Inputs:		none						*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 9 April 1989 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include "summary.h"

int
pr_allsumm()
    {
    extern struct datasumm rsumm, csumm, fsumm, tsumm, qsumm;
    extern int rscan, cscan, fscan, tscan, qscan;
    extern int rflag, cflag, fflag, tflag, qflag;
    int i;

    if(rscan == 0) msg("No type 0 (root) data",2);
    if(cscan == 0) msg("No type 1 (corel) data",2);
    if(fscan == 0) msg("No type 2 (fringe) data",2);
    if(tscan == 0) msg("No type 3 (triangle) data",2);
    if(qscan == 0) msg("No type 4 (quad) data",2);

    printf("\n\n\t\tSUMMARY OF UNFLAGGED DATA IN MEMORY\n");
    printf("\t\t-----------------------------------\n\n");
    if (rscan > 0)
	{
	printf("Root records:\n-------------\n\n");
	printf("Total/flagged/unflagged root records = %d/%d/%d\n",
	    rscan, rflag, rscan-rflag);
	printf("Stations present:\t%s\n",rsumm.stations);
	if(rsumm.nexp < 15)
	    {
	    printf("Experiments present:");
	    for(i=0;i<rsumm.nexp;i++) printf("%5d",rsumm.experiments[i]);
	    printf("\n");
	    }
	else printf("Experiments present: more than 15\n");
	printf("\n\n");
	}
    if (cscan > 0)
	{
	printf("Corel records:\n-------------\n\n");
	printf("Total/flagged/unflagged corel records = %d/%d/%d\n",
	    cscan, cflag, cscan-cflag);
	printf("Stations present:\t%s\n",csumm.stations);
	if(csumm.nexp < 15)
	    {
	    printf("Experiments present:");
	    for(i=0;i<csumm.nexp;i++) printf("%5d",csumm.experiments[i]);
	    printf("\n");
	    }
	else printf("Experiments present: more than 15\n");
	printf("\n\n");
	}
    if (fscan > 0)
	{
	printf("Fringe records:\n-------------\n\n");
	printf("Total/flagged/unflagged fringe records = %d/%d/%d\n",
	    fscan, fflag, fscan-fflag);
	printf("Stations present:\t%s\n",fsumm.stations);
	if(fsumm.nexp < 15)
	    {
	    printf("Experiments present:");
	    for(i=0;i<fsumm.nexp;i++) printf("%5d",fsumm.experiments[i]);
	    printf("\n");
	    }
	else printf("Experiments present: more than 15\n");
	printf("\n\n");
	}
    if (tscan > 0)
	{
	printf("Triangle records:\n-------------\n\n");
	printf("Total/flagged/unflagged triangle records = %d/%d/%d\n",
	    tscan, tflag, tscan-tflag);
	printf("Stations present:\t%s\n",tsumm.stations);
	if(tsumm.nexp < 15)
	    {
	    printf("Experiments present:");
	    for(i=0;i<tsumm.nexp;i++) printf("%5d",tsumm.experiments[i]);
	    printf("\n");
	    }
	else printf("Experiments present: more than 15\n");
	printf("\n\n");
	}
    if (qscan > 0)
	{
	printf("Quad records:\n-------------\n\n");
	printf("Total/flagged/unflagged quad records = %d/%d/%d\n",
	    qscan, qflag, qscan-qflag);
	printf("Stations present:\t%s\n",qsumm.stations);
	if(qsumm.nexp < 15)
	    {
	    printf("Experiments present:");
	    for(i=0;i<qsumm.nexp;i++) printf("%5d",qsumm.experiments[i]);
	    printf("\n");
	    }
	else printf("Experiments present: more than 15\n");
	printf("\n\n");
	}
    return(0);
    }
