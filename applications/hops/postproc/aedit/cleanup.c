/************************************************************************/
/*									*/
/* At present, all this does is gracefully terminate the PGPLOT		*/
/* activities.								*/
/*									*/
/*	Inputs:		none						*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 21 April 1989 by CJL						*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "aedit.h"

int cleanup(void)
{
	extern int plot_open, psplot_open;

	if(plot_open || psplot_open) cpgend();
	return(0);
}
