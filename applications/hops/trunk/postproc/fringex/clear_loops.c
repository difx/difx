/************************************************************************/
/*									*/
/* Initializes the loop structure					*/
/*									*/
/*	Inputs:		loop		undefined state			*/
/*									*/
/*	Output:		loop		initialized			*/
/*									*/
/* Created October 17 1995 by CJL					*/
/*									*/
/************************************************************************/
#include "fringex.h"

void clear_loops (struct loops *loops)
    {
    int i;

    loops->nnsec = 1;
    for (i=0; i<MAXNSECS; i++) loops->nsecs[i] = 9999.0;
    loops->nrates = 1;
    for (i=0; i<MAXRATES; i++) loops->rates[i] = 0.0;
    loops->ndelays = 1;
    for (i=0; i<MAXDELAYS; i++) loops->delays[i] = 0.0;
    }

/*
 * eof
 */
