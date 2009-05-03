#ifndef __DIFX_CALC_H__
#define __DIFX_CALC_H__

#include <difxio.h>
#include "CALCServer.h"

typedef struct
{
	int order;
	int increment;	/* seconds */
	double delta;	/* (rad) step size for dtau/dl and dtau/dm */
	char calcServer[64];
	int calcProgram;
	int calcVersion;
	int allowNegDelay;
	struct getCALC_arg request;
	CLIENT *clnt;
} CalcParams;

int difxCalcInit(const DifxInput *D, CalcParams *p);
int difxCalc(DifxInput *D, CalcParams *p);

#endif
