#ifndef __DELAY_DERIVATIVES_H__
#define __DELAY_DERIVATIVES_H__

typedef struct
{
	/* all quantities in seconds */
	double modelDelay;
	double modelDelayDeriv[3];
	double modelDelayDeriv2[3][3];
} DelayDerivatives;

/* Returns not the total delay but the difference between total at srcOffset and (0,0,0) */
double calcDelay(const DelayDerivatives *dd, const double *srcOffset);

/* Returns derivative of calcDelay() with respect to srcOffset[index1] */
double calcDelay1(const DelayDerivatives *dd, const double *srcOffset, int index1);

/* Returns second derivative of calcDelay() with respect to srcOffset[index1], srcOffset[index2] */
double calcDelay2(const DelayDerivatives *dd, const double *srcOffset, int index1, int index2);


void printDelayDerivatives(const DelayDerivatives *dd);

#endif
