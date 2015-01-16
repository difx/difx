#include <stdio.h>
#include <delayderivatives.h>

/* Returns not the total delay but the difference between total at srcOffset and (0,0,0) */
double calcDelay(const DelayDerivatives *dd, const double *srcOffset)
{
	double rv;
	int i, j;

	rv = 0.0;
	for(i = 0; i < 3; ++i)
	{
		rv += dd->modelDelayDeriv[i] * srcOffset[i];
		for(j = 0; j < 3; ++j)
		{
			rv += 0.5 * srcOffset[j] * dd->modelDelayDeriv2[j][i] * srcOffset[i];
		}
	}

	return rv;
}

/* Returns derivative of calcDelay() with respect to srcOffset[index1] */
double calcDelay1(const DelayDerivatives *dd, const double *srcOffset, int index1)
{
	double rv;
	int i;

	rv = dd->modelDelayDeriv[index1];
	for(i = 0; i < 3; ++i)
	{
		rv += dd->modelDelayDeriv2[index1][i] * srcOffset[i];
	}

	return rv;
}

/* Returns second derivative of calcDelay() with respect to srcOffset[index1], srcOffset[index2] */
double calcDelay2(const DelayDerivatives *dd, const double *srcOffset, int index1, int index2)
{
	return dd->modelDelayDeriv2[index1][index2];
}

void printDelayDerivatives(const DelayDerivatives *dd)
{
	printf("  T = %10.8e\n", dd->modelDelay);
	printf("  dT/dX =  %10.8e\n", dd->modelDelayDeriv[0]);
	printf("  dT/dY =  %10.8e\n", dd->modelDelayDeriv[1]);
	printf("  dT/dZ =  %10.8e\n", dd->modelDelayDeriv[2]);
	printf("  d2T/dXdX =  %10.8e\n", dd->modelDelayDeriv2[0][0]);
	printf("  d2T/dXdY =  %10.8e\n", dd->modelDelayDeriv2[0][1]);
	printf("  d2T/dXdZ =  %10.8e\n", dd->modelDelayDeriv2[0][2]);
	printf("  d2T/dYdY =  %10.8e\n", dd->modelDelayDeriv2[1][1]);
	printf("  d2T/dYdZ =  %10.8e\n", dd->modelDelayDeriv2[1][2]);
	printf("  d2T/dZdZ =  %10.8e\n", dd->modelDelayDeriv2[2][2]);
}
