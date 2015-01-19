#include <stdio.h>
#include <delayderivatives.h>

/* Returns not the total delay but the difference between total at srcOffset and (0,0,0) */
/* srcOffset[] is source offset from nominal position in (m) */
double calcDelayXYZ(const DelayXYZDerivatives *dd, const double *srcOffset)
{
	double rv; /* (s) */
	int i, j;

	rv = 0.0;
	for(i = 0; i < 3; ++i)
	{
		rv += dd->delayDeriv[i] * srcOffset[i];
		for(j = 0; j < 3; ++j)
		{
			rv += 0.5 * srcOffset[j] * dd->delayDeriv2[j][i] * srcOffset[i];
		}
	}

	return rv;
}

/* Returns derivative of calcDelayXYZ() with respect to srcOffset[index1] */
/* srcOffset[] is source offset from nominal position in (m) */
/* index1 is 0,1,2 for X,Y,Z coordinates respectively */
double calcDelayXYZ1(const DelayXYZDerivatives *dd, const double *srcOffset, int index1)
{
	double rv;	/* (s/m) */
	int i;

	rv = dd->delayDeriv[index1];
	for(i = 0; i < 3; ++i)
	{
		rv += dd->delayDeriv2[index1][i] * srcOffset[i];
	}

	return rv;
}

/* Returns second derivative of calcDelayXYZ() with respect to srcOffset[index1], srcOffset[index2] */
/* srcOffset[] is source offset from nominal position in (m)  (unused in this instance) */
/* index1 and index2 are 0,1,2 for X,Y,Z coordinates respectively */
double calcDelayXYZ2(const DelayXYZDerivatives *dd, const double *srcOffset, int index1, int index2)
{
	return dd->delayDeriv2[index1][index2];	/* (s/m^2) */
}

void printDelayXYZDerivatives(const DelayXYZDerivatives *dd)
{
	printf("  T        = %10.8e s\n", dd->delay);
	printf("  dT/dX    = %10.8e s/m\n", dd->delayDeriv[0]);
	printf("  dT/dY    = %10.8e s/m\n", dd->delayDeriv[1]);
	printf("  dT/dZ    = %10.8e s/m\n", dd->delayDeriv[2]);
	printf("  d2T/dXdX = %10.8e s/m^2\n", dd->delayDeriv2[0][0]);
	printf("  d2T/dXdY = %10.8e s/m^2\n", dd->delayDeriv2[0][1]);
	printf("  d2T/dXdZ = %10.8e s/m^2\n", dd->delayDeriv2[0][2]);
	printf("  d2T/dYdY = %10.8e s/m^2\n", dd->delayDeriv2[1][1]);
	printf("  d2T/dYdZ = %10.8e s/m^2\n", dd->delayDeriv2[1][2]);
	printf("  d2T/dZdZ = %10.8e s/m^2\n", dd->delayDeriv2[2][2]);
}



/* Returns not the total delay but the difference between total at srcOffset and (0,0,0) */
/* srcOffset[] is source offset from nominal position in (rad) */
double calcDelayLM(const DelayLMDerivatives *dd, const double *srcOffset)
{
	double rv; /* (s) */
	int i, j;

	rv = 0.0;
	for(i = 0; i < 2; ++i)
	{
		rv += dd->delayDeriv[i] * srcOffset[i];
		for(j = 0; j < 2; ++j)
		{
			rv += 0.5 * srcOffset[j] * dd->delayDeriv2[j][i] * srcOffset[i];
		}
	}

	return rv;
}
/* Returns derivative of calcDelayLM() with respect to srcOffset[index1] */
/* srcOffset[] is source offset from nominal position in (m) */
/* index1 is 0,1,2 for X,Y,Z coordinates respectively */
double calcDelayLM1(const DelayLMDerivatives *dd, const double *srcOffset, int index1)
{
	double rv;	/* (s/rad) */
	int i;

	rv = dd->delayDeriv[index1];
	for(i = 0; i < 2; ++i)
	{
		rv += dd->delayDeriv2[index1][i] * srcOffset[i];
	}

	return rv;
}

/* Returns second derivative of calcDelayLM() with respect to srcOffset[index1], srcOffset[index2] */
/* srcOffset is source offset from nominal position in (rad)  (unused in this instance) */
/* index1 and index2 are 0,1 for l,m coordinates respectively */
double calcDelayLM2(const DelayLMDerivatives *dd, const double *srcOffset, int index1, int index2)
{
	return dd->delayDeriv2[index1][index2];	/* (s/rad^2) */
}

void printDelayLMDerivatives(const DelayLMDerivatives *dd)
{
	printf("  T        = %10.8e s\n", dd->delay);
	printf("  dT/dl    = %10.8e s/rad\n", dd->delayDeriv[0]);
	printf("  dT/dm    = %10.8e s/rad\n", dd->delayDeriv[1]);
	printf("  d2T/dldl = %10.8e s/rad^2\n", dd->delayDeriv2[0][0]);
	printf("  d2T/dldm = %10.8e s/rad^2\n", dd->delayDeriv2[0][1]);
	printf("  d2T/dmdm = %10.8e s/rad^2\n", dd->delayDeriv2[1][1]);
}
