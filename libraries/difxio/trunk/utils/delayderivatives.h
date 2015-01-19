#ifndef __DELAY_DERIVATIVES_H__
#define __DELAY_DERIVATIVES_H__

typedef struct
{
	double delay;			/* (s) */
	double delayDeriv[3];		/* (s/m) */
	double delayDeriv2[3][3];	/* (s/m^2) */
} DelayXYZDerivatives;

typedef struct
{
	double delay;			/* (s) */
	double delayDeriv[2];		/* (s/rad) */
	double delayDeriv2[2][2];	/* (s/rad^2) */
} DelayLMDerivatives;

/* Returns not the total delay but the difference between total at srcOffset and (0,0,0) */
double calcDelayXYZ(const DelayXYZDerivatives *dd, const double *srcOffset);

/* Returns derivative of calcDelay() with respect to srcOffset[index1] */
double calcDelayXYZ1(const DelayXYZDerivatives *dd, const double *srcOffset, int index1);

/* Returns second derivative of calcDelay() with respect to srcOffset[index1], srcOffset[index2] */
double calcDelayXYZ2(const DelayXYZDerivatives *dd, const double *srcOffset, int index1, int index2);

void printDelayXYZDerivatives(const DelayXYZDerivatives *dd);



/* Returns not the total delay but the difference between total at srcOffset and (0,0) */
double calcDelayLM(const DelayLMDerivatives *dd, const double *srcOffset);

/* Returns derivative of calcDelay() with respect to srcOffset[index1] */
double calcDelayLM1(const DelayLMDerivatives *dd, const double *srcOffset, int index1);

/* Returns second derivative of calcDelay() with respect to srcOffset[index1], srcOffset[index2] */
double calcDelayLM2(const DelayLMDerivatives *dd, const double *srcOffset, int index1, int index2);

void printDelayLMDerivatives(const DelayLMDerivatives *dd);

#endif
