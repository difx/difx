#include <stdio.h>
#include <difxio/difx_input.h>
#include "model.h"
#include "poly.h"

int getModelIndex(const DifxPolyModel *im, int imSize, int mjd, double sec)
{
	int nSec;	// [s] seconds since start of first polynomial
	int index;

	nSec = (mjd - im[0].mjd)*86400 + (int)sec - im[0].sec;
	index = nSec/im[0].validDuration;

	if(index < 0 || index >= imSize)
	{
		return -1;
	}
	else
	{
		return index;
	}
}

/* return value in microseconds, positive if arrival at station before earth center
 * tInt [s] is the integration time (FFT length), used for bias correction
 */
double getDelay(const DifxPolyModel *im, int imSize, int mjd, double sec, double tInt)
{
	double dt;	/* [s] */
	double bias;	/* [us] */
	int index;

	index = getModelIndex(im, imSize, mjd, sec);
	if(index < 0)
	{
		fprintf(stderr, "Error: getDelay %d %f: out time out of range\n", mjd, sec);

		return 0.0;
	}
	
	dt = (mjd - im[index].mjd)*86400 + sec - im[index].sec;

	/* bias corrects for the second derivative of delay w.r.t. time.  Typical correction is at ps level for tInt=1 sec */
	bias = im[index].delay[2]*tInt*tInt/12.0;

	return evaluatePoly(im[index].delay, im[index].order+1, dt) + bias;
}

/* return value in microseconds/sec, time derivative of getDelay() */
double getRate(const DifxPolyModel *im, int imSize, int mjd, double sec)
{
	double dt;	/* [s] */
	int index;

	index = getModelIndex(im, imSize, mjd, sec);
	if(index < 0)
	{
		fprintf(stderr, "Error: getRate %d %f: out time out of range\n", mjd, sec);

		return 0.0;
	}
	
	dt = (mjd - im[index].mjd)*86400 + sec - im[index].sec;

	return evaluatePolyDeriv(im[index].delay, im[index].order+1, dt);
}

