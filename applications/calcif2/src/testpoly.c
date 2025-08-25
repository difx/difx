#include <stdio.h>
#include <math.h>
#include "poly.h"

int main()
{
	const int N=4;
	const int M=5;
	const int NSAMP=(N-1)*M+1;
	double dt = 1.0;
	double samples[NSAMP];
	double in[N];
	double out[N];
	double delta;
	int i, j;

	in[0] = 1.0;
	in[1] = 0.5;
	in[2] = 0.25;
	in[3] = 0.125;

	delta = dt/(NSAMP-1.0);

	for(i = 0; i < NSAMP; ++i)
	{
		double a=0.0;
		double t=i*delta;

		for(j = 0; j < N; ++j)
		{
			a *= t;
			a += in[N-1-j];
		}

		samples[i] = a;

		printf("%f -> %f\n", t, samples[i]);
	}

	fitPoly(out, samples, N, M, delta);

	for(i = 0; i < N; ++i)
	{
		printf("%d   %f %f\n", i, in[i], out[i]);
	}

	return 0;
}
