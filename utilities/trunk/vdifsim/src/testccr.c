#include <stdio.h>
#include <complex.h>
#include <math.h>
#include <fftw3.h>

int main(int argc, char **argv)
{
	const int N = 48;
	int i;

	double *b1;
	double complex *b2;
	double complex *b3;
	double *b4;

	fftw_plan c2cPlan;
	fftw_plan c2rPlan;

	b1 = (double *)fftw_malloc(N*sizeof(double));
	b2 = (double complex *)fftw_malloc(N*sizeof(double complex));
	b3 = (double complex *)fftw_malloc(N*sizeof(double complex));
	b4 = (double *)fftw_malloc(N*sizeof(double));

	c2cPlan = fftw_plan_dft_1d(N, b2, b3, FFTW_FORWARD, FFTW_ESTIMATE | FFTW_PRESERVE_INPUT);
	c2rPlan = fftw_plan_dft_c2r_1d(N, b3, b4, FFTW_ESTIMATE | FFTW_PRESERVE_INPUT);

	for(i = 0; i < N; ++i)
	{
		b1[i] = 0.0;
	}
	b1[12] = 1.0;
	b1[20] = 0.5;
	b1[21] = 0.5;

	for(i = 0; i < N; ++i)
	{
		double phi;

		phi = 0.0;

		b2[i] = b1[i]*cos(phi) + I * b1[i]*sin(phi);
	}

	fftw_execute(c2cPlan);

	for(i = 0; i < N; ++i)
	{
		double phi;

		phi = 2*M_PI*i/(double)N;

		b3[i] *= (cos(phi) + I * sin(phi));
	}

	fftw_execute(c2rPlan);

	for(i = 0; i < N; ++i)
	{
		b4[i] /= N;
	}

	for(i = 0; i < N; ++i)
	{
		printf("%d  %f  %f %f  %f %f  %f\n", i, b1[i], creal(b2[i]), cimag(b2[i]), creal(b3[i]), cimag(b3[i]), b4[i]);
	}


	return 0;
}
