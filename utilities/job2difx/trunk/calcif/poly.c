#include <stdio.h>
#include <stdlib.h>

#define MAX_MODEL_ORDER 10

/* Implement a specific variant of Neville's algorithm for equally spaced
 * data.  The expansion is about x=0 and the input data points are 
 * expected to be at x = d*i for i = [0, n) 
 */
void computePoly(double *p, int n, double d)
{
	long double C[MAX_MODEL_ORDER+1][MAX_MODEL_ORDER+1];
	int g, h, i;

	for(h = 0; h < n; h++)
	{
		C[h][0] = p[h];
	}

	for(g = 1; g < n; g++)
	{
		for(h = 0; h < n-g; h++)
		{
			C[h][g] = 0.0;
			for(i = g-1; i >= 0; i--)
			{
				C[h][i+1] += (C[h+1][i] - C[h][i])/(g*d);
				C[h][i]    = (C[h][i]*(h+g) - C[h+1][i]*h)/g;
			}
		}
	}

	for(i = 0; i < n; i++)
	{
		p[i] = C[0][i];
	}
}

int main(int argc, char **argv)
{
	double p[10], d[10];
	int i, j, n=0;
	double delta = 1.0, x, xx;

	for(i = 1; i < argc; i++)
	{
		p[i-1] = atof(argv[i]);
		n++;
	}

	for(i = 0; i < n; i++)
	{
		d[i] = 0;
		x = i*delta;
		xx = 1.0;
		for(j = 0; j < n; j++)
		{
			d[i] += p[j]*xx;
			xx *= x;
		}
	}

	for(i = 0; i < n; i++)
	{
		printf("%d %f %f\n", i, p[i], d[i]);
	}

	printf("\n");

	computePoly(d, n, delta);

	for(i = 0; i < n; i++)
	{
		printf("%d %f %f %e\n", i, p[i], d[i], p[i]-d[i]);
	}

	return 0;
}
