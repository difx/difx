/*    Basic functions for complex variables   */
/*                  6/19/91 cmn     	     	  */

#include <math.h>
#include <stdio.h>
#include "type_comp.h"

double c_mag(z)				/* Magnitude of complex number */
complex z;
    {
/*    if ((z.re ==0.) && (z.im == 0.))
        printf("ZERO\n");  */
    return (sqrt(z.re * z.re + z.im * z.im));
    }
	

double c_phase(z) 			/* Phase of complex number */
complex z;
    {
    return (atan2(z.im,z.re));
    }


complex c_add(a,b)			/* Add two complex numbers */
complex a,b;
    {
    complex z;
    z.re = a.re + b.re;
    z.im = a.im + b.im;
    return(z);
    }


complex c_sub(a,b)			/* Subtract two complex numbers */
complex a,b;
    {
    complex z;
    z.re = a.re - b.re;
    z.im = a.im - b.im;
    return(z);
    }


complex c_mult(a,b)			/* Multiply 2 complex numbers */
complex a,b;

    {
    complex z;
    z.re = a.re*b.re - a.im*b.im;
    z.im = a.im*b.re + a.re*b.im;
    return(z);
    }

complex rect(mag,phase)		/* Reconstruct complex number from phase and magnitude */
double mag,phase;
    {
    complex z;
    z.re = mag * cos(phase);
    z.im = mag * sin(phase);
    return(z);
    }

complex conju(z)
complex z;
    {
    complex c;
    c.re = z.re;
    c.im = z.im * -1.;
    return(c);
    }

complex c_exp(theta)		/*  Complex exponential --> computes exp(i * theta)  */
double theta;
    {
    complex z;
    z.re = cos(theta);
    z.im = sin(theta);
    return(z);
    }

complex c_zero()
    {
    complex z;
    z.re=0.;
    z.im=0.;
    return(z);
    }

complex s_mult(z,i)
complex z;
double i;
    {
    complex c;
    c.re=z.re * i;
    c.im=z.im * i;
    return(c);
    }

c_print(z)
complex z;
    {
    if ((z.re==0.) && (z.im==0.)) printf("ZERO\n");
    else printf("%lf + %lf i ; %lf , %lf rad\n",z.re,z.im,c_mag(z),c_phase(z));
    fflush(stdout);
    }

complex c_mean (complex *z, int n) // returns mean of n complex numbers, z
    {
    int i;
    complex sum;
    complex c_zero (), c_add();

    sum = c_zero ();
    for (i=0; i<n; i++)
        sum = c_add (sum, *(z+i));
    sum = s_mult (sum, 1.0 / n);
    return (sum);
    }
