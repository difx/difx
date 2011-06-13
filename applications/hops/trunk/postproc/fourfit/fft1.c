/**********************************************************/
/* first FFT program, based on FOUR1 in Numerical Recipes */
/*                                                        */
/*       Complex data of length NN stored in In[]         */
/*       Frequency output in Out[]                        */
/*       DC in 0th complex element                        */
/*       smallest +ve freq in element 1                   */
/*       smallest -ve freq in element NN-1                */
/*       ISign = +1 for FFT , = -1 for inverse FFT        */
/*                      - cmn                             */    
/* increased size of Data to 8*MAXMAX+1, since fft1       */
/* called with nn=4*nlags, and data is twice as large     */       
/*                                          rjc 2007.5.24 */
/**********************************************************/

#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "type_comp.h"

FFT1 (In, NN, ISign,Out,rev)    
complex In[MAXMAX*2], Out[MAXMAX*2];
int NN,ISign,rev;
    {
    double wr,wi,wpr,wpi,wtemp,theta,tempr,tempi,Data[MAXMAX*8 + 1];
    int i,j,n,istep,mmax,m;

                        /* Sort complex data into 1-dimen. array of length 2*NN */
    for (i=0;i<=NN-1;i++) 
        {
        Data[2*i+1] = In[i].re;
        Data[2*i+2] = In[i].im; 
        }

    n = 2*NN;
    j = 1;

    for (i=1;i<=n;i+=2)     /*  Re-arrange data by bit-reversal */
        {
        if (j>i) 
            {
            tempr = Data[j];
            tempi = Data[j+1];
            Data[j] = Data[i];
            Data[j+1] = Data[i+1];
            Data[i] = tempr;
            Data[i+1] = tempi;
            }
        m = n/2;
        while ((m>=2) && (j>m)) 
            {
            j -= m;
            m /= 2;
            }
        j+=m;
        }

    mmax=2;                     /* Danielson-Lanczos section, repeat logN times */
    while (n>mmax) 
        {
        istep = 2*mmax;
        theta = 6.28318530717959/(ISign*mmax);
        wtemp = sin(0.5*theta);
        wpr = (-2.0 * wtemp * wtemp) ;
        wpi = sin(theta);
        wr = 1.;
        wi = 0.;
        for (m=1;m<=mmax;m+=2)
            {
            for (i=m;i<=n;i+=istep) 
                {
                j = i+mmax;
                tempr = wr*Data[j] - wi*Data[j+1];
                tempi = wr*Data[j+1] +wi*Data[j];
                Data[j] = Data[i] -tempr;
                Data[j+1] = Data[i+1]-tempi;
                Data[i] += tempr;
                Data[i+1] += tempi;
                }
            wtemp = wr;
            wr = wr*wpr - wi*wpi + wr;
            wi = wi*wpr + wtemp*wpi + wi;
            }
        mmax = istep;
        }
    Out[0].re = Data[1];
    Out[0].im = Data[2];
    for (i=1;i<=NN-1;i++)
        {
        if (rev==1)
           {
           j=2*(NN-i) + 1;
           }
        else
           {j=2*i + 1;
           }
        Out[i].re = Data[j];
        Out[i].im = Data[j+1];
        }
    }
