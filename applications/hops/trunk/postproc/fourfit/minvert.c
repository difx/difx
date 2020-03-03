// minvert () is passed an n x n matrix a 
// and returns its inverse in ainv
// rc is non-zero if matrix a is singular
//
// initial code (adapted from internet version)  2019.9.6  rjc

#include <stdio.h>
#include <math.h>

int minvert (size_t n,              // matrix size is n x n
             double a[n][n],        // input matrix
             double ainv[n][n])     // inverse of the input matrix (returned)
    {
    int i, 
        j, 
        k, 
        rc = 0;

    double x[n][2*n], 
           ratio, 
           c;

    for (i=0; i<n; i++)             // copy a xrix into work area
        for (j=0; j<2*n; j++)
            if (j < n)
                x[i][j] = a[i][j];
            else
                x[i][j] = 0;

    for(i = 0; i < n; i++)
        for(j = n; j < 2*n; j++)
            if(i==(j-n))
                x[i][j] = 1.0;
            else
                x[i][j] = 0.0;
        
    for(i = 0; i < n; i++)
        for(j = 0; j < n; j++)
            if(i != j)
                {
                ratio = x[j][i]/x[i][i];
                for(k = 0; k < 2*n; k++)
                    x[j][k] -= ratio * x[i][k];
                }

    for(i = 0; i < n; i++)
        {
        c = x[i][i];
        for(j = n; j < 2*n; j++)
            {
            x[i][j] /= c;
            rc |= isnan (x[i][j]);
            }
        }

    for (i=0; i<n; i++)             // copy inverse into ainv
        for (j=0; j<n; j++)
            ainv[i][j] = x[i][j+n];

    return rc;
    }
