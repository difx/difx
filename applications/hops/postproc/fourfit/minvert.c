// minvert () is passed an n x n matrix a 
// and returns its inverse in ainv
// rc is non-zero if matrix a is singular
//
// initial code (adapted from internet version)  2019.9.6  rjc

#include <stdio.h>
#include <math.h>
#include "ffmath.h"

int minvert3( double a[3][3],        // input matrix
             double ainv[3][3])     // inverse of the input matrix (returned)
    {
    int i, 
        j, 
        k, 
        rc = 0;

    double x[3][2*3], 
           ratio, 
           c;

    for (i=0; i<3; i++)             // copy a xrix into work area
        for (j=0; j<2*3; j++)
            if (j < 3)
                x[i][j] = a[i][j];
            else
                x[i][j] = 0;

    for(i = 0; i < 3; i++)
        for(j = 3; j < 2*3; j++)
            if(i==(j-3))
                x[i][j] = 1.0;
            else
                x[i][j] = 0.0;
        
    for(i = 0; i < 3; i++)
        for(j = 0; j < 3; j++)
            if(i != j)
                {
                ratio = x[j][i]/x[i][i];
                for(k = 0; k < 2*3; k++)
                    x[j][k] -= ratio * x[i][k];
                }

    for(i = 0; i < 3; i++)
        {
        c = x[i][i];
        for(j = 3; j < 2*3; j++)
            {
            x[i][j] /= c;
            rc |= isnan (x[i][j]);
            }
        }

    for (i=0; i<3; i++)             // copy inverse into ainv
        for (j=0; j<3; j++)
            ainv[i][j] = x[i][j+3];

    return rc;
    }


int minvert5( double a[5][5],        // input matrix
                 double ainv[5][5])     // inverse of the input matrix (returned)
        {
        int i, 
            j, 
            k, 
            rc = 0;

        double x[5][2*5], 
               ratio, 
               c;

        for (i=0; i<5; i++)             // copy a xrix into work area
            for (j=0; j<2*5; j++)
                if (j < 5)
                    x[i][j] = a[i][j];
                else
                    x[i][j] = 0;

        for(i = 0; i < 5; i++)
            for(j = 5; j < 2*5; j++)
                if(i==(j-5))
                    x[i][j] = 1.0;
                else
                    x[i][j] = 0.0;
            
        for(i = 0; i < 5; i++)
            for(j = 0; j < 5; j++)
                if(i != j)
                    {
                    ratio = x[j][i]/x[i][i];
                    for(k = 0; k < 2*5; k++)
                        x[j][k] -= ratio * x[i][k];
                    }

        for(i = 0; i < 5; i++)
            {
            c = x[i][i];
            for(j = 5; j < 2*5; j++)
                {
                x[i][j] /= c;
                rc |= isnan (x[i][j]);
                }
            }

        for (i=0; i<5; i++)             // copy inverse into ainv
            for (j=0; j<5; j++)
                ainv[i][j] = x[i][j+5];

        return rc;
        }



