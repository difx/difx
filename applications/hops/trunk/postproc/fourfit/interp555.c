// interp555 - interpolates a gridded-function to a specific point within a 5x5x5 cube
// 
// created - rjc 2009.10.20


void interp555 (double drf[5][5][5],// input: real function
                double xi[3],       // input: coordinates to be evaluated at
                double *drfval)     // output: interpolated value
    {
    int i,
        j,
        k;

    double a[5][3],
           p,
           p2;
    
    for (j=0; j<3; j++)
        {
        p = xi[j];
        p2 = p * p;
                                    // Lagrange interpolating polynomials based
                                    // on Abramowitz & Stegun's 25.2.15
        a[0][j] = (p2-1) * p * (p-2) / 24;
        a[1][j] = -(p-1) * p * (p2-4) / 6;
        a[2][j] = (p2-1) * (p2-4) / 4;
        a[3][j] = -(p+1) * p * (p2-4) / 6;
        a[4][j] = (p2-1) * p * (p+2) / 24;
        //msg ("p %lf a[][%d] %lf %lf %lf %lf %lf",2,
        //        p, j, a[0][j], a[1][j], a[2][j], a[3][j], a[4][j]);
        }

    *drfval = 0.0;

    for (i=0; i<5; i++)
        for (j=0; j<5; j++)
            for (k=0; k<5; k++)
                {
                *drfval += a[i][0] * a[j][1] * a[k][2] * drf[i][j][k];
                //msg ("drf[%d][%d][%d] %lf drfval %lf",2, i,j,k, drf[i][j][k],*drfval);
                }
    }
