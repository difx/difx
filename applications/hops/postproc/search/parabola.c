/************************************************************************/
/*									*/
/* Simple parabolic interpolation for three equally spaced points	*/
/* located at X positions -1, 0 and +1.  The q[] array below holds the	*/
/* polynomial coefficients.  The routine is borrowed from RJC's fourfit	*/
/* version, and simplified for the "search" application.		*/
/*									*/
/*	Inputs:		y0, y1, y2	SNR values at the three points	*/
/*									*/
/*	Output:		x_max		Fitted X coordinate of maximum	*/
/*			y_max		Fitted Y value at maximum	*/
/*									*/
/* Created 5 February 1996 by CJL					*/
/*									*/
/************************************************************************/

int 
parabola (double y0, double y1, double y2, double* x_max, double* y_max)
    {
    double q[3];
					/* This is trivial to derive, */
					/* or see rjc's 94.1.10 derivation */
    q[0] = (y0 - 2 * y1 + y2) / 2.0;
    q[1] = (y2 - y0) / 2.0;
    q[2] = y1;
					/* This should never happen if y1 */
					/* is the peak */
    if (q[0] >= 0.0) return (1);
					/* x value at maximum y, and */
					/* interpolated maximum y value */
    *x_max = -q[1] / (2.0 * q[0]);
    *y_max = q[0] * *x_max * *x_max  +  q[1] * *x_max  +  q[2];

    return (0);
    }
