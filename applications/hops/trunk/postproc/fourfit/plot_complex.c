/****************************************/
/*                                      */
/* plot_complex takes a pointer to      */
/* an array of complex points, a character, and */
/* alternate character to plot in,      */
/* a screen area, then segments the data */
/* and coherently averages the data,    */
/* which is plotted using grid.         */
/* Amp_phase_flag determines whether    */
/* amplitude or phase is plotted.       */
/* (0=Amp, 1 = phase)                   */
/* Segment_flag determines whether data */
/* needs to be segmented.               */
/****************************************/
#include <stdio.h>
#include <math.h>
#include <complex.h>

void 
plot_complex (char** matrix, int x, int y, int width, int height, 
                double scale_x, double scale_y, int winstart, int winstop,
                        complex* points, double* weights, int numpoints, char ch, char altch, int segment, int amp_phase_flag) 
    {
    int index, seg, nlsb, nusb, start, stop;
    char symbol;
    double seg_len,data,wt, wtsum;
    complex segsum;

    if (segment)
        {
        seg_len = (scale_x / (double)width);
        if (seg_len < 1) seg_len = 1.;
        for(seg = 0; seg < width; seg++)
            {
            segsum = 0.0;
            wtsum=0; nlsb=0; nusb=0;
            start = (int)(seg * seg_len);
            stop = (int)((seg + 1.0) * seg_len);
                                        /* Accumulate vector sum, weights, */
                                        /* and usb/lsb counts */
            for (index = start; index < stop; index++)
                if ((index<numpoints) && (cabs(points[index]) != 0.))
                    {
                    wt = weights[index];
                    wtsum += fabs (wt);
                    segsum = segsum + points[index];
                    if (wt < -0.5) nlsb++;
                    else if ((wt > 0.5) && (wt < 1.5)) nusb++;
                    else if (wt > 1.5) {nusb++;nlsb++;}
                    }
            if (wtsum > 0)
                {
                if(amp_phase_flag == 0)
                    {
                    data = cabs (segsum) / wtsum;
                    if (nlsb < nusb) symbol = '+';
                    else if (nusb < nlsb) symbol = '-';
                    else symbol = '*';
                                        /* Override for all freqs */
                    if (ch == 'A') symbol = ch;
                    }
                else
                    {
                    data = carg(segsum) *180./M_PI + 180.;
                    symbol = ch;
                    }
                grid (matrix, x, y, width, height,
                        (double)width, scale_y,
                        winstart, winstop,
                        seg, data,
                        symbol, altch);
                }
            }
        }
    else
        {
        for(index=0;index<numpoints;index++)
            {
            if(amp_phase_flag == 0)
                data = cabs(points[index]);
            else
                data = carg(points[index]) *180./M_PI + 180.;
            if (weights[index] != 0)
                grid(matrix,x,y,width,height,
                    scale_x,scale_y,
                    winstart,winstop,
                    index,data,
                    ch,altch);
            }
        }
    }
