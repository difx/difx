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
#include "type_comp.h"

void 
plot_complex (matrix, x, y, width, height, 
                scale_x, scale_y, winstart, winstop,
                        points, weights, numpoints, ch, altch,segment,amp_phase_flag) 
char **matrix;
int x, y, width, height, numpoints, winstart, winstop,segment;
complex *points;
double scale_x, scale_y, *weights;
char ch, altch;

    {
    int index, seg, nlsb, nusb, start, stop;
    char symbol;
    double seg_len,data,c_mag(),c_phase(), wt, wtsum;
    complex segsum, c_zero(), s_mult(), c_add();

    if (segment)
        {
        seg_len = (scale_x / (double)width);
        if (seg_len < 1) seg_len = 1.;
        for(seg = 0; seg < width; seg++)
            {
            segsum = c_zero();
            wtsum=0; nlsb=0; nusb=0;
            start = (int)(seg * seg_len);
            stop = (int)((seg + 1.0) * seg_len);
                                        /* Accumulate vector sum, weights, */
                                        /* and usb/lsb counts */
            for (index = start; index < stop; index++)
                if ((index<numpoints) && (c_mag(points[index]) != 0.))
                    {
                    wt = weights[index];
                    wtsum += fabs (wt);
                    segsum = c_add (segsum, points[index]);
                    if (wt < -0.5) nlsb++;
                    else if ((wt > 0.5) && (wt < 1.5)) nusb++;
                    else if (wt > 1.5) {nusb++;nlsb++;}
                    }
            if (wtsum > 0)
                {
                if(amp_phase_flag == 0)
                    {
                    data = c_mag (segsum) / wtsum;
                    if (nlsb < nusb) symbol = '+';
                    else if (nusb < nlsb) symbol = '-';
                    else symbol = '*';
                                        /* Override for all freqs */
                    if (ch == 'A') symbol = ch;
                    }
                else
                    {
                    data = c_phase(segsum) *180./M_PI + 180.;
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
                data = c_mag(points[index]);
            else
                data = c_phase(points[index]) *180./M_PI + 180.;
            if (weights[index] != 0)
                grid(matrix,x,y,width,height,
                    scale_x,scale_y,
                    winstart,winstop,
                    index,data,
                    ch,altch);
            }
        }
    }
