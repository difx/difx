/************************************************************************/
/*                                                                      */
/*                                                                      */
/*      Inputs:         x,y             Starting cursor position        */
/*                                                                      */
/*      Output:         x,y             device coords of selected point */
/*                      return value    if < 0, -1=non-fatal error      */
/*                                              -2=user typed 'a'       */
/*                                              -3=user typed 'b'       */
/*                                              -4=user typed 'x'       */
/*                                              -5=fatal error          */
/*                                                                      */
/* Created 26 April 1990 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include "cpgplot.h"
#include <stdio.h>

int
read_cursor(x, y)
float *x, *y;
    {
    extern int plot_open;
    char c;

    if(! plot_open) 
        {
        msg("You must have a plot on screen before enabling cursor",2);
        return(-5);
        }
                                        /* Make x and y come out in device coords */
    cpgsvp(0.0,1.0,0.0,1.0);
    cpgswin(0.0,1.0,0.0,1.0);
                                        /* Usually due to hardcopy device */
    if(cpgcurs(x, y, &c) != 1) 
        {
        msg("Cannot use cursor on this device",2);
        return(-5);
        }
                                        /* TRC, BLC return codes */
                                        /* Terminate return code on 'x' */
    if(c == 'a' || c == 'A') return(-2);
    if(c == 'b' || c == 'B') return(-3);
    if(c == 'x' || c == 'X') return(-4);

    return(0);
    }
