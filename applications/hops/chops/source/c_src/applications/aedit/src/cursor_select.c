/************************************************************************/
/*                                                                      */
/* This is the routine that enables the user to select points on an     */
/* interactive screen with a cursor, and zap them from the database     */
/* (actually, set the ZAPPED bit in the flag field).  Two modes are     */
/* supported ... point-by-point zapping (in which case the point must   */
/* be selected unambiguously - cursor between 2 points is no good), or  */
/* area zapping (where the user defines the TRC and BLC of a rectangle, */
/* and all points inside are zapped).   There is an option to merely    */
/* place the index of the unambiguously selected data point in the      */
/* npoint variable, for use as a more general data point selection      */
/* mechanism ... this avoids duplication of the rather messy coordinate */
/* transformation and pdata looping code below.                         */
/*                                                                      */
/*      Inputs:         data                                            */
/*                      option          0 ==> edit points               */
/*                                      1 ==> place index of nearest    */
/*                                            point in npoint           */
/*                                                                      */
/*      Output:         return value    if -5, some error occurred      */
/*                                      otherwise same as read_cursor() */
/*                      data            flag field modified (not opt 1) */
/*                      npoint          Number of newly flagged points  */
/*                                      (index of best point, option 1) */
/*                                                                      */
/* Created 26 April 1990 by CJL                                         */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "flags.h"

int cursor_select (esum *data, int *npoint, int option)
    {
    extern struct plot_info pdata[];
    extern struct inputs inp;
    static float x = 0.5;
    static float y = 0.5;
    float xscale, xoffset, yscale, yoffset, xval, yval, 
                xerrl, xerrh, yerrl, yerrh;
    float xpos, ypos, xbest, ybest, xdist, ydist, dist, dmin, dmin2;
    float xhpos, xlpos, yhpos, ylpos, xhbest, xlbest, yhbest, ylbest;
    float trc[2], blc[2];
    int i, j, best, trcinf[2], blcinf[2], ret, offset, symbol, *flag;
    int temp, ambig, refrem, index;
    char c;
    fringearray *fdatum;
    trianglearray *tdatum;
    struct plot_info *pd;

    if(option != 0 && option != 1) 
        {
        msg("Bad option passed to edit_cursor", 2);
        return(-5);
        }

    trcinf[0] = FALSE;
    blcinf[0] = FALSE;
                                        /* Start cursor in center of screen, all  */
                                        /* coords in device units for these ops */
    if (option == 0) x = y = 0.5;
    while ((ret = read_cursor (&x, &y)) > -4) 
        {
                                        /* Find out which plot we are dealing with */
        i = 0;
        while (pdata[i].npts > 0) 
            {
            if((x >= pdata[i].vport[0]) && (x <= pdata[i].vport[1]) 
                    && (y >= pdata[i].vport[2]) && (y <= pdata[i].vport[3])) 
                break;
            i++;
            }
                                        /* Cursor must be inside plot border */
        if(pdata[i].npts == 0) 
            {
            msg("Not in any plot.  Try again", 2);
            continue;
            }
                                        /* Convenience pointer */
        pd = pdata + i;
                                        /* Box mode, BLC */
        if (ret == -2 && option == 0) 
            {
            blcinf[0] = TRUE;
            blcinf[1] = i;
            blc[0] = x;
            blc[1] = y;
            if(! trcinf[0]) 
                {
                msg("BLC set, type 'b' or 'B' for TRC", 2);
                continue;
                }
            }
                                        /* TRC */
        if (ret == -3 && option == 0) 
            {
            trcinf[0] = TRUE;
            trcinf[1] = i;
            trc[0] = x;
            trc[1] = y;
            if(! blcinf[0]) 
                {
                msg("TRC set, type 'a' or 'A' for BLC", 2);
                continue;
                }
            }
                                        /* If one corner set, insist on second */
        if((blcinf[0] && (! trcinf[0])) || (trcinf[0] && (! blcinf[0]))) 
            {
            msg("Set other corner for area zap ('a' or 'b')", 2);
            break;
            }
                                        /* Check for sanity */
        if(trcinf[0] && blcinf[0] && (blcinf[1] != trcinf[1])) 
            {
            msg("TRC and BLC must be in the same plot!", 2);
            trcinf[0] = FALSE;
            blcinf[0] = FALSE;
            continue;
            }
                                        /* Conversion constants to device coords */
        xscale = (pd->vport[1] - pd->vport[0]) / (pd->window[1] - pd->window[0]);
        xoffset = pd->vport[0] - xscale*pd->window[0];
        yscale = (pd->vport[3] - pd->vport[2]) / (pd->window[3] - pd->window[2]);
        yoffset = pd->vport[2] - yscale*pd->window[2];
                                        /* Some initialization */
        best = -1;
        dmin = 1.0;
        dmin2 = 5.0;
        if (pd->plotby == BASELINE_PLOT) refrem = inp.refrem;
                                        /* Now loop though points in this plot */
                                        /* looking for points to zap */
        for(j=0;j<pd->npts;j++) 
            {
                                        /* Point at the datum for this entry */
            fdatum = data->fdata + pd->index[j];
            tdatum = data->tdata + pd->index[j];
                                        /* For station plots, set refrem */
            if (pd->plotby == STATION_PLOT)
                {
                if (pd->station == fdatum->data.baseline[0]) refrem = REFERENCE;
                else refrem = REMOTE;
                }
                                        /* Get the world coordinate values */
            if (pd->plotby == TRIANGLE_PLOT)
                {
                triangle_value (pd->xaind, tdatum, 
                                pd->toffset, &xval, &xerrh, &xerrl);
                triangle_value (pd->yaind, tdatum, 
                                pd->toffset, &yval, &yerrh, &yerrl);
                }
            else
                {

                datum_value (pd->xaind, pd->x_aux, pd->toffset,
                                refrem, fdatum, &xval, &xerrh, &xerrl);
                datum_value (pd->yaind, pd->y_aux, pd->toffset,
                                refrem, fdatum, &yval, &yerrh, &yerrl);
                }

                                        /* Convert to device coordinates */
            xpos = xval*xscale + xoffset;
            ypos = yval*yscale + yoffset;
            xhpos = xerrh*xscale + xoffset;
            xlpos = xerrl*xscale + xoffset;
            yhpos = yerrh*yscale + yoffset;
            ylpos = yerrl*yscale + yoffset;
                                        /* Area zap, if point in rectangle, */
                                        /* delete it */
            if(trcinf[0] && blcinf[0])
                {
                if(xpos > blc[0] && xpos < trc[0] && ypos > blc[1] && ypos < trc[1]) 
                    {
                    symbol = pd->symbol[j];
                                        /* Point to appropriate flag field */
                    index = pd->index[j];
                    if (pd->plotby == TRIANGLE_PLOT) flag = &(data->tdata[index].flag);
                    else flag = &(data->fdata[index].flag);
                                        /* If not already zapped, zap it. */
                                        /* erase_point() removes it from screen. */
                    if (*flag & ZAPPED) continue;
                    erase_point (xpos, ypos, xhpos, xlpos, yhpos, ylpos, symbol);
                    if (! *flag) *npoint += 1;
                    *flag |= ZAPPED;
                    }
                }
                                        /* Point by point zap, merely keep */
                                        /* track of which point is closest, */
                                        /* and zap it at the end */
            else
                {
                                        /* How close is it? */
                xdist = x - xpos;
                ydist = y - ypos;
                dist = xdist*xdist + ydist*ydist;
                                        /* Is it closest or 2nd closest? */
                if(dist < dmin) 
                    {
                    dmin2 = dmin;
                    dmin = dist;
                    best = pd->index[j];
                    symbol = pd->symbol[j];
                                        /* Remember position */
                    xbest = xpos;
                    ybest = ypos;
                    xhbest = xhpos;
                    xlbest = xlpos;
                    yhbest = yhpos;
                    ylbest = ylpos;
                    }
                else if (dist < dmin2) dmin2 = dist;
                }
            }           /* End loop through pd */

                                        /* Must be twice as close to a point */
                                        /* (in device coordinates) */
                                        /* than any neighbour to accept */
        if(4.0*dmin > dmin2) 
            msg ("Ambiguous data point selection", 2);
                                        /* Merely send back index of point */
        else if (option == 1) 
            {
            *npoint = best;
            return (ret);
            }
        else if (best >= 0) 
            {
            if (pd->plotby == TRIANGLE_PLOT) flag = &(data->tdata[best].flag);
            else flag = &(data->fdata[best].flag);
                                        /* If not already zapped, zap it. */
                                        /* erase_point() removes it from screen. */
            if (! (*flag & ZAPPED))
                {
                erase_point (xbest, ybest, xhbest, xlbest, yhbest, ylbest, symbol);
                if (! *flag) *npoint += 1;
                *flag |= ZAPPED;
                }
            }

                                        /* Reset .. we just zapped area */
        if(trcinf[0] && blcinf[0]) 
            {
            blcinf[0] = FALSE;
            trcinf[0] = FALSE;
            }
        }               /* End cursor read loop */

    return (ret);

    }
