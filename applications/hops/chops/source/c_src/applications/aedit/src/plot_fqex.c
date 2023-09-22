/************************************************************************/
/*                                                                      */
/* This routine causes all the fringe data for a frequency/experiment   */
/* combination to be plotted on the current device.  Up to              */
/* 30 plots may be placed on each page.  Each individual plot contains  */
/* only one station, baseline, closure triangle, or quad as appropriate */
/* and only one frequency band and experiment.  Depending on the        */
/* setting of the mode input                                            */
/* parameter, the plots will or will not be split up one source per     */
/* plot.  This routine directly handles only the NOSPLIT case.  Source  */
/* by source plots are handled in plot_source(), called from here.      */
/*                                                                      */
/*      Inputs:         data                                            */
/*                                                                      */
/*      Output:         return value            0=success               */
/*                                                                      */
/* Created 17 April 1989 by CJL                                         */
/* Altered for generalization of x-y plotting Feb 21 1994 by CJL        */
/* Added edit capability "on the fly" for each page                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cpgplot.h"
#include "aedata.h"
#include "aedit.h"
#include "summary.h"
#include "pstruct.h"

#define NOSPLIT 0
#define SPLIT 1

int symlist[NSYMBOL] = {4,5,6,7,11,12,13,14,16,17,18};

struct plot_info pdata[30];

int plot_fqex (esum *data, struct frqexp fqex)
    {
    extern struct inputs inp;
    extern int interactive, up_to_date, fflag, tflag, qflag;
    struct plot_points pp;
    struct plot_info *pd, pdumy;
    int i, j, nloop, ret, nplot, onscreen, plots_per_page, tdiff, day, npoint;
    int np, pno, sno;
    char bas[3], trilist[MAXCLOSE*4], *tri, quadlist[MAXCLOSE*5], *quad;
    char plot_id[5], buf[256], *eol;
    int symbol[3];

    ret = 0;

    if (pdata[0].npts > 0) onscreen = TRUE;
    else onscreen = FALSE;
    nplot = 0;
    plots_per_page = inp.grid[0] * inp.grid[1];
                                        /* How many plots are needed to do this */
                                        /* frequency/experiment? */
    switch (inp.plotby)
        {
        case STATION_PLOT:
            np = strlen (fqex.stations);
            break;
        case BASELINE_PLOT:
        case TRIANGLE_PLOT:
        case QUAD_PLOT:
            np = fqex.nbtq;
            break;
        case ALL_PLOT:
            np = 1;
        default:
            ;
        }
                                        /* Must also loop over sources */
                                        /* in split mode */
    if (inp.mode == SPLIT) nloop = np * fqex.nsource;
    else nloop = np;
                                        /* Now loop over baselines, whatever */
    for (i=0; i<nloop; i++)
        {
                                        /* pno is plot number for this fqex */
                                        /* or fqex/source, sno is source number */
        if (inp.mode == SPLIT) 
            {
            pno = i % np;
            sno = i / np;
                                        /* New source = new page */
/*          if (pno == 0) nplot = 0;   */
            }
        else pno = i;
                                        /* Do we need a new page? */
        if(nplot >= plots_per_page) nplot = 0;
        if(nplot == 0) 
            {
            if (((i != 0) || onscreen) && interactive)
                {
                ret = 0;
                while(TRUE)
                    {
                    printf ("\007Continue with next page (y/n) or edit points (e)? "); 
                    if (!fgets (buf, sizeof(buf), stdin)) exit(0);
		    eol = strrchr(buf, '\n');
		    if (eol) *eol = 0;	/* Drop newline */
                    if(buf[0] == 'Y' || buf[0] == 'y') 
                        {
                        ret = 0;
                        break;
                        }
                    else if(buf[0] == 'N' || buf[0] == 'n') 
                        {
                        ret = 1;
                        break;
                        }
                    else if(buf[0] == 'E' || buf[0] == 'e') 
                        {
                        npoint = 0;
                        cursor_select (data, &npoint, 0);
                        if (npoint > 0)
                            {
                            msg ("Zapped %d points from this screen", 2, npoint);
                            switch (inp.plotby)
                                {
                                case STATION_PLOT:
                                case BASELINE_PLOT:
                                    fflag += npoint;
                                    break;
                                case TRIANGLE_PLOT:
                                    tflag += npoint;
                                    break;
                                case QUAD_PLOT:
                                    qflag += npoint;
                                    break;
                                default:
                                    break;
                                }
                            up_to_date = FALSE;
                            }
                        continue;
                        }
                    else printf("Answer 'y', 'n' or 'e'\n");
                    }
                }
            if (ret == 1) break;
            cpgpage();
            for (j=0; j<30; j++) clear_pstruct (pdata + j);
                                        /* A nice, pretty page header */
                                        /* with key in bottom 5% of page */
            if (inp.mode == SPLIT)
                {
/*              plot_header (fqex, fqex.slist+sno);  */
                plot_header (fqex, NULL);
                symbol_key (NULL);
                }
            else
                {
                plot_header (fqex, NULL);
                symbol_key (&fqex);
                }
            }
        pd = pdata + nplot;             /* Convenience pointer */

                                        /* fill identifying string */
        switch (inp.plotby)
            {
            case STATION_PLOT:
                plot_id[0] = fqex.stations[pno];
                plot_id[1] = '\0';
                break;
            case BASELINE_PLOT:
                strncpy (plot_id, fqex.btq + pno*3, 2);
                plot_id[2] = '\0';
                break;
            case TRIANGLE_PLOT:
                strncpy (plot_id, fqex.btq + pno*4, 3);
                plot_id[3] = '\0';
                break;
            case QUAD_PLOT:
                strncpy (plot_id, fqex.btq + pno*5, 4);
                plot_id[4] = '\0';
                break;
            case ALL_PLOT:
                plot_id[0] = '\0';
            default:
                ;
            }
                                        /* In case we have time axes, must fill */
                                        /* in toffset here .. otherwise the */
                                        /* f.p. variables passed to cpgswin() */
                                        /* run out of precision */
        day = fqex.begin / 86400;
        pd->toffset = day * 86400;
                                        /* Do plotting separately for the 2 modes */
        if (inp.mode == SPLIT)
            {
                                        /* Symbols indicate quality */
            symbol[0] = 5; symbol[1] = 13; symbol[2] = 4;
                                        /* Get data for just one source */
            get_plot_data (data, pd, fqex, fqex.slist+sno,
                                        plot_id, &pp, symbol);

            if (setup_plot (pd, nplot, fqex) != 0) continue;

            plot_points (pd, &pp, symbol);
            }

        else if (inp.mode == NOSPLIT)
            {
                                        /* Loop over all sources, putting */
                                        /* all points in same pd structure */
                                        /* and figuring out plot dimensions */
                                        /* Fills in the REAL pd structure */
            for (j=0; j<fqex.nsource; j++)
                {
                if (j > NSYMBOL) symbol[0] = symbol[1] = symbol[2] = 18;
                else symbol[0] = symbol[1] = symbol[2] = symlist[j];

                get_plot_data (data, pd, fqex, fqex.slist+j, 
                                                plot_id, &pp, symbol);
                }
                                        /* Now know enough to set up viewport, */
                                        /* draw and label frame, etc */
            if (setup_plot (pd, nplot, fqex) != 0) continue;
                                        /* This does actual plotting, a */
                                        /* different symbol for each source */
                                        /* Uses a temporary pd structure */
            for (j=0; j<fqex.nsource; j++)
                {
                                        /* Clean out temp plot structure */
                clear_pstruct (&pdumy);
                                        /* Insert time offset in dummy */
                pdumy.toffset = pd->toffset;
                                        /* Symbols = source in nosplit mode */
                if (j >= NSYMBOL) symbol[0] = symbol[1] = symbol[2] = 18;
                else symbol[0] = symbol[1] = symbol[2] = symlist[j];

                get_plot_data (data, &pdumy, fqex, fqex.slist+j, 
                                                plot_id, &pp, symbol);
                plot_points (&pdumy, &pp, symbol);
                }
            }
        nplot++;
        }               /* End loop over baselines/whatever */

    return(ret);
    }
