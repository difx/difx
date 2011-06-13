/************************************************************************/
/*                                                                      */
/* Once the user has selected a data point on the psplot screen, this   */
/* routine takes action on that point, based on the button or key       */
/* pressed to generate the cursor read event.                           */
/*                                                                      */
/*      Inputs:         data            so we can zap points            */
/*                      psarray         param section needed here       */
/*                      x, y            world coordinates of cell center*/
/*                      cell            pointer to psarray cell of this */
/*                                              data point              */
/*                      key             key that was pressed.  psplot   */
/*                                      is hardwired to /XW, and the    */
/*                                      three mouse buttons return      */
/*                                      values A, D, X for left, middle */
/*                                      and right, respectively.        */
/*                      do_fplot        modifies behaviour of left      */
/*                                      mouse button to do fringe plot  */
/*                                      instead of point tagging        */
/*                                                                      */
/*      Output:         visuals, and updated flag field in main data    */
/*                      array.                                          */
/*                                                                      */
/* Created 22 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "cpgplot.h"
#include "aedata.h"
#include "flags.h"
#include "psplot.h"

void
ps_proc_datum (data, psarray, x, y, cell, key, do_fplot)
esum *data;
struct ps_array *psarray;
float x, y;
struct psplot_cell *cell;
char key;
int do_fplot;
    {
    int year, day, hour, minute, second, band;
    float xtag, ytag, sepmin, xmin, xmax, ymin, ymax;
    char infotxt[12];
    static fringearray *fdatum;
    extern int data_version;

    band = psarray->param.band;
    fdatum = data->fdata + cell->data_index[band];

    if (do_fplot && (key == 'A')) key = 'F';

    switch (key)
        {
                                        /* Just remove the point altogether */
                                        /* Invoked by 'x' or right button */
        case 'x':
        case 'X':
            fdatum->flag |= ZAPPED;
            xmin = x - (psarray->param.scan_sep * 0.5) + GUARD_BAND;
            xmax = x + (psarray->param.scan_sep * 0.5) - GUARD_BAND;
            ymin = y - (psarray->param.base_sep * 0.5) + GUARD_BAND;
            ymax = y + (psarray->param.base_sep * 0.5) - GUARD_BAND;
            cpgsci (UNPROC);
            cpgsfs (1);
            cpgrect (xmin, xmax, ymin, ymax);
            cell->data_index[band] = -1;
            cell->colour_index[band] = UNPROC;
                                        /* Zapping renders tags irrelevant */
            if (cell->flag[0] != 0) psarray->ntagged--;
            break;

                                        /* Tag a point visually and in the */
                                        /* data array for later treatment */
                                        /* Invoked by 'a' or left button, */
                                        /* unless do_fplot is true.  Always */
                                        /* accessible through lower case 'a' */
                                        /* on keyboard */
        case 'a':
        case 'A':
            xtag = x;
            ytag = y;
            cpgsci (TAG_COLOUR);
                                        /* Undo tagging if already done */
            if (cell->flag[band]) 
                {
                cell->flag[band] = 0;
                cpgsci (cell->colour_index[band]);
                psarray->ntagged--;
                }
            else 
                {
                cell->flag[band] = 1;
                psarray->ntagged++;
                }
                                        /* Do some symbol size scaling */
            cpgsch (psarray->param.tagsize);
            cpgslw (1);
            cpgpt (1, &xtag, &ytag, 13);
                                        /* Make a point of falling through */
                                        /* to info section below */

                                        /* Merely identify point with text */
                                        /* in lower left corner of screen */
                                        /* Invoked by 'd' or middle button */
        case 'D':
        case 'd':
            cpgsci (0);                 /* Erase old text */
            cpgsfs (1);
            cpgrect (INFO);
            int_to_time (fdatum->data.time_tag, &year, &day, &hour, &minute, &second);
            if (data_version > 1)
                sprintf (infotxt, "%02d%03d-%02d%02d%02d", 
                                                year, day, hour, minute, second);
            else
                sprintf (infotxt, "%02d%03d-%02d%02d", year, day, hour, minute);
            if (strlen (fdatum->data.scan_id) > 0) strcpy (infotxt, fdatum->data.scan_id);
            cpgsci (TEXT);
            cpgsch (0.9);
            cpgslw (2);
            cpgtext (INFO_X, INFO_Y + 40., fdatum->data.source);
            cpgtext (INFO_X, INFO_Y + 20., infotxt);
            cpgtext (INFO_X, INFO_Y + 2.0, fdatum->data.baseline);
            break;
                                        /* Pop up a fringe plot ... too */
                                        /* complicated to do in this switch */
        case 'f':
        case 'F':
            ps_fplot (psarray, &(fdatum->data));
            break;

        default:
            ;
        }

    }
