/************************************************************************/
/*                                                                      */
/* Opens and sets up a /XW pgplot device for display of quality codes   */
/* in a 2-d array of coloured rectangles, for experiment overview       */
/* purposes.  Several interactive options are implemented via buttons   */
/* displayed in the pgplot window.                                      */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created 17 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include "cpgplot.h"
#include "psplot.h"

int
setup_psplot (psarray)
struct ps_array *psarray;
    {
    int i, clow, chigh, length, nbands;
    float xmin, xmax, ymin, ymax;
    char dummy[200], band[2];
    static char codes[] = QUALITIES;
    static char code[] = " ";
    
                                        /* Clean up from other stuff */
    length = sizeof (dummy);
    cpgqinf ("STATE", dummy, &length);
    if (strcmp (dummy, "OPEN") == 0) cpgend();
                                        /* Open up xwindow device */
    if (cpgbeg (0, "/XS", 1, 1) != 1)
        {
        msg ("Could not open /xw device for psplot display", 2);
        msg ("The xwindow device is the only one currently", 2);
        msg ("supported for this command", 2);
        return (1);
        }
    cpgask (FALSE);
                                        /* Check that we have enough colours */
    cpgqcol (&clow, &chigh);
    if (chigh < 22)
        {
        msg ("Unable to use the necessary 25 colours on the /xw display.", 2);
        msg ("Probable cause ... other applications, or 'wallpaper'", 2);
        msg ("have used up too many colour map entries on your X server", 2);
        msg ("You may have better luck if you simplify your screen", 2);
        msg ("There were only %d colours available", 2, chigh);
        msg ("\nTip: if you are using xv for xwindows wallpaper, you", 2);
        msg ("can use the flag '-ncols nnn' to limit the number of", 2);
        msg ("colors the wallpaper display soaks up.  CJL has found", 2);
        msg ("that '-ncols 170' works well for his Xterminal setup", 2);
        cpgend();
        return (1);
        }
    msg ("Using 25 of %d available colours", 2, chigh);
                                        /* Set up coordinate system */
    cpgsvp (0.0, 1.0, 0.0, 1.0);
    cpgswin (0., 1000., 0., 1000.);

                                        /* Redefine the colours */
                                        /* Quality codes first .... */

    cpgscr (1, 0.4, 0.2, 0.1);          /* qcode 0, dark brown */
    cpgscr (2, 0.6, 0.3, 0.15);         /* qcode 1, medium brown */
    cpgscr (3, 0.8, 0.4, 0.2);          /* qcode 2, light brown */
    cpgscr (4, 0.08, 0.2, 0.08);        /* qcode 3, dark  green */
    cpgscr (5, 0.1, 0.3, 0.1);          /* qcode 4,   |   green */
    cpgscr (6, 0.13, 0.4, 0.13);        /* qcode 5,   |   green */
    cpgscr (7, 0.18, 0.55, 0.18);       /* qcode 6,   |   green */
    cpgscr (8, 0.23, 0.7, 0.23);        /* qcode 7,   |   green */
    cpgscr (9, 0.28, 0.85, 0.28);       /* qcode 8,  \|/  green */
    cpgscr (10, 0.33, 1.0, 0.33);       /* qcode 9, light green */
    cpgscr (11, 0.0, 0.0, 1.0);         /* qcode A, blue */
    cpgscr (12, 0.6, 0.25, 0.8);        /* qcode B, violet */
    cpgscr (13, 0.06, 0.5, 0.6);        /* qcode C, deep turquoise */
    cpgscr (14, 1.0, 0.0, 0.0);         /* qcode D, red */
    cpgscr (15, 1.0, 0.4, 0.0);         /* qcode E, orange */
    cpgscr (16, 0.55, 0.55, 0.8);       /* qcode F, light slate blue */
    cpgscr (17, 0.6, 0.0, 0.0);         /* qcode G, deep red */
    cpgscr (18, 0.1, 0.8, 0.87);        /* qcode H, turquoise */

    cpgscr (19, 0.3, 0.3, 0.3);         /* Unprocessed, dark grey */
    cpgscr (20, 0.0, 0.0, 0.3);         /* Minus'ed out, deep blue */
                                        /* Now text and button stuff */

    cpgscr (21, 1.0, 1.0, 0.0);         /* Text & borders, yellow */
    cpgscr (22, 1.0, 0.4, 0.8);         /* Active button, pink */
    cpgscr (23, 0.4, 0.4, 0.4);         /* Inactive button, dark grey */

                                        /* Miscellaneous colours */

    cpgscr (24, 1.0, 1.0, 1.0);         /* Tag character, white */

                                        /* Draw main box */
    cpgsci (TEXT);                      /* Yellow, thick solid lines, */
    cpgslw (3);                         /* hollow fill */
    cpgsls (1);
    cpgsfs (2);
    cpgrect (PLOT_XMIN, PLOT_XMAX, PLOT_YMIN, PLOT_YMAX);
    cpgslw (1);
                                        /* Draw key */
    cpgsch (1.3);
    cpgsci (TEXT);
    cpgtext (KEY_XMIN - 180., KEY_YMIN - KEYINC, "KEY:");
    ymin = KEY_YMIN; ymax = ymin + KEYINC; 
    xmin = KEY_XMIN; xmax = xmin + KEYINC;
    cpgsfs (1);
    cpgsch (0.8);
    for (i=1; i<=18; i++)
        {
        xmin += KEYINC + GUARD_BAND;
        xmax += KEYINC - GUARD_BAND;
        cpgsci (i);
        cpgrect (xmin, xmax, ymin, ymax);
        xmin -= GUARD_BAND;
        xmax += GUARD_BAND;
        cpgsci (TEXT);
        code[0] = codes[i];
        cpgptxt (xmin + (KEYINC / 2.0), ymin - KEYINC, 0.0, 0.5, code);
        }
    cpgsci (TEXT);
    cpgtext (KEY_XMIN - 110., ymin - KEYINC, "QUALITY CODES:");
                                        /* Info area */
    cpgsch (0.8);
    cpgtext (0.0, 10.0, "B'LINE");
    cpgtext (0.0, 30.0, "SCAN");
    cpgtext (0.0, 50.0, "SOURCE");
                                        /* Special colours for minus, not done */
    cpgsci (MINUS);
    cpgrect (800., 820., 50., 70.);
    cpgsci (UNPROC);
    cpgrect (800., 820., 10., 30.);
    cpgsci (TEXT);
    cpgtext (830., 50., "MINUS'ED OUT");
    cpgtext (830., 10., "UNPROCESSED");
                                        /* Draw top buttons */
    cpgsfs (2);
    cpgslw (2);
    if (psarray->param.npages > 1)
        {
        cpgsci (INACTIVE);
        cpgrect (PREV_BUTTON);
        cpgsci (ACTIVE);
        cpgrect (NEXT_BUTTON);
        cpgsci (TEXT);
        cpgsch (0.8);
        cpgslw (1);
        cpgptxt (210., 975., 0.0, 0.5, "PREVIOUS PAGE");
        cpgptxt (390., 975., 0.0, 0.5, "NEXT PAGE");
        cpgslw (2);
        }
    cpgsci (INACTIVE);
    cpgrect (FPLOT_BUTTON);
    cpgrect (RETAIN_BUTTON);
    cpgsci (ACTIVE);
    cpgrect (SELECT_BUTTON);
    cpgsci (TEXT);
    cpgrect (QUIT_BUTTON);
                                        /* Insert button text */
    cpgsci (TEXT);
    cpgsch (0.8);
    cpgslw (1);
    cpgptxt (570., 975., 0.0, 0.5, "SELECT");
    cpgptxt (750., 975., 0.0, 0.5, "FRINGE PLOT");
    cpgptxt (930., 975., 0.0, 0.5, "QUIT PSPLOT");
    cpgptxt (37., 915., 0.0, 0.5, "RETAIN");
                                        /* Draw buttons for frequency bands */
    nbands = strlen (psarray->subgroups);
    if (nbands > 1)
        {
        band[1] = '\0';
        for (i=0; i<nbands; i++)
            {
            cpgsci (INACTIVE);
            cpgrect (BAND_XORIGIN, BAND_XORIGIN + BAND_SIZE,
                     BAND_YORIGIN - i*BAND_SIZE,
                     BAND_YORIGIN - (i+1)*BAND_SIZE);
            cpgsci (TEXT);
            band[0] = psarray->subgroups[i];
            cpgsch (1.0);
            cpgptxt (BAND_XORIGIN + BAND_SIZE/2, 
                     BAND_YORIGIN - i*BAND_SIZE - BAND_SIZE/2 - 12,
                     0.0, 0.5, band);
            }
                                        /* Init with band 1 displayed */
        cpgsci (ACTIVE);
        cpgrect (BAND_XORIGIN, BAND_XORIGIN + BAND_SIZE,
                 BAND_YORIGIN, BAND_YORIGIN - BAND_SIZE);
                                        /* Draw side buttons */
        }
/****************** Not implemented yet
    cpgslw (2);
    cpgsci (ACTIVE);
    cpgrect (0., 70., 400., 440.);
    cpgsci (INACTIVE);
    cpgrect (0., 70., 330., 370.);
***********************************/
                                        /* Insert button text */
/****************** Not implemented yet
    cpgsci (TEXT);
    cpgslw (1);
    cpgptxt (35., 415., 0.0, 0.5, "BASELINE");
    cpgptxt (35., 345., 0.0, 0.5, "SOURCE");
***********************************/

    return (0);
    }
