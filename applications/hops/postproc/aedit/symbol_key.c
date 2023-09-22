/************************************************************************/
/*									*/
/* Places a symbol key in the bottom 5 percent of the page.  If the	*/
/* argument is NULL, this is SPLIT mode, and the key is just the 3	*/
/* quality levels.							*/
/*									*/
/*	Inputs:		fqex		structure containing freq, expt */
/*									*/
/*	Output:		none						*/
/*									*/
/* Created  March 10 1992 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "cpgplot.h"
#include "summary.h"
#include "pstruct.h"
#include "aedit.h"

void symbol_key (struct frqexp *fqex)
    {
    extern int symlist[NSYMBOL];
    char symbol;
    char keystring[200], key[20];
    int font, width, l, i;
    float size, line;

    cpgsvp (0.0, 1.0, 0.0, 1.0);
    cpgswin (0.0, 1.0, 0.0, 1.0);

    cpgqch(&size);
    cpgqcf(&font);
    cpgqlw(&width);

    cpgsch (0.7);
    cpgslw (1);
    cpgscf (1);

    line = 1.0;
    sprintf (keystring, "Symbol key:  ");

    if (fqex == NULL)
	{
	sprintf (keystring, "Symbol key: %c = good, %c = suspect, %c = bad  ", 5, 13, 4);
	l = strlen (keystring);
	}
    else
	{
	for (i=0; i<fqex->nsource; i++)
	    {
	    if (i > NSYMBOL) break;
	    sprintf (key, "  = %s, ", fqex->slist[i].name);
	    if ((i == NSYMBOL - 1) && (fqex->nsource > NSYMBOL))
		sprintf (key, "  = the rest, ");
	    key[0] = symlist[i];

	    strcat (keystring, key);
	    if ((l = strlen (keystring)) > 90)
		{
		keystring[l-2] = '\0';	/* Remove ", " from end */
		cpgtext (0.0, 0.05 - line*0.02, keystring);
		line += 1.0;
		sprintf (keystring, "             ");
		l = 0;
		}
	    }
	}

    if (l > 0)
	{
	keystring[l-2] = '\0';	/* Remove ", " from end */
	cpgtext (0.0, 0.05 - line*0.02, keystring);
	}
	
    cpgsch (size);
    cpgscf (font);
    cpgslw (width);
    }
