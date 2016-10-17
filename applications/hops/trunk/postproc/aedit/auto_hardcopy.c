/************************************************************************/
/*									*/
/* Supports automatic hardcopy plotting, to hide nasty PGPLOT details	*/
/* from user.								*/
/*									*/
/*	Inputs:		None						*/
/*									*/
/*	Output:		on printer					*/
/*			return value		0 OK, !=0 bad		*/
/*									*/
/* Created 1 March 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "cpgplot.h"
#include "aedit.h"

int auto_hardcopy(void)
    {
    extern struct inputs inp;
    extern int plot_open;
    char dummy[30], command[256], filename[256], directory[256], c;
    int i, j;

					/* Relevant modes are 2, 3, 4 */
    if (inp.dev_auto <= 1) return (0);
                                        /* First flush plot file to disk */
    cpgend();
    plot_open = FALSE;
					/* Strip out diskfile name, which */
					/* is enclosed in double quotes for */
					/* the benefit of pgplot */
    i = j = 0;
    while (inp.device[i++] != '"')
	;
    while (inp.device[i] != '"') 
	{
	filename[j] = inp.device[i];
	i++; j++;
	}
    filename[j] = '\0';
					/* Issue command for hpgl or postscript */
    if (inp.dev_auto == 4)
	sprintf (command, "aedit_plot -h %s", filename); 
    else if ((inp.dev_auto == 2) || (inp.dev_auto == 3))
	sprintf (command, "aedit_plot -p %s", filename); 
    else
	{
	msg ("Error: device input messed up in auto_hardcopy()", 2);
	return (-1);
	}
					/* system() has zero return on success */
    return (system (command));
    }
