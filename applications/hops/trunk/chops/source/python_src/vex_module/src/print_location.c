/************************************************************************/
/*									*/
/* This is a utility routine to print out the line number(s) of a	*/
/* statement, generally as a parsing error diagnostic to allow the	*/
/* user to find offending statements easily.  Efficiency is not a	*/
/* concern.								*/
/*									*/
/*	Inputs:		st		Statement number		*/
/*			stlist		via extern			*/
/*									*/
/*	Output:		printed via msg()				*/
/*									*/
/* Created 2 November 1998 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

void
print_location (int st)
    {
    int line, startline, endline;
    char *ch;
    extern struct statement *stlist;
    extern int nstmt;
    extern char *vexstart, *vexend;

    line = 1;
    for (ch=vexstart; ch<vexend; ch++)
	{
	if (*ch == '\n') line++;
	if (ch == stlist[st].start) startline = line;
	if (ch == stlist[st].end) 
	    {
	    endline = line;
	    break;
	    }
	}
					/* Print out results */
    if (startline == endline)
	msg ("Event occurred on line %d", 2, startline);
    else
	msg ("Event occurred between lines %d and %d", 2, startline, endline);

    }
