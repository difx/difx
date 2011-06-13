/************************************************************************/
/*									*/
/* This routine prints the contents of the summ structs on the screen	*/
/* in a nice format, after updating the structures with a call to	*/
/* summ_data()								*/
/*									*/
/*	Inputs:		data						*/
/*			arg		Specifies what data to display	*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 9 April 1989 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"

int
pr_summary (data, arg)
esum *data;
char *arg;
    {
    extern int dotype;
    int type, ret;

    if (strncmp (arg, "root", strlen(arg)) == 0) type = 0;
    else if (strncmp (arg, "corel", strlen(arg)) == 0) type = 1;
    else if (strncmp (arg, "fringe", strlen(arg)) == 0) type = 2;
    else if (strncmp (arg, "triangle", strlen(arg)) == 0) type = 3;
    else if (strlen(arg) > 1) type = 5;
    else if (sscanf (arg, "%d", &type) != 1) type = -2;
    if (strlen(arg) == 0) type = -1;
    if ((type < -1) || (type > 5))
	{
	msg("\tOptions:  \tSUMMARY",2);
	msg("\t\t\tSUMMARY ROOT",2);
	msg("\t\t\tSUMMARY COREL",2);
	msg("\t\t\tSUMMARY FRNGE",2);
	msg("\t\t\tSUMMARY TRIANGLE",2);
	msg("\t\t\t(= SUMMARY 0, 1, 2, OR 3)",2);
	msg("\t\tor\tSUMMARY <sourcename>",2);
	return (-1);
	}

    if(summ_data (data, STANDARD) != 0) return(-1);
    
    if (type == 0) 
	{
	if ((dotype == 0) || (dotype == -1)) ret = pr_rsumm ();
	else 
	    {
	    msg("Sorry, cannot summarize root data when DOTYPE = 1 or 2",2);
	    ret = -1;
	    }
	}
    else if (type == 1)
	{
	if ((dotype == 1) || (dotype == -1)) ret = pr_csumm ();
	else 
	    {
	    msg("Sorry, cannot summarize corel data when DOTYPE = 0 or 2",2);
	    ret = -1;
	    }
	}
    else if (type == 2) 
	{
	if ((dotype == 2) || (dotype == -1)) ret = pr_fsumm ();
	else 
	    {
	    msg("Sorry, cannot summarize fringe data when DOTYPE = 0 or 1",2);
	    ret = -1;
	    }
	}
    else if (type == 3) ret = pr_tsumm ();
    else if (type == -1) ret = pr_allsumm ();
    else if (type == 5) ret = pr_ssumm (arg);

    return (ret);
    }
