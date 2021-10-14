/************************************************************************/
/*									*/
/* Takes care of setting up the device and dev_auto inputs, based on a	*/
/* user-supplied string.						*/
/*									*/
/*	Inputs:		string		Typed by the user		*/
/*									*/
/*	Output:		inp		Suitably modified		*/
/*			return value	0=OK, not 0 = bad		*/
/*									*/
/* Created 1 March 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int set_device (char *string)
    {
    extern struct inputs inp;
    char c, *filename;
    int i, len;
					/* Simple PGPLOT string */
    if (strchr (string, '/') != NULL)
	{
	strcpy (inp.device, string);
	inp.dev_auto = 0;
	return (0);
	}
					/* Special aedit devices */
    if (strncasecmp (string, "xwindow", strlen (string)) == 0)
	{
	inp.dev_auto = 1;
	strcpy (inp.device, "/xw");
	return (0);
	}
					/* All devices below this need a */
					/* disk file, so construct name */
    /* FIXME: deprecated call and usage below */
    if ((filename = tmpnam (NULL)) == NULL)
	{
	msg ("Temporary file name creation failed in set_device()", 2);
	return (-1);
	}
					/* Strip preceding directory stuff */
    len = strlen (filename);
    for (i=len; i>0; i--) if (filename[i] == '/') break;
    if (filename[i] == '/') filename += i+1;
					/* Construct PGPLOT specification */
    if (strncasecmp (string, "ppostscript", strlen (string)) == 0)
	{
	inp.dev_auto = 2;
	sprintf (inp.device, "\"/tmp/%s\"/vps", filename);
	}
    else if (strncasecmp (string, "lpostscript", strlen (string)) == 0)
	{
	inp.dev_auto = 3;
	sprintf (inp.device, "\"/tmp/%s\"/ps", filename);
	}
    else if (strncasecmp (string, "hpgl", strlen (string)) == 0)
	{
	inp.dev_auto = 4;
	sprintf (inp.device, "\"/tmp/%s\"/hp", filename);
	}
    else
	{
	msg ("Unrecognized device specification '%s'", 2, string);
	return (-1);
	}

    return (0);
    }
