/************************************************************************/
/*									*/
/* At present, set only the extern which specifies the aedit help file	*/
/* directory								*/
/*									*/
/*	Inputs:								*/
/*									*/
/*	Output:								*/
/*									*/
/* Created ?? by CJL							*/
/*									*/
/************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
					/* Declare this global, to be */
					/* initialized here, but used */
					/* throughout program */
char ahelpdir[200];

void check_env(void)
    {					/* Default value */
    static char *helpdef = "/usr/local/lib/ahelp";
    char *dummy;

    if ((dummy = getenv ("AHELP")) != NULL) strcpy (ahelpdir, dummy);
    else strcpy (ahelpdir, helpdef);

    return;
    }
