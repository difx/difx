/************************************************************************/
/*                                                                      */
/* This routine sets up the sources input parameter, parsing the        */
/* user-typed input line and accepting up to 32-character alphanumeric  */
/* source names.                                                        */
/*                                                                      */
/*  Inputs:     arg1,arg2,remarg    Typed input strings                 */
/*                                                                      */
/*  Output:     inp.sources     source input parameter                  */
/*                                                                      */
/* Created                                         12 April 1989 by CJL */
/*   minor mod to allow 32 char source names       19 June  2008        */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_sources(arg1,arg2,remarg)
char *arg1, *arg2, *remarg;
    {
    extern struct inputs inp;
    char outbuf[500], buf[500];
    char *src, *strtok();

    outbuf[0] = '\0';

    sprintf(buf,"%s %s %s",arg1,arg2,remarg);

    src = strtok(buf," ,");         /* Step through baselines */
    while(src != NULL) 
        {
        if(strlen(src) > 32) 
            {
            msg("Bad source '%s'",2,src);
            return(-1);
            }
        strcat(outbuf,src);         /* Build up pretty source string */
        strcat(outbuf," ");
        src = strtok(NULL," ,");
        }
    inp.sources[0] = '\0';
    strcpy(inp.sources,outbuf);     /* Blank input line gives blank result */
    return(0);
    }
