/************************************************************************/
/*                                                                      */
/* Basic utility to find the memory addresses of all the comments       */
/* and quoted strings within the memory image of the vex file.          */
/*                                                                      */
/*      Inputs:         vexstart,vexend Via extern                      */
/*                                                                      */
/*      Output:         clist, qlist    Arrays of structs with          */
/*                                      requested information           */
/*                      ncom, nqot      Number of entries in lists      */
/*                                      via extern                      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 21 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
locate_cq (void)
    {
    extern struct comment clist[];
    extern struct quote qlist[];
    extern int ncom, nqot;
    extern char *vexstart, *vexend;
    char *ptr, tmpstring[512];
    int comment, quoted, clen;
                                        /* Requires a bit of care, since */
                                        /* VEX allows arbitrary strings in */
                                        /* comments, including quote chars, */
                                        /* and arbitrary strings inside quotes */
                                        /* including asterisks */
    ncom = nqot = 0;
    ptr = vexstart;
    comment = FALSE;
    quoted = FALSE;
    while (ptr <= vexend)
        {
                                        /* We are in a comment, so ignore */
                                        /* everything except ending newline */
                                        /* or EOF */
        if (comment)
            {
            if ((*ptr == '\n') || (ptr == vexend)) 
                {
                clist[ncom].end = ptr;
                                        /* Debugging */
                clen = ptr - clist[ncom].start;
                if (clen > 511) clen = 511;
                strncpy (tmpstring, clist[ncom].start, clen);
                tmpstring[clen] = '\0';
                msg ("Comment %d: '%s'", -3, ncom, tmpstring);
                ncom++;
                comment = FALSE;
                }
            }
                                        /* In a quoted string, ignore all */
                                        /* except non-escaped terminating quote */
        else if (quoted)
            {
            if ((*ptr == '\"') && (*(ptr-1) != '\\'))
                {
                qlist[nqot].end = ptr;
                nqot++;
                quoted = FALSE;
                }
            }
                                        /* Not in either, look for comment */
                                        /* start or quoted string start */
        else
            {
            if (*ptr == '*') 
                {
                if (ncom >= MAXCOMMENTS)
                    {
                    msg ("Error, maximum number of comments exceeded", 2);
                    return (-1);
                    }
                comment = TRUE;
                clist[ncom].start = ptr;
                }
                                        /* Special case handling of double */
                                        /* quote in declination value ... */
                                        /* this is not start of quoted string */
            else if ((*ptr == '"') && (! isdigit (*(ptr-1)))) 
                {
                if (nqot >= MAXQUOTES)
                    {
                    msg ("Error, maximum number of quoted strings exceeded", 2);
                    return (-1);
                    }
                quoted = TRUE;
                qlist[nqot].start = ptr;
                }
            }

        ptr++;
        }

    return (0);
    }
