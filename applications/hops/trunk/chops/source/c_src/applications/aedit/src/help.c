/********************************************************************************/
/*                                                                              */
/* This is the help utility for aedit.  The routine is called with a            */
/* minimum-matchable string representing a command, and the corresponding       */
/* help file is printed on the stdout stream under the control of "more"        */
/*                                                                              */
/*      Input:          string          Typed command name as argument 1        */
/*                                                                              */
/* Created March 31 1989 by CJL                                                 */
/*                                                                              */
/********************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <errno.h>
#include <sys/types.h>
#include "aedit.h"


char *help_files[] = {
    "help",

    "exit",                     /* "Action" commands */
    "inputs",
    "plot",
    "clear",
    "edit",
    "zoom",
    "read",
    "write",
    "unflag",
    "summary",
    "setyear",
    "run",
    "sort",
    "unsort",
    "fplot",
    "parameter",
    "pwrite",
    "plist",

    "grid",                     /* Plot control parameters */
    "yscale", 
    "axis", 
    "mode",
    "xscale", 
    "reference",
    "remote",

    "timerange",                /* Data selection parameters */
    "stations", 
    "qcodes",
    "snrmin",
    "snrmax",
    "length",
    "sources",
    "baselines", 
    "frequencies",
    "polarizations",
    "experiment", 
    "type",
    "fraction",
    "nfreq",
    "prange",
    "procrange",

    "ccread",
    "psplot",
    "psfile",
    "reproc",

    "device",                   /* I/O control parameters */
    "outversion",

    "general",
    "batch",
    "nobatch",
        
    "end"                       /* Terminates help list */
};

void help(char *string)
                                /* string is just the command name */
{                               /* without arguments */
        extern char ahelpdir[];
        int n, i, match, nmatch;
        char c, filename[200], buf[200];
        struct stat statbuf;

        n = strlen(string);     /* Convert to lower case */
        for(i=0;i<n;i++) {
            c = string[i];
            if(isupper(c)) c = tolower(c);
            string[i] = c;
        }

        nmatch = 0;             /* Loop through commands, comparing */
        for(i=0;i<100;i++) {
            if(strcmp(help_files[i],"end") == 0) break;
            if(strncmp(string, help_files[i], n) == 0) {
                match = i;
                nmatch++;
            }
        }
        if(n == 0) {
            nmatch = 1;
            match = 0;
        }

                                /* Catch as many errors as possible */
        if(nmatch == 0) {
            msg("No help for %s", 2,string);
        }
        else if(nmatch > 1) {
            msg("Ambiguous help category %s",2,string);
        }
        else {
            sprintf(filename,"%s/%s",ahelpdir,help_files[match]);

            if(stat(filename,&statbuf) != 0) {
                if(errno == ENOENT) {
                    msg("Help file %s missing", 2,help_files[match]);
                }
                else perror("Help");
            }
            else {              /* Simply "more" the file to stdout */
                printf("\t\t\tAEDIT version 5.0\n\t\t\t-----------------\n\n");
                fflush(stdout);
                sprintf(buf,"cat %s | grep -v verbatim | more",filename);
                if(system(buf) != 0) msg("System() problem", 2);
            }
        }

}
