/********************************************************************************/
/*                                                                              */
/* This routine identifies typed user input as commands, returning an integer   */
/* which corresponds to the command.  This integer is fed to a switch           */
/* statement in the command interpreter (execute.c).  This routine also         */
/* performs a minimum match and checks for uniquness, issuing an informative    */
/* message in cases of ambiguity.  The routine has been coded with easy         */
/* expandability in mind.  To add new commands, all that is required is to      */
/* modify the initialization of the commands structure below, and add the       */
/* appropriate integer branch in the switch statement in execute.c.             */
/*                                                                              */
/*      Input:          string          Typed command name                      */
/*                                                                              */
/*      Output:         return value    Command number, or -1 for failure       */
/*                                                                              */
/* Created March 30 1989 by CJL                                                 */
/********************************************************************************/
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/********************************************************************************/
/* Main structure defining command numbers **************************************/
/********************************************************************************/

struct 
    {
    char        *comname;
    int         comno;
    } commands[] = 
    {
    "exit", 0,                  /* "Action" commands */
    "inputs", 1,
    "plot", 2,
    "clear", 3,
    "edit", 4,
    "zoom", 5,
    "read", 6,
    "write", 7,
    "help", 8,
    "unflag", 9,
    "summary", 10,
    "setyear", 11,
    "batch", 12,
    "nobatch", 13,
    "sort",14,
    "unsort",15,
    "run",16,
    "fplot",17,
    "parameter",18,
    "pwrite",19,
    "plist", 20,
    "twrite", 21,
    "close", 22,

    "grid", 30,                 /* Plot control parameters */
    "yscale", 31,
    "axis", 32,
    "mode", 33,
    "xscale", 34,
    "remote", 35,
    "reference", 36,

    "timerange", 40,            /* Data selection parameters */
    "stations", 41,
    "baselines", 42,
    "frequencies", 43,
    "experiment", 44,
    "qcodes", 45,
    "type", 46,
    "snrmin", 47,
    "sources", 48,
    "length", 49,
    "fraction",50,
    "nfreq",51,
    "snrmax",52,
    "prange",53,
    "procrange", 54,
    "triangles", 55,
    "quads", 56,
    "bsnrmin", 57,
    "bsnrmax", 58,
    "polarizations", 59,

    "ccread", 70,               /* Experiment overview commands/parameters */
    "psplot",71,
    "psfile",72,
    "reproc",73,

    "device", 81,               /* I/O control parameters */
    "outversion", 82,

    "noop", 99,                 /* Miscellaneous */
    "quit", 100,
    "test", 101,
    "coclip", 102,
    "unrecognized", -1
    };

int
command_id(string)
char *string;                   /* string is just the command name */
    {                           /* without arguments */
    int n, i, j, index[20];
    char c;

/********************************************************************************/
/* Convert to lower case, compare with above commands, and return id number *****/
/********************************************************************************/

    n = strlen(string); /* Convert to lower case */
    for(i=0;i<n;i++) 
        {
        c = string[i];
        if(isupper(c)) c = tolower(c);
        string[i] = c;
        }

    i = 0;j = 0;                /* Loop through commands, comparing */
    while(commands[i].comno >= 0) 
        {
        if(strncmp(string, commands[i++].comname, n) == 0) 
            {
            index[j] = i-1;
            j++;
            }
        }

    if(j == 0) return(-1);                                      /* No match */
    else if(j == 1) return(commands[index[0]].comno);   /* Unique match */
    else 
        {                                                       /* Ambiguous */
        msg("Ambiguous command. Could be:");
        for(i=0;i<j;i++)
            msg("       %s",2,commands[index[i]].comname);
        return(99);
        }
    }
