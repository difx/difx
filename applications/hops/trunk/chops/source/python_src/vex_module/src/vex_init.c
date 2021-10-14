/************************************************************************/
/*                                                                      */
/* Initializes the rudimentary vex parsing code, defines externs.       */
/*                                                                      */
/*      Inputs:         None                                            */
/*                                                                      */
/*      Output:         None                                            */
/*                                                                      */
/* Created 21 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

struct comment   clist[MAXCOMMENTS];
struct quote     qlist[MAXQUOTES];
struct statement *stlist;
struct block     blist[MAXBLOCKS];
struct scan      slist[MAXSCANS];
struct ref       rlist[MAXREFS];
struct def       dlist[10][MAXDEFS];
struct blk_stmt  blk_stmts[MAXBLKSTMT];

int ncom, nqot, nstmt, nblock, nscan, nref, nblk_stmt, ndef[10];
int defspace = 0, ndefs = 0; /* ndefs set to zero in scan init */
char *vexstart, *vexend;
char ovex_src[262144];
char cvex_src[32768];
char evex_src[4096];
char ivex_src[32768];
char svex_src[32768];
char lvex_src[32768];
int do_output;
int ovex_ver, cvex_ver, ivex_ver, svex_ver, evex_ver, lvex_ver;

int
vex_init (void)
    {
    int i;
    static int first = TRUE;

    ncom = nqot =  nblock = nscan = nref = 0;
    for (i=0; i<10; i++) ndef[i] = 0;
    do_output = FALSE;
    ovex_ver = cvex_ver = ivex_ver = svex_ver = evex_ver = lvex_ver = 0;
                                        /* We want output vex code to */
                                        /* persist between calls */
    if (first)
        {
        environment();
        ovex_src[0] = '\0';
        cvex_src[0] = '\0';
        evex_src[0] = '\0';
        ivex_src[0] = '\0';
        svex_src[0] = '\0';
        lvex_src[0] = '\0';

                                        /* This is used for freeing memory */
                                        /* on all except first call */
        nstmt = 0;
                                        /* Flags for freeing memory */
        stlist = NULL;
        vexstart = NULL;
                                        /* Reading text files only needed */
                                        /* on first execution */
        if (param_formats() < 0)
            {
            msg ("Error reading vex parameter format definitions", 2);
            return (-1);
            }

        nblk_stmt = 0;
        if (block_params() != 0)
            {
            msg ("Error reading block parameter lists", 2);
            return (-1);
            }
        first = FALSE;
        }
                                        /* Free memory for stripped statements */
    if (nstmt > 0)
        {
        if (stlist == NULL)
            {
            msg ("Memory allocation inconsistency in vex_init()", 2);
            return (-1);
            }
        for (i=0; i<nstmt; i++) free (stlist[i].str);
        nstmt = 0;
        free (stlist);
        stlist = NULL;
        }
                                        /* If vexstart is not null, it */
                                        /* must have been allocated from */
                                        /* a previous execution and should */
                                        /* be freed. */
    if (vexstart != NULL) free (vexstart);

    return (0);

    }
