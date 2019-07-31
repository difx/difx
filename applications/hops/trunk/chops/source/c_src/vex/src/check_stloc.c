/************************************************************************/
/*                                                                      */
/* Checks all blocks for statements which don't belong, according to    */
/* the text file blk_stmt.txt                                           */
/*                                                                      */
/*      Inputs:         stlist (via extern)                             */
/*                      blk_stmts (via extern)                          */
/*                                                                      */
/*      Output:         return value            0=OK, else bad          */
/*                                                                      */
/* Created 2 November 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
check_stloc (void)
    {
    int i, j, st, len;
    char *str, *stname;
    struct blk_stmt *bst;
    struct block *blk;
    extern struct statement *stlist;
    extern struct block blist[];
    extern struct blk_stmt blk_stmts[];
    extern int nstmt, nblock, nblk_stmt;
                                        /* Loop over all blocks in vex file */
    for (i=0; i<nblock; i++)
        {
        blk = blist + i;
                                        /* Find blk_stmt entry corresponding */
                                        /* to this block */
        for (j=0; j<nblk_stmt; j++)
            if (strcmp (blk_stmts[j].name, blk->name) == 0) break;
        if (j == nblk_stmt)
            {
            msg ("Warning: unrecognized block $%s found, ignoring", 1, blk->name);
            continue;
            }
        bst = blk_stmts + j;
                                        /* Loop over all statements in this */
                                        /* block from vex file */
        for (st=blk->stno+1; st<=blk->end; st++)
            {
            str = stlist[st].str;
                                        /* Compare against list of allowed */
                                        /* statements in blk_stmts array for */
                                        /* this block */
            for (j=0; j<bst->nstmt; j++)
                {
                stname = bst->stmt[j];
                len = strlen (stname);
                if (strncmp (stname, str, len) == 0) break;
                }
                                        /* Not found, rogue statement */
            if (j == bst->nstmt)
                {
                msg ("Statement '%s' out of place in $%s", 2, str, blk->name);
                print_location (st);
                return (-1);
                }
            }    
        }

    return (0);
    }
