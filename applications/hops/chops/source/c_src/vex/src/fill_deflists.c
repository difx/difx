/************************************************************************/
/*                                                                      */
/* This routine goes through all primitive blocks, making a list of     */
/* the defs they contain.                                               */
/*                                                                      */
/*      Inputs:         blist (extern)  list of blocks                  */
/*                      stlist (extern) list of all statements          */
/*                                                                      */
/*      Output:         deflist         element of each blist struct    */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 21 August 1998 by CJL                                        */
/* Modified 26 October 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define DEF 1
#define ENDDEF 2
#define OTHER 3

int
fill_deflists (void)
    {
    int i, j, start, end, ndef, in_def, stmt, st, n;
    char stname[128], arg1[128], junk[256];
    struct block *blk;
    struct def *dlist;
    extern int nblock, nstmt;
    extern struct block blist[];
    extern struct statement *stlist;
    extern struct blk_stmt blk_stmts[];
    struct blk_stmt *bs;
    extern int nblk_stmt;

    for (i=0; i<nblock; i++)
        {
        blk = blist + i;
                                        /* Find block in blk_stmt list */
        for (j=0; j<nblk_stmt; j++)
            if (strcmp (blk->name, blk_stmts[j].name) == 0) break;
        if (j == nblk_stmt) 
            {
            msg ("Block '%s' not found in blk_stmt.txt file", 2, blk->name);
            return (-1);
            }
        bs = blk_stmts + j;
                                        /* Does this block have defs? */
                                        /* If not, skip it. */
        for (j=0; j<bs->nstmt; j++) 
            if (strcmp (bs->stmt[j], "def") == 0) break;
        if (j == bs->nstmt) continue;
                                        /* Get start/end statement #'s */
                                        /* of this $block */
        start = blk->stno;
        if (i == nblock-1) end = nstmt;
        else end = blist[i+1].stno;
                                        /* Point to local dlist */
        dlist = blk->deflist;
                                        /* Search for def/enddef pairs */
        in_def = FALSE;
        ndef = 0;
        for (st=start+1; st<end; st++)
            {
            if (ndef >= MAXDEFS)
                {
                msg ("Too many defs in $%s block", 2, blk->name);
                return (-1);
                }
                                        /* Identify the statement */
            /* kad */
            /* n = sscanf (stlist[st].str, "%s %s", stname, arg1, junk); */
            n = sscanf (stlist[st].str, "%s %s", stname, arg1);
            if (strcmp (stname, "def") == 0) stmt = DEF;
            else if (strcmp (stname, "enddef") == 0) stmt = ENDDEF;
            else stmt = OTHER;
                                        /* Check for rogue statements */
            if ((! in_def) && (stmt != DEF))
                {
                msg ("Inappropriate '%s' statement outside def", 2, stname);
                print_location (st);
                return (-1);
                }
            if ((in_def) && (stmt == DEF))
                {
                msg ("Missing 'enddef' for def '%s'", 2, dlist[ndef].name);
                print_location (st);
                return (-1);
                }
                                        /* Take appropriate action */
            switch (stmt)
                {
                case DEF:
                    if (n != 2)
                        {
                        msg ("Malformed def statement", 2);
                        print_location (st);
                        return (-1);
                        }
                    dlist[ndef].start = st;
                    strncpy (dlist[ndef].name, arg1, MAX_NAMESIZE);
                    in_def = TRUE;
                    break;

                case ENDDEF:
                    if (n != 1)
                        {
                        msg ("Malformed enddef statement", 2);
                        print_location (st);
                        return (-1);
                        }
                    dlist[ndef].end = st;
                    ndef++;
                    in_def = FALSE;
                    break;

                default:
                    break;
                }
            }
                                        /* Better have finished with */
                                        /* an enddef */
        if (in_def)
            {
            msg ("Missing 'enddef' for def '%s'", 2, dlist[ndef].name);
            if (end == nstmt) st = end - 1;
            print_location (st);
            return (-1);
            }
                                        /* How many defs did we find? */
        blk->ndef = ndef;
        }

    return (0);
    }
