/************************************************************************/
/*                                                                      */
/* This routine parses the "primitives.txt" file in order to discover   */
/* what is supposed to be in each primitive block.  It stores the       */
/* parsed information in the primitives memory structure (extern)       */
/*                                                                      */
/*      Inputs:         primitives.txt  external text file              */
/*                                                                      */
/*      Output:         primitives      Structure array (extern)        */
/*                      nprimitive      # of filled elements (extern)   */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created October 26 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
block_params (void)
    {
    FILE *fp;
    int lno, stmt_no;
    char line[256], blockname[256], stmt_name[256];
    struct blk_stmt *blk;
    extern struct blk_stmt blk_stmts[];
    extern int nblk_stmt;
    char filename[256];
    extern char textdir[];

                                        /* Open the text file */
    sprintf (filename, "%s/blk_stmt.txt", textdir);
    fp = fopen (filename, "r");
    if (fp == NULL)
        {
        msg ("Failed to open blk_stmt.txt", 2);
        return (-1);
        }
                                        /* Read all lines until the end */
                                        /* Do minimum error checking ... */
                                        /* the input file is a controlled */
                                        /* file, part of the library */
    lno = nblk_stmt = 0;
    stmt_no = 0;
    while (fgets (line, 255, fp) != NULL)
        {
        lno++;
                                        /* Ignore comments */
        if (line[0] == '*') continue;
                                        /* New primitive block */
        if (line[0] == '$')
            {
            sscanf (line, "$%s", blockname);
            if (strlen (blockname) >= 32)
                {
                msg ("Block name too long '%s'", 2, blockname);
                return (-1);
                }
            blk = blk_stmts + nblk_stmt;
            strcpy (blk->name, blockname);
            nblk_stmt++;
            if (nblk_stmt > MAXBLKSTMT)
                {
                msg ("Too many blk_stmt block definitions", 2);
                return (-1);
                }
            stmt_no = 0;
            }
                                        /* Parameter name */
        else
            {
            sscanf (line, "%s", stmt_name);
            if (strlen (stmt_name) >= 32)
                {
                msg ("Statement name name too long '%s'", 2, stmt_name);
                return (-1);
                }
            strcpy (blk->stmt[stmt_no], stmt_name);
            stmt_no++;
            if (stmt_no > MAXSTMT)
                {
                msg ("Too many statements in $%s block definition", 2, 
                                        blk->name);
                return (-1);
                }
            blk->nstmt = stmt_no;
            }
        }

    return (0);
    }
