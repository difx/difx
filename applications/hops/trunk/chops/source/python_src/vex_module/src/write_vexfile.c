/************************************************************************/
/*                                                                      */
/* This routine is responsible for writing the (possibly modified) vex  */
/* file back out to disk.  To accommodate insertions, deletions and     */
/* changes (the latter constructed by a combination of deletion and     */
/* insertion), the writing is done on a statement by statement basis,   */
/* taking note of the contents of the insert and delete lists.  To      */
/* relax the constraint of sorted lists, the entire insert and delete   */
/* lists are searched each time, but this should be manageably fast.    */
/*                                                                      */
/*      Inputs:         del_list        List of statements to delete    */
/*                      ins_list        List of strings to insert       */
/*                      fp              Open file to write to           */
/*                                                                      */
/*      Output:         return value    0=OK, else bad                  */
/*                                                                      */
/* Created 25 August 1998 by CJL                                        */
/* Modified 21 December 1998 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
write_vexfile (int *del_list,
               struct insert *ins_list,
               FILE *fp)
    {
    int i, nchar, delete, nw, ni, nd;
    extern struct statement *stlist;
    extern int nstmt;

    for (i=0; i<nstmt; i++)
        {
        delete = FALSE;
        nd = 0;
        if (del_list != NULL)
            {
            while (del_list[nd] >= 0)
                if (i == del_list[nd++])
                    {
                    delete = TRUE;
                    break;
                    }
            }
        if (! delete)
            {
                                        /* Write the statement */
            nchar = stlist[i].end - stlist[i].start + 1;
            nw = fwrite (stlist[i].start, sizeof (char), nchar, fp);
            if (nw != nchar)
                {
                msg ("Error writing to output file", 2);
                return (-1);
                }
            }
                                        /* Any insertions? */
        if (ins_list == NULL) continue;
        ni = 0;
        while (ins_list[ni].after >= 0)
            {
            if (i == ins_list[ni].after)
                {
                if (fputs (ins_list[ni].str, fp) == EOF)
                    {
                    msg ("Error writing inserted text in output file", 2);
                    return (-1);
                    }
                break;
                }
            ni++;
            }
        }

                                    /* Last statement has trailing */
                                    /* whitespace omitted, so add a newline here */
    fputc ('\n', fp);

    return (0);
    }
