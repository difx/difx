/************************************************************************/
/*                                                                      */
/* This routine compiles the master list of all vex statements in the   */
/* vex file.  It stores the start and end addresses of the statement    */
/* in the raw vex file image, inclusive of whitespace and comments, and */
/* also fills in a string which is the vex statement with comments and  */
/* excess whitespace removed, ready for keyword-specific parsing.  It   */
/* allocates memory as needed to accomodate the statement list.         */
/*                                                                      */
/*      Inputs:         vexstart,vexend         via extern              */
/*                                                                      */
/*      Output:         stlist          (extern) allocated & filled in  */
/*                      return value    Number of statements listed     */
/*                                                                      */
/* Created 24 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
find_statements (void)
    {
    extern char *vexstart, *vexend;
    extern struct statement *stlist;
    extern int nstmt;
    char *statement, *ptr, *start, *end;
    struct statement *st;
    int nalloc;
    char *get_statement (char *, char **, char **);
                                        /* Allocate some starting space */
    nalloc = 500;
    stlist = (struct statement *)calloc (nalloc, sizeof (struct statement));
    if (stlist == NULL)
        {
        msg ("Memory allocation failure in find_statements()", 2);
        return (-1);
        }
                                        /* Get statements until we hit EOF */
    ptr = vexstart;
    while (TRUE)
        {
        st = stlist + nstmt;
        statement = get_statement (ptr, &start, &end);
                                        /* Null return => EOF */
        if (statement == NULL) break;

        st->start = start;
        st->end = end;
        st->str = strdup (statement);
        if (st->str == NULL)
            {
            msg ("Strdup failure in find_statements()", 2);
            return (-1);
            }
                                        /* Point beyond ';' and increment count */
        ptr = end + 1;
        nstmt++;
                                        /* Time to expand memory? */
        if (nstmt >= nalloc)
            {
            nalloc += 100;
            stlist = (struct statement *)realloc 
                                (stlist, nalloc * sizeof (struct statement));
            if (stlist == NULL)
                {
                msg ("Memory allocation failure in find_statements()", 2);
                return (-1);
                }
            }
        }

    return (nstmt); 
    }
