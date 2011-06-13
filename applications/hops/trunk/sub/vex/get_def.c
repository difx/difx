/************************************************************************/
/*                                                                      */
/* Returns a pointer to the named def in the specified primitive block  */
/*                                                                      */
/*      Inputs:         defname         Name of requested def           */
/*                      blockname       name of block it is in          */
/*                                                                      */
/*      Output:         blockno         index into blist                */
/*                      defno           index into blist[].deflist      */
/*                      return value    ptr to requested def            */
/*                                      NULL indicates error            */
/*                                                                      */
/* Created December 31 1997 by CJL                                      */
/* Rewritten for Haystack parser, 28 October 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

struct def *
get_def (char *defname,
         char *blockname,
         int *blockno,
         int *defno)
    {
    int i;
    struct block *blk;
    extern struct block blist[];
    extern int nblock;
                                        /* Find block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, blockname) == 0) break;
    if (i == nblock)
        {
        msg ("Can't find '$%s' block", 2, blockname);
        return (NULL);
        }
    blk = blist + i;
    *blockno = i;
                                        /* Find def */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Can't find '%s' def in $%s block", 2, defname, blockname);
        return (NULL);
        }
    *defno = i;
                                        /* Found it */
    return (blk->deflist + i);
    }
