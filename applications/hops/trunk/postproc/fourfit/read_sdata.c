/************************************************************************/
/*                                                                      */
/* Given a fileset structure, this routine reads all type-3 (sdata)     */
/* files into memory, storing them in the sdata array.                  */
/*                                                                      */
/*      Inputs:         fset            fileset structure               */
/*                                                                      */
/*      Output:         sdata           array of pointers to mk4_sdata  */
/*                                      structures                      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created April 9 1998 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "fileset.h"
#include "mk4_dfio.h"

int
read_sdata (
struct fileset *fset,
struct mk4_sdata *sdata)
    {
    int i, nf;
    char filename[256];

                                        /* Forcibly initialize array */
    for (i=0; i<MAXSTATIONS; i++) clear_mk4sdata (sdata + i);

    nf = 0;
    for (i=0; i<MAXFSET; i++)
        {
        if (fset->file[i].name == NULL) continue;
        if (fset->file[i].type != 3) continue;

        sprintf (filename, "%s/%s", fset->scandir, fset->file[i].name);
                                        /* Nonfatal error */
        if (read_mk4sdata (filename, sdata + nf) != 0)
            {
            msg ("Error reading sdata file '%s'", 1, filename);
            continue;
            }
        nf++;
        }
                                        /* No error conditions */
    return (0);
    }
