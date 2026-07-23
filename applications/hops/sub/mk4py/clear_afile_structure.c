#include "afile_structure.h"
#include <stdio.h>
#include <stdlib.h>

#ifndef CLEAR_AFILE_STRUCTURE_H__
#define CLEAR_AFILE_STRUCTURE_H__

void clear_afile_structure(afile_structure* afile_struct)
{
    afile_struct->nroot = 0;
    afile_struct->ncorel = 0;
    afile_struct->nfringe = 0;
    afile_struct->ntriangle = 0;
    afile_struct->nquad = 0;
    afile_struct->rootdata = NULL;
    afile_struct->coreldata = NULL;
    afile_struct->fringedata = NULL;
    afile_struct->triangledata = NULL;
    afile_struct->quaddata = NULL;
}

#endif /* CLEAR_AFILE_STRUCTURE_H__ */
