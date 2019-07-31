#include "adata.h"

#ifndef AFILE_STRUCTURE_H__
#define AFILE_STRUCTURE_H__

typedef struct
{
    int nroot;
    int ncorel;
    int nfringe;
    int ntriangle;
    int nquad;
    rootsum* rootdata;
    corelsum* coreldata;
    fringesum* fringedata;
    trianglesum* triangledata;
    quadsum* quaddata;
} afile_structure;

#endif /* AFILE_STRUCTURE_H__ */
