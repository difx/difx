#ifndef FSTRUCT_H
#define FSTRUCT_H

#include "fstruct.h"

#define MAXFSET 1000

struct fileset
    {
    char	    scandir[256];	/* Absolute pathname */
    short	    expno;		/* Part of directory name */
    char	    scanname[32];	/* Part of directory name */
    char	    rootname[40];	/* Stripped of directory information */
    short	    maxfile;		/* Biggest file number enountered */
    fstruct	    file[1000];		/* Files belonging to fileset */
    };

#endif
