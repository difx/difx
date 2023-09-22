#include <stdio.h>
#include "fstruct.h"
#include "mk4_util.h"

void
clear_fstruct (fstruct *f_info)
    {
    int i;

    f_info->order = -1;
    f_info->name = NULL;
    f_info->type = -1;
    f_info->source[0] = '\0';
    f_info->baseline[0] = '\0';
    f_info->station = ' ';
    f_info->freq_code = ' ';
    f_info->filenum = -1;
    f_info->rootcode[0] = '\0';
    f_info->done = FALSE;
    for (i=0; i<4; i++)
        {
        f_info->intparm[i] = 0;
        f_info->floatparm[i] = 0.0;
        }
    }
