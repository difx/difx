#include <stdlib.h>
#include <string.h>

#include "d2m4_pcal_record.h"

void d2m4_pcal_init_record(struct d2m4_pcal_record* rec)
{
    //initialize the memory space
    memset(rec->antenna, '\0', sizeof(char)*8);
    rec->mjd = 0.0;
    rec->tint = 0.0;
    rec->nchannels = 0;
    rec->ntones = 0;
    rec->phasors = NULL;
};
