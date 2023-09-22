#include <stdlib.h>

#include "d2m4_pcal_record.h"

void d2m4_pcal_free_record(struct d2m4_pcal_record* rec)
{
    free(rec->phasors);
    rec->phasors = NULL;
    free(rec);
};
