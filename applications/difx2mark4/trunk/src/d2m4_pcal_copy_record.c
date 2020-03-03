#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "d2m4_pcal_record.h"

void d2m4_pcal_copy_record(struct d2m4_pcal_record* record1, struct d2m4_pcal_record* record2)
{
    int ri = 0;
    int n_total_phasors = 0;

    if(record1 != NULL && record2 != NULL)
    {
        //first delete the phasor data in record1 if it is already present
        if(record1->phasors != NULL)
        {
            free(record1->phasors);
        }
        record1->phasors = NULL;

        //now init and copy in the new data
        d2m4_pcal_init_record(record1);
        strcpy(record1->antenna, record2->antenna);
        record1->mjd = record2->mjd;
        record1->tint = record2->tint;
        record1->nchannels = record2->nchannels;
        record1->ntones = record2->ntones;

        //determine how much phasor space we need
        n_total_phasors = (record1->nchannels * record1->ntones);
        //make some space for the phasors
        record1->phasors = (struct d2m4_pcal_phasor*) malloc( sizeof( struct d2m4_pcal_phasor)*n_total_phasors );

        //now we loop over each phasor record and add them into the merged record
        for(ri=0; ri<n_total_phasors; ri++)
        {
            record1->phasors[ri].polarization = record2->phasors[ri].polarization;
            record1->phasors[ri].frequency = record2->phasors[ri].frequency;
            record1->phasors[ri].dstr = record2->phasors[ri].dstr;
            record1->phasors[ri].real = record2->phasors[ri].real;
            record1->phasors[ri].imag = record2->phasors[ri].imag;
        }
    }
    else
    {
        printf("Error could not copy p-cal records, null pointer encountered!\n");
    }

};
