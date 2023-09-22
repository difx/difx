#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "d2m4_pcal_record.h"

int d2m4_pcal_can_merge_records(struct d2m4_pcal_record* rec1, struct d2m4_pcal_record* rec2)
{

    if(rec1 != NULL && rec2 != NULL)
    {
        //better have the same antenna, tint and mjd if we are merging two records
        //we are also enforcing the same number of tones per channel --- but maybe this isn't necessary?
        if( ( strcmp(rec1->antenna, rec2->antenna) == 0 ) &&   //check the antenna IDs are the same
            ( fabs(rec1->tint - rec2->tint) < AP_TOL*rec1->tint  ) &&  //check the relative difference between the two integration times is less than AP_TOL
            ( fabs(rec1->mjd - rec2->mjd) < AP_TOL*rec1->tint  ) //&&
            //( rec1->ntones == rec2->ntones) //check the difference between the mjds is less than AP_TOL*tint )
        )
        {
            if(rec1->phasors != NULL && rec2->phasors != NULL)
            {
                return 1;
            }
        }
    }
    return 0;
}


struct d2m4_pcal_record* d2m4_pcal_merge_record(struct d2m4_pcal_record* record1, struct d2m4_pcal_record* record2)
{
    int ri = 0;
    int n1 = 0;
    int n2 = 0;
    int ch = 0;
    int nt = 0;
    int max_ntones = 0;
    int n_total_phasors = 0;
    struct d2m4_pcal_record* merged_record = NULL;

    struct d2m4_pcal_record* rec1 = NULL;
    struct d2m4_pcal_record* rec2 = NULL;


    if( d2m4_pcal_can_merge_records(record1, record2) == 1)
    {
        rec1 = record1;
        rec2 = record2;

        //swap position of records if frequency order is different
        if( record2->phasors[0].frequency < record1->phasors[0].frequency)
        {
            rec1 = record2;
            rec2 = record1;
        }

        //make some space for the merged record
        merged_record =  malloc( sizeof( struct d2m4_pcal_record ) );
        d2m4_pcal_init_record(merged_record);
        strcpy(merged_record->antenna, rec1->antenna);
        merged_record->mjd = rec1->mjd;
        merged_record->tint = rec1->tint;
        merged_record->nchannels = rec1->nchannels + rec2->nchannels;

        max_ntones = rec1->ntones;
        if(max_ntones < rec2->ntones)
        {
            max_ntones = rec2->ntones;
        }

        merged_record->ntones = max_ntones;
        //determine how much phasor space we need
        n1 = (rec1->nchannels * max_ntones);
        n2 = (rec2->nchannels * max_ntones);
        n_total_phasors = n1 + n2;

        //make some space for the phasors
        merged_record->phasors = (struct d2m4_pcal_phasor*) malloc( sizeof( struct d2m4_pcal_phasor)*n_total_phasors );

        //now we loop over each phasor record and add them into the merged record
        for(ch=0; ch<rec1->nchannels; ch++)
        {
            for(nt=0; nt<max_ntones; nt++)
            {
                ri = nt + ch*max_ntones;

                if(nt < rec1->ntones)
                {
                    //printf("merging rec1 %d %d  $c\n ", ch, nt, rec1->phasors[ nt + ch*(rec1->ntones) ].polarization);
                    merged_record->phasors[ri].polarization = rec1->phasors[ nt + ch*(rec1->ntones) ].polarization;
                    merged_record->phasors[ri].frequency = rec1->phasors[ nt + ch*(rec1->ntones) ].frequency;
                    merged_record->phasors[ri].dstr = rec1->phasors[ nt + ch*(rec1->ntones) ].dstr;
                    merged_record->phasors[ri].real = rec1->phasors[ nt + ch*(rec1->ntones) ].real;
                    merged_record->phasors[ri].imag = rec1->phasors[ nt + ch*(rec1->ntones) ].imag;
                }
                else
                {
                    //insert dummy pcal phasor
                    merged_record->phasors[ri].polarization = '0';
                    merged_record->phasors[ri].frequency = -1;
                    merged_record->phasors[ri].dstr = 0;
                    merged_record->phasors[ri].real = 0;
                    merged_record->phasors[ri].imag = 0;
                }
            }
        }

        for(ch=0; ch<rec2->nchannels; ch++)
        {

            for(nt=0; nt<max_ntones; nt++)
            {
                ri = n1 + nt + ch*max_ntones;
                if(nt < rec2->ntones)
                {
                    merged_record->phasors[ri].polarization = rec2->phasors[ nt + ch*(rec2->ntones)].polarization;
                    merged_record->phasors[ri].frequency = rec2->phasors[ nt + ch*(rec2->ntones)].frequency;
                    merged_record->phasors[ri].dstr = rec2->phasors[ nt + ch*(rec2->ntones)].dstr;
                    merged_record->phasors[ri].real = rec2->phasors[ nt + ch*(rec2->ntones)].real;
                    merged_record->phasors[ri].imag = rec2->phasors[ nt + ch*(rec2->ntones) ].imag;
                }
                else
                {
                    //insert dummy pcal phasor
                    merged_record->phasors[ri].polarization = '0';
                    merged_record->phasors[ri].frequency = -1;
                    merged_record->phasors[ri].dstr = 0;
                    merged_record->phasors[ri].real = 0;
                    merged_record->phasors[ri].imag = 0;
                }
            }
        }

        return merged_record;
    }

    //ERROR!
    return NULL;
}
