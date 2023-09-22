#include <string.h>
#include "d2m4_pcal_record.h"

//returns the number of unique polaization in a pcal record
int d2m4_pcal_count_unique_polarizations(struct d2m4_pcal_record* record)
{
    char pol_array[8] = ""; //plenty of space, can't have more than 4 unique pols...R,L,X,Y
    char* chpos;
    int ri = 0;
    int count = 0;

    if(record != NULL)
    {
        int nchannels = record->nchannels;
        int ntones = record->ntones;

        for(ri=0; ri< nchannels*ntones; ri++)
        {
            chpos = strchr(pol_array, record->phasors[ri].polarization);
            if(chpos == NULL)
            {
                //we haven't seen this pol character before
                if(count < 8)
                {
                    pol_array[count] = record->phasors[ri].polarization;
                    count++;
                }
                else 
                {
                    break;
                }
            }
        }
    }

    return count;

}
