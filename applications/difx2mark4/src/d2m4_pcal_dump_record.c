#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "d2m4_pcal_record.h"

int d2m4_pcal_dump_record(struct d2m4_pcal_record* rec, char* line_buffer, int buffer_size)
{
    int ri;
    int dstr = 0;
    int nchars = 0;
    int mchars = 0;
    int nrecords = 0;
    struct d2m4_pcal_phasor* ph = NULL;
    if(rec->phasors != NULL)
    {
        nrecords = rec->nchannels * rec->ntones;

        mchars = sprintf(line_buffer, "%s %lf %lf %d %d ", rec->antenna, rec->mjd, rec->tint, rec->nchannels, rec->ntones);
        nchars += mchars;

        //loop over the total number of phasor records
        //this is assumed to be nchannels*ntones
        //(although not all of these may be filled with valid data (this is indicated by freq of -1))
        for(ri=0; ri<nrecords; ri++)
        {
            if(nchars + mchars < buffer_size)
            {
                ph = &(rec->phasors[ri]);
                //keep track of the original data stream at the individual phasor level
                //I believe in RJC's notation 'cquad' is the real component, and 'squad' is the imaginary component
                //sprintf( "%d %c %lf%lf%n",  &(ph->frequency), &(ph->polarization), &(ph->real), &(ph->imag), &mchars);
                mchars = sprintf(line_buffer+nchars, "%d %c %d %lf %lf ", ph->frequency, ph->polarization, ph->dstr, ph->real, ph->imag);
                nchars += mchars;
            }
            else
            {
                printf("Error! d2m4_pcal_dump_record is out of buffer space!\n");
                break;
            }
        }
    }
    else
    {
        printf("Error! d2m4_pcal_dump_record cannot find phasor data!\n");
    }

    return nchars;
};
