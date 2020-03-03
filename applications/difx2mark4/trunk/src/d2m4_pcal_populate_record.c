#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "d2m4_pcal_record.h"

void d2m4_pcal_populate_record(struct d2m4_pcal_record* rec, char* line_buffer)
{
    int ri;
    int dstr = 0;
    int nchars = 0;
    int mchars = 0;
    int nrecords = 0;
    struct d2m4_pcal_phasor* ph = NULL;

    sscanf(line_buffer, "%s%lf%lf%d%d%d%n", rec->antenna, &(rec->mjd), &(rec->tint), &dstr, &(rec->nchannels), &(rec->ntones), &mchars);
    nchars += mchars;
    nrecords = rec->nchannels * rec->ntones;

    #ifdef D2M4_PCAL_DEBUG
    printf("PCAL AP record data: %s %lf %lf %d %d %d \n", rec->antenna, rec->mjd, rec->tint, dstr, rec->nchannels, rec->ntones );
    #endif

    //now allocate space for all the records
    rec->phasors = (struct d2m4_pcal_phasor*) malloc( sizeof( struct d2m4_pcal_phasor)*nrecords );

    //loop over the total number of phasor records
    //this is assumed to be dstr*nchannels*ntones
    //(although not all of these may be filled with valid data (this is indicated by freq of -1))
    for(ri=0; ri<nrecords; ri++)
    {
        ph = &(rec->phasors[ri]);
        ph->dstr = dstr; //keep track of the original data stream at the individual phasor level
        //I believe in RJC's notation 'cquad' is the real component, and 'squad' is the imaginary component
        sscanf(line_buffer+nchars, "%d %c %lf%lf%n",  &(ph->frequency), &(ph->polarization), &(ph->real), &(ph->imag), &mchars);
        #ifdef D2M4_PCAL_DEBUG
        printf("phasor data: %d %d %c %lf %lf\n", ph->dstr, ph->frequency, ph->polarization, ph->real, ph->imag);
        #endif
        nchars += mchars;
    }
};
