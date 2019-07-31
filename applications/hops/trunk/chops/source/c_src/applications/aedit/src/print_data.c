/************************************************************************/
/*                                                                      */
/*                                                                      */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created  by CJL                                                      */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include "aedata.h"
#include "aedit.h"

int print_data (fringearray *fdata)
{
        int year, day, hour, min, sec;
        fringesum *datum;

        datum = &(fdata->data);
        int_to_time(datum->time_tag,&year,&day,&hour,&min,&sec);
        printf("SCAN: %d%03d-%02d%02d    BASELINE: %s    SOURCE: %s    FREQ: %c\n",
        year,day,hour,min,datum->baseline,datum->source,datum->freq_code);
        printf("----------------------------------------------------------------\n");

        printf("EXPERIMENT NUMBER:    %d\n",datum->expt_no);
        printf("RECORDING MODE:       %c\n",datum->mode);
        if (datum->version == 1)
            printf("ARCHIVE FILE:         A%04d\n",datum->archiv);
        int_to_time(datum->procdate,&year,&day,&hour,&min,&sec);
        printf("PROCESSING DATE:      %d%03d-%02d%02d\n",year,day,hour,min);
        if (datum->version == 1)
            printf("HP1000 FILENAME:      <%s\n",datum->fname);
        printf("EXTENT NUMBER:        %d\n\n",datum->extent_no);

        printf("FRNGE QUALITY CODE:   %c\n",mk3_qf (datum));
        printf("SCAN LENGTH SECONDS:  %d\n",datum->length);
        printf("NUMBER FREQ CHANNELS: %d\n",datum->no_freq);
        printf("CORRELATION AMP:      %f\n",datum->amp);
        printf("CORRELATION SNR:      %#5.4g\n",datum->snr);
        printf("CORRELATION PHASE:    %5.1f\n",datum->resid_phas);
        printf("SINGLEBAND DELAY:     %f\n",datum->sbdelay);
        printf("MULTIBAND DELAY:      %f\n",datum->mbdelay);
        printf("DELAY RATE:           %f\n\n",datum->delay_rate);
        if(fdata->flag != 0) printf("THIS DATA POINT FLAGGED, value %05o\n\n",fdata->flag);
        return(0);
}
