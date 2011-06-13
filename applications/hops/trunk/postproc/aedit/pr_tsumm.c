/************************************************************************/
/*                                                                      */
/* This routine prints the contents of the tsumm struct on the screen   */
/* in a nice format. It assumes summ_data() has been called             */
/*                                                                      */
/*      Inputs:         none                                            */
/*                                                                      */
/*      Output:         none                                            */
/*                                                                      */
/* Created 9 April 1989 by CJL                                          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include "summary.h"

int
pr_tsumm()
{
        extern struct datasumm tsumm;
        extern int tscan, tflag;
        int i, j, year, day, hour, min, sec, type, ret;
        char qclist[20], buf[150];

        if((tscan-tflag) == 0) {
            msg("No unflagged triangle data!",2);
            return(-1);
        }

        printf("\n\n\t\tSUMMARY OF UNFLAGGED TRIANGLE DATA IN MEMORY\n");
        printf("\t\t-----------------------------------\n\n");
        printf("Total number of unflagged triangle records = %d\n\n",tscan-tflag);
        int_to_time(tsumm.begin,&year,&day,&hour,&min,&sec);
        printf("Earliest scan:       %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(tsumm.end,&year,&day,&hour,&min,&sec);
        printf("Latest scan:         %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        printf("Stations present:    %s\n",tsumm.stations);
        printf("Triangles present:   ");
        for (i=0; i<tsumm.nbtq; i++) 
            {
            if ((i != 0) && (i%14 == 0)) printf ("\n\t\t");
            printf ("%s ", tsumm.btq + 4*i);
            }
        printf ("\n");
        printf("Frequencies present: %s\n",tsumm.frequencies);
        printf("SNR extrema:         %#5.4g  %#5.4g\n",tsumm.snrmin,tsumm.snrmax);
        if(tsumm.nexp < 15) {
            printf("Experiments present:");
            for(i=0;i<tsumm.nexp;i++) printf("%5d",tsumm.experiments[i]);
            printf("\n");
        }
        else printf("Experiments present: more than 15\n");
        printf("Sources present:     ");
        j = 0;
        for(i=0;i<tsumm.nsource;i++) {
            if(tsumm.source[i].count > 0) {
                j++;
                if(j%7 == 0) printf("\n\t");
                printf("%s ",tsumm.source[i].name);
            }
        }
        printf("\nQuality code summary:\n");
        sprintf(qclist,"ABCDEFGH0123456789?");
        buf[0] = '\0';                  /* Make nice format */
        for(i=0;i<19;i++) {
            sprintf(buf,"%s%c ",buf,qclist[i]);
            if(tsumm.qcodes[i] >= 10000) sprintf(buf,"%s    ",buf);
            else if(tsumm.qcodes[i] >= 1000) sprintf(buf,"%s   ",buf);
            else if(tsumm.qcodes[i] >= 100) sprintf(buf,"%s  ",buf);
            else if(tsumm.qcodes[i] >= 10) sprintf(buf,"%s ",buf);
        }
        printf("\t%s\n",buf);
        printf("\t");
        for(i=0;i<19;i++) printf("%d ",tsumm.qcodes[i]);
        printf("\n\nThere are %d flagged records present\n\n",tflag);
        return(0);
}
