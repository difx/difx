/************************************************************************/
/*                                                                      */
/* This routine prints the contents of the fsumm struct on the screen   */
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
#include "aedit.h"

int pr_fsumm(void)
{
        extern struct datasumm fsumm;
        extern int fscan, fflag;
        int i, j, year, day, hour, min, sec, type, ret;
        char qclist[20], buf[150];

        if((fscan-fflag) == 0) {
            msg("No unflagged fringe data!",2);
            return(-1);
        }

        printf("\n\n\t\tSUMMARY OF UNFLAGGED DATA IN MEMORY\n");
        printf("\t\t-----------------------------------\n\n");
        printf("Total number of unflagged fringe records = %d\n\n",fscan-fflag);
        int_to_time(fsumm.begin,&year,&day,&hour,&min,&sec);
        printf("Earliest scan:       %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(fsumm.end,&year,&day,&hour,&min,&sec);
        printf("Latest scan:         %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(fsumm.proc_begin,&year,&day,&hour,&min,&sec);
        printf("Earliest procdate:   %d-%03d-%02d%02d\n",year,day,hour,min);
        int_to_time(fsumm.proc_end,&year,&day,&hour,&min,&sec);
        printf("Latest procdate:     %d-%03d-%02d%02d\n",year,day,hour,min);
        printf("Stations present:    %s\n",fsumm.stations);
        printf("Baselines present:   ");
        for (i=0; i<fsumm.nbtq; i++) 
            {
            if ((i != 0) && (i%17 == 0)) printf ("\n\t\t");
            printf ("%s ", fsumm.btq + 3*i);
            }
        printf ("\n");
        printf("Frequencies present: %s\n",fsumm.frequencies);
        printf("Polarizations present:   %s\n", fsumm.polarizations);
        printf("SNR extrema:         %#5.4g  %#5.4g\n",fsumm.snrmin,fsumm.snrmax);
        if(fsumm.nexp < 15) {
            printf("Experiments present:");
            for(i=0;i<fsumm.nexp;i++) printf("%5d",fsumm.experiments[i]);
            printf("\n");
        }
        else printf("Experiments present: more than 15\n");
        printf("Sources present:     ");
        j = 0;
        for(i=0;i<fsumm.nsource;i++) {
            if(fsumm.source[i].count > 0) {
                j++;
                if(j%7 == 0) printf("\n\t");
                printf("%s ",fsumm.source[i].name);
            }
        }
        printf("\nQuality code summary:\n");
        sprintf(qclist,"ABCDEFGH0123456789?");
        buf[0] = '\0';                  /* Make nice format */
        for(i=0;i<19;i++) {
            sprintf(buf,"%s%c ",buf,qclist[i]);
            if(fsumm.qcodes[i] >= 10000) sprintf(buf,"%s    ",buf);
            else if(fsumm.qcodes[i] >= 1000) sprintf(buf,"%s   ",buf);
            else if(fsumm.qcodes[i] >= 100) sprintf(buf,"%s  ",buf);
            else if(fsumm.qcodes[i] >= 10) sprintf(buf,"%s ",buf);
        }
        printf("\t%s\n",buf);
        printf("\t");
        for(i=0;i<19;i++) printf("%d ",fsumm.qcodes[i]);
        printf("\n\nThere are %d flagged records present\n\n",fflag);
        return(0);
}
