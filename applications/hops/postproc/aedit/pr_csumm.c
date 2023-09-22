/************************************************************************/
/*                                                                      */
/* This routine prints the contents of the csumm struct on the screen   */
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

int pr_csumm(void)
{
        extern struct datasumm csumm;
        extern int cscan, cflag;
        int i, j, year, day, hour, min, sec, type, ret;
        char qclist[20], buf[150];

        if((cscan-cflag) == 0) {
            msg("No unflagged corel data!",2);
            return(-1);
        }

        printf("\n\n\t\tSUMMARY OF UNFLAGGED DATA IN MEMORY\n");
        printf("\t\t-----------------------------------\n\n");
        printf("Total number of unflagged corel records = %d\n\n",cscan-cflag);
        int_to_time(csumm.begin,&year,&day,&hour,&min,&sec);
        printf("Earliest scan:       %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(csumm.end,&year,&day,&hour,&min,&sec);
        printf("Latest scan:         %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(csumm.proc_begin,&year,&day,&hour,&min,&sec);
        printf("Earliest procdate:   %d-%03d-%02d%02d\n",year,day,hour,min);
        int_to_time(csumm.proc_end,&year,&day,&hour,&min,&sec);
        printf("Latest procdate:     %d-%03d-%02d%02d\n",year,day,hour,min);
        printf("Stations present:    %s\n",csumm.stations);
        printf("Baselines present:   ");
        for (i=0; i<csumm.nbtq; i++) 
            {
            if ((i != 0) && (i%17 == 0)) printf ("\n\t\t");
            printf ("%s ", csumm.btq + 3*i);
            }
        printf ("\n");
        printf("Frequencies present: %s\n",csumm.frequencies);
        if(csumm.nexp < 15) {
            printf("Experiments present:");
            for(i=0;i<csumm.nexp;i++) printf("%5d",csumm.experiments[i]);
            printf("\n");
        }
        else printf("Experiments present: more than 15\n");
        printf("Sources present:     ");
        j = 0;
        for(i=0;i<csumm.nsource;i++) {
            if(csumm.source[i].count > 0) {
                j++;
                if(j%7 == 0) printf("\n\t");
                printf("%s ",csumm.source[i].name);
            }
        }
        printf("\nQuality code summary:\n");
        sprintf(qclist,"ABCDEFGH0123456789?");
        buf[0] = '\0';                  /* Make nice format */
        for(i=0;i<19;i++) {
            sprintf(buf,"%s%c ",buf,qclist[i]);
            if(csumm.qcodes[i] >= 10000) sprintf(buf,"%s    ",buf);
            else if(csumm.qcodes[i] >= 1000) sprintf(buf,"%s   ",buf);
            else if(csumm.qcodes[i] >= 100) sprintf(buf,"%s  ",buf);
            else if(csumm.qcodes[i] >= 10) sprintf(buf,"%s ",buf);
        }
        printf("\t%s\n",buf);
        printf("\t");
        for(i=0;i<19;i++) printf("%d ",csumm.qcodes[i]);
        printf("\n\nThere are %d flagged records present\n\n",cflag);
        return(0);
}
