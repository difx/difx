/************************************************************************/
/*                                                                      */
/* This routine prints the contents of the rsumm struct on the screen   */
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
pr_rsumm()
{
        extern struct datasumm rsumm, fsumm;
        extern int rscan, rflag;
        int i, j, year, day, hour, min, sec, type, ret;
        char buf[150];

        if((rscan-rflag) == 0) {
            msg("No unflagged root data!",2);
            return(-1);
        }

        printf("\n\n\t\tSUMMARY OF UNFLAGGED DATA IN MEMORY\n");
        printf("\t\t-----------------------------------\n\n");
        printf("Total number of unflagged root records = %d\n\n",rscan-rflag);
        int_to_time(rsumm.begin,&year,&day,&hour,&min,&sec);
        printf("Earliest scan:       %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(rsumm.end,&year,&day,&hour,&min,&sec);
        printf("Latest scan:         %d-%03d-%02d%02d%02d\n",year,day,hour,min,sec);
        int_to_time(rsumm.proc_begin,&year,&day,&hour,&min,&sec);
        printf("Earliest procdate:   %d-%03d-%02d%02d\n",year,day,hour,min);
        int_to_time(rsumm.proc_end,&year,&day,&hour,&min,&sec);
        printf("Latest procdate:     %d-%03d-%02d%02d\n",year,day,hour,min);
        printf("Stations present:    %s\n",rsumm.stations);
        if(fsumm.nexp < 15) {
            printf("Experiments present:");
            for(i=0;i<rsumm.nexp;i++) printf("%5d",rsumm.experiments[i]);
            printf("\n");
        }
        else printf("Experiments present: more than 15\n");
        printf("Sources present:     ");
        j = 0;
        for(i=0;i<rsumm.nsource;i++) {
            if(rsumm.source[i].count > 0) {
                j++;
                if(j%7 == 0) printf("\n\t");
                printf("%s ",rsumm.source[i].name);
            }
        }
        printf("\n\nThere are %d flagged records present\n\n",rflag);
        return(0);
}
