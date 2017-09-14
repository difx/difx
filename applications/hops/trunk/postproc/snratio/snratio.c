/**************************************************************/
/*                                                            */
/*                  SNRATIO                                   */
/*                                                            */
/*   This program reads a file of predicted snrs for a vlbi   */
/*   sked and builds a structure of snrs for each scan.       */
/*                                                            */
/*   The pgm then reads a 2nd file which is an alist of the   */
/*   observed scans from which it gets the observed snr for   */
/*   each observed scan and stores it in the structure.       */
/*                                                            */
/*   Then finds means of snr for each source, each baseline/  */
/*   freq over all sources, and each bl/freq for each source. */
/*                                                            */
/*   Program is invoked by:  snratio <srnpred> <alist> [p]    */
/*   and output is to stdio - 3rd parameter is optional       */
/*   and if present entire scan structure will be output      */
/*                                                            */
/*                                                            */
/*      complile & link:  cc snratio.c -lm -o snratio         */
/*                        or                                  */
/*                        gcc snratio.c -lm -o snratio        */
/*                                                            */
/*                                                            */
/*   created: 31 Jan 1995 by JOM                              */
/*    mod 22 Feb 1995 jom - output source by baseline         */
/*    mod 23 Feb 1995 jom - write sors to snrout.xxxx         */
/*    mod 21 Sep 1995 jom - allow for 2 letter station        */
/*                          codes in predicts (requires file  */
/*                          stations.m to convert)            */
/*    mod 30 Nov 1995 jom - ignores data from all non-detect. */
/*           i.e. F's and 0's starting NA133. Previously only */
/*           ignored F's                                      */
/*    mod 28 Dec 1995 jom - add .001 to all snrs to handle 0s */
/*           in snr predicts - a sked artifact                */
/*                                                            */
/*    mod 30 May 1996 jom - do not use ratios > SNRMAX and    */
/*           give warning for ratios > SNRWARN                */
/*                                                            */
/*    mod 23 Nov 1999 jom - ignore snr > 5000                 */
/*                        - ignore qcode = E,B,               */
/*                                                            */
/*    mod 25 Jan 2001 jom - use new Afile input which is      */
/*                    the output of aflist on ccc             */
/*     scantime is now ddd-hhmm                               */
/*                                                            */
/*    mod 26 Jan 2001 jom - use new Alist output directly     */
/*                          as input for observed snr         */
/*                                                            */
/*    mod 19 Nov 2004 kak - new sked output and 16 stations   */
/*                                                            */
/*    mod 09 May 2007 kak - clean up                          */
/*                                                            */
/*    on ccc:   alist -o 8403.out /data2/8403                 */
/*                                                            */
/*    in sked: unit 8403.snr, xl snr, list                    */
/*                                                            */
/*    then:   snratio 8403.snr 8403.out > 8403.r              */
/*                                                            */
/*    mod 15 May 2006 ab/alr - strtok removed                 */
/*                                                            */
/*    mod 2015.3.2  rjc  fix behavior with new sked format    */
/**************************************************************/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 

#include "mk4_afio.h"

/*  max baselines, sources and scans defined:   */
/*  Note: actually baseline/freq, each scan has X baselines then S  */
#define MAXBL 600 
#define MAXSOR 256
#define MAXSC 2000
#define MAXLINLEN 6000  
#define SNRMAX 10.0
#define SNRWARN 5.0
#define OUTFILE "snr.out"

struct {                             /* struc for each scans snrs  */
       char scantime[9];             /* sched'd scan time ddd-hhmm */
       char source[9];               /* source                     */  
       float psnr[MAXBL];            /* predicted snr by bl X, S   */
       float osnr[MAXBL];            /* observed  snr "     "      */
       } scbl[MAXSC];

struct {                          /* struct for bl means by source */
       char source[13];           /* source                        */
       float blsor[MAXBL];        /* accum R's for each bl         */
       int bln[MAXBL];            /* accumulation count            */
       float sumx;                /* accum for all bl,this source  X */
       float sums;                /*  S                            */
       int nsumx;                 /* counts for sums                */
       int nsums;
       } sors[MAXSOR];

void addsec();
int parse_station_name (char *in_line, int nbl, char *ref, char *rem);

char progname[8] = "snratio";
int msglev = 3;

int main(int argc, char *argv[])
{

  FILE *fm, *fp2, *fp3, *fp, *fp_snr;

  char bl[MAXBL][2],scantime[13],source[9],line[MAXLINLEN],ch,*st,snrp[5],freq;
  char bl1,bl2,dtime[11],qcode,qerr,lasttime[13],outname[12],teststring[6],bl_style;
  char stc[MAXSC][3],st1c[MAXSC], ref[3], rem[3];
  float blsum[MAXBL],snro,ratio;
  int bln[MAXBL],n,nbl,nc,nsc,nsor,nline,nb,ns,type,exp,isnro,m,mm,colns;
  int return_code, done, stat_index, head_done, i;
  int offset;
 

/* open snr apriori file (created from sked) */

  if (argc < 3) {
    printf("No alist! \n   enter:  snratio <snrs> <alist> \n");
    exit(1);
  }

//printf("fp2 = %s fp3 = %s\n", argv[1],argv[2] );


  fp2 = fopen (argv[1], "r"); 
  if (fp2 == NULL) {
    printf("Trouble opening %s\n", argv[1]);
    exit(1);
  }

  fp3 = fopen (argv[2], "r");
  if (fp3 == NULL) {
    printf("Trouble opening %s\n", argv[2]);
    exit(1);
  }

  fp_snr = fopen(OUTFILE, "w");
  if (fp_snr == NULL) {
    printf("Ohohhh! Cannot open output file %s\n", OUTFILE);
    exit(1);
  }

  head_done = 0;

/* read the first line from the snr apriori file */

  fgets(line,MAXLINLEN,fp2);

/* search for the line containing as first word 'name' and count the number
 * of '-' */

  while (!feof(fp2) && !head_done) {
    if (strncmp(line,"name",4) == 0) {
      nbl = 0;
      for (i = 0 ; i < strlen(line) ; i++) 
        if (line[i] == '-') 
            {
            nbl++;
            if (nbl == 2)  // remember the offset to the first true baseline field
                offset = i - 2;
            }

/*remove one of the '-' since it is a divider between hh-mm and not between
 *baselines (e.g. Hh-Ho)*/

      nbl--;
/*for loop to read the station names from the snr apriori file */

      for (i = 1; i <= nbl; i++) {

/* function parse_station_name: read in the line and the number of '-' 
 * occurring in line (n) minus the first one => n = 2* number of baselines 
 * (accounting for both X and S band, return two letter code [2ch] (or in the 
 * old version of sked the one letter code [1ch]) reference
 * station (ref) and remote station (rem)*/

        return_code=parse_station_name (line, i, ref, rem);
        if (return_code == 1) {
          printf(" No baselines in input file\n");
          exit(1);
        }
/* first read 1 baseline to see if old (1ch) or new (2ch) stations */

        if (strlen(ref) > 1) { 
          bl_style = 'n';

/* open and read in memory file stations.m */

          if ((fm = fopen("stations.m","r")) == NULL) {
            if ((fm = fopen("/correlator/data/expn/stations.m","r")) == NULL) {
              printf("......can't find or open the stations.m file\n");
              exit(2);
            }
          }
          m = 0;
          while(fscanf(fm,"%2s %c\n",&stc[m], &st1c[m]) != EOF) {
            m++;
          }
          fclose(fm);
        } 

        else

/* found old style of baseline (1ch) */

          bl_style = 'o';

/* found new style of baseline: the two letter code must be substituted
 * with the appropiate one letter code for the comparison with the baseline
 * in the alist file*/

        if (bl_style == 'n') {
          done = 0;
          stat_index = 0;                
          while(!done && stat_index < m) {

/* compare the two letter code read from the function parse_station_name
 * with the one in stations.m file and replace the two letter code with 
 * the matching one letter code present in stations.m file. Write the results 
 * in the bl [MAXBL][2] array.
 * First for the X-band, then for the S-band */

            if (strcmp(ref,stc[stat_index]) == 0) {
              ref[0] = st1c[stat_index];
              ref[1] = '\0';
              done = 1; 
            }
            stat_index++;
          } 
          if (stat_index == m) {
            printf("\n\n ooooo> station %2s not in stations.m file\n",ref);
            exit(2);
          }

/* S-band turn */ 
          done = 0;
          stat_index = 0;                
          while(!done && stat_index < m) {
            if (strcmp(rem,stc[stat_index]) == 0) {
              rem[0] = st1c[stat_index];
              rem[1] = '\0';
              done = 1; 
            }
            stat_index++; 
          }
          if (stat_index == m) {
            printf("\n\n ooooo> station %2s not in stations.m file\n",rem);
            exit(2);
          }
          bl[i-1][0] = ref[0];
          bl[i-1][1] = rem[0];
        }


/* end of the new baseline style issue */

        else if (bl_style == 'o') {

/* if the style is old then simply copy the ref and rem into the bl[MAXBL][2]
 * array */

          bl[i-1][0] = ref[0];
          bl[i-1][1] = rem[0];         
        }

      } 
      head_done = 1;
    }
    fgets(line,MAXLINLEN,fp2);
  }   

/* test if the file snr apriori and stations.m have been read correctly */

/*
 for (i = 0; i < nbl ; i++)
     printf("%1c%1c ",bl[i][0],bl[i][1]);
     printf("\n %d freq-baselines, style=%1c\n",nbl,bl_style); 

*/

/* done with header and list of baselines if statement */


/* start reading the SNR values */

  done = 0;
  nsc = 0;

/* Open the snr apriori file and memorize in the structure scbl
 * the source name and the scan time */

  while (!feof(fp2) && !done) {       
/* Search for the word 'End' in the file, when found exit from the
 * loop */ 

    if (strncmp(line,"End",3) == 0) done = 1;

/* Search for lines which are not comments, but
 * starting with an alphanumeric char */

    if (!afile_comment(line) && isalnum(line[0]) && !done) {   
  
/*  read each line of predicted snr's  */

      sscanf(line,"%s %*2c%8s",scbl[nsc].source,scbl[nsc].scantime);
      // printf("source %s scan %s\n",scbl[nsc].source,scbl[nsc].scantime);

/* check for scantime identical to last. 
 * if so add 1 sec to duplicate what vsked does 
 * no longer used if(!strcmp(scbl[nsc].scantime,lasttime)) 
 * addsec(scbl[nsc].scantime); 
 * strcpy(lasttime,scbl[nsc].scantime); 
 * note: scantime is now ddd-hhmm    year and sec are ignored 
*/

/* Search and memorize the apriori snr in the structure scbl for
 * both X-band (first) and S-band*/

      for (n = 0; n < nbl/2; n++) {
        strncpy(snrp,line+offset+(n*6),4);
        // printf("X: n %d snrp %s\n",n,snrp);
        if (isdigit(snrp[3])) {
          sscanf(snrp,"%f",&scbl[nsc].psnr[n]);
          if (scbl[nsc].psnr[n] <= 0) 
            scbl[nsc].psnr[n] = 1;  /* handles predicted 0 snr's */
        }
        else
          scbl[nsc].psnr[n] = 0;           /* set blank snrs to 0 */
        scbl[nsc].osnr[n] = 0;            /* set all obs snrs to 0 */
      }


/* S-band */

      for (n = nbl/2; n < nbl; n++) {
        strncpy(snrp,line+offset+1+n*6,4);
        // printf("S: n %d snrp %s\n",n,snrp);
        if (isdigit(snrp[3])) {
          sscanf(snrp,"%f",&scbl[nsc].psnr[n]);
          if (scbl[nsc].psnr[n] <= 0) 
            scbl[nsc].psnr[n] = 1;  /* handles predicted 0 snr's */
        }
        else
        scbl[nsc].psnr[n] = 0;           /* set blank snrs to 0 */
        scbl[nsc].osnr[n] = 0;            /* set all obs snrs to 0 */
      }
      nsc++;
    }
    fgets(line,MAXLINLEN,fp2);
  }


/* done with the first while, close the snr apriori file */

  fclose(fp2);

/*Test:*/ 
/*     for(n=0; n<nsc; n++) {
* printf(" %8s   %8s ",scbl[n].source,scbl[n].scantime);
* for(nb=0; nb<nbl; nb++) printf("%3d ",scbl[n].psnr[nb]);
* printf("\n");}  
*/
    
/*------------Open Alist and read snr's for each baseline----------------*/

/*
  if ((fp=fopen(argv[2], "r")) == NULL) {
    printf(".....can't open Alist file\n");
    exit(2);
  }
*/
  
  ns = 0;      

  while (fgets(line,400,fp3) != NULL) {        /* read a line from alist */
   if (afile_comment(line)) continue;       /* ignore comment line */
    if (isalnum(line[0])) {                 /* ignore garbage line    */


      sscanf(line,"%*c%*s%d",&type);
      if (type == 2) {
        sscanf(line,"%*c%*s%d%*d%*d%*d%*d%d%8s%*c%*s%*d%*s%*d%s %1c%1c %1c%1c %1c%*2c %*2c%*d%*f%f", 
        &type,&exp,scantime,source,&bl1,&bl2,&qcode,&qerr,&freq,&snro);
/*
      printf(" %d     %d  %8s  %s    %1c%1c   %1c%1c  %1c    %f\n",type,
                exp,scantime,source,bl1,bl2,qcode,qerr,freq,snro); 
                 
*/

/* ignore non-detections and bad correlations */

        if (qerr != 'F' && qerr != 'E' && qerr != 'B' && qcode != '0'  
           && snro > 6.9 && snro < 5000.0) {

/*   strcat(scantime,dtime);  add yr+ddd-hhmmss to scantime */

          if(freq == 'X') {
            for (n=0; n<nbl/2 && ((bl[n][0] != bl1 || bl[n][1] != bl2) &&
                                  (bl[n][0] != bl2 || bl[n][1] != bl1)) ; n++);
            if (n == nbl/2) { 
               printf("\n >>could not match X baseline %c%c from alist\n",bl1,bl2);
               exit(3);
            } 
          }
          else {   
            for (n=nbl-1; n>(nbl/2-1) && ((bl[n][0] != bl1 || bl[n][1] != bl2) &&
                                          (bl[n][0] != bl2 || bl[n][1] != bl1)); n--);
            if (n == (nbl/2 - 1)) {
              printf("\n>>could not match S baseline %c%c from alist\n",bl1,bl2);
              exit(4);
            }
          }

/*    n=index of baseline found in afile, now find scantime match
*     in structure scbl[]                                             
*/


          for (ns=0; ns<nsc && (strcmp(scantime,scbl[ns].scantime) || 
            strcmp(source,scbl[ns].source)) ; ns++) {
           
//           printf(" Test %s %s %s %8s %d\n", scantime,scbl[ns].scantime, source,scbl[ns].source, ns); 
          }

          if (ns == nsc) {
            printf("\n >>>> could not match %s %s from alist\n",scantime,source);
            exit(4);
          }

/*   ns=matching scbl index    */

            isnro = snro;

/*   use snr from alist only if greater than existing value  */

          if(isnro > scbl[ns].osnr[n] )  scbl[ns].osnr[n] = isnro;
        }
      }    
    }
  }
  fclose(fp3);

  if (argv[3] != NULL) {
    printf(" SNR for %d by scan for baselines in column headings below, X-band then S-band:\n"
            " first line is predicted SNR, second is correlated SNR from alist\n\n",exp);
    for(nb=0; nb<nbl; nb++)
      printf("  %c%c  ",bl[nb][0],bl[nb][1]);
    printf("\n");
    for(n=0;n<nsc;n++) {
      printf(" %8s  %12s\n",scbl[n].scantime,scbl[n].source);
      for(nb=0;nb<nbl;nb++) printf("%5.1f ",scbl[n].psnr[nb]);
      printf("\n");
      for(nb=0;nb<nbl;nb++) printf("%5.1f ",scbl[n].osnr[nb]);
      printf("\n");
    }
  }

/*  scbl structure now contains predicted and observed snr's  Now find means */

  nsor = 0;
  for (nb=0; nb<nbl; nb++) {       /*  initialize bl accumulator */
    blsum[nb] = 0.0;
    bln[nb] = 0;
  }

  for (n=0; n<nsc; n++) {
     for (ns=0; ns<nsor && (strcmp(scbl[n].source,sors[ns].source)); ns++); 
     if (ns == nsor) {
       if (nsor == MAXSOR) {                   /* too many sources ! */
         printf("\n ****> CAUTION: number of sources exceeds %d !!!\n",nsor);
         exit(6);
       }
               
       strcpy (sors[ns].source,scbl[n].source);     /* add new source to sors */
       nsor++;
       sors[ns].sumx = 0.0;  sors[ns].nsumx = 0;     /* initialize sors    */
       sors[ns].sums = 0.0;  sors[ns].nsums = 0;
       for (nb=0; nb<nbl; nb++) {
         sors[ns].blsor[nb] = 0.0;
         sors[ns].bln[nb] = 0;
       }
     }

/* for scbl[n] the matching source is sors[ns]  */
/* now sum ratios from scbl (on bl that have obs) into various accumulators  */

     for (nb = 0; nb < nbl; nb++)
{
       if (scbl[n].osnr[nb]) {                /* is there an observation? */  
         if ( ! scbl[n].psnr[nb]) {
           printf("\n???scan %s %s %c%c has no predicted snr\n",
/*                   scbl[n].scantime,sors[ns],bl[nb][0],bl[nb][1]); */
                  scbl[n].scantime,scbl[n].source,bl[nb][0],bl[nb][1]);
           exit(7);
         }

         ratio = scbl[n].osnr[nb] / scbl[n].psnr[nb];
         if (ratio > SNRMAX) {
           if(nb<nbl/2) freq = 'X'; else freq = 'S';
           printf("\n %c%c (%c) on %s at %s: ratio = %5.1f  not used!\n",bl[nb][0],bl[nb][1],freq,
              scbl[n].source,scbl[n].scantime,ratio); 
         }
         else {
           blsum[nb]+= ratio;           bln[nb]++;
           sors[ns].blsor[nb]+= ratio;  sors[ns].bln[nb]++;
           if (nb<nbl/2) {
             sors[ns].sumx+= ratio;        sors[ns].nsumx++; 
             if (ratio > SNRWARN) {
               printf("\n %c%c (X) on %s at %s: ratio = %4.1f\n",
                      bl[nb][0],bl[nb][1],scbl[n].source,scbl[n].scantime,ratio);
             }

           }
           else {
             sors[ns].sums+= ratio;        sors[ns].nsums++;
             if (ratio > SNRWARN) {
               printf("\n %c%c (S) on %s at %s: ratio = %4.1f\n"
                      ,bl[nb][0],bl[nb][1],scbl[n].source,scbl[n].scantime,ratio);
             }

           }
       }
     }
}
  }

/*  calculate mean for each baseline and baseline means for each source  */


  fprintf(fp_snr,"MEAN RATIOS = Observed SNR / Predicted SNR  for exp no. %d\n",exp);
     
  fprintf(fp_snr,"\n ...by baseline, over all sources:\n\n");
  fprintf(fp_snr, " bl     X     n       S     n\n\n");

  for (nb = 0; nb < nbl; nb++) { 
    if(bln[nb])
      blsum[nb] /= bln[nb];
    }
    for (nb = 0; nb < nbl/2; nb++)
      fprintf(fp_snr," %c%c    %4.2f  %3d     %4.2f  %3d\n",bl[nb][0],bl[nb][1],blsum[nb],bln[nb], 
           blsum[nbl/2+nb],bln[nbl/2+nb]);
    fprintf(fp_snr,"\n");

    fclose(fp_snr);
    printf("\n\n ....by source:\n\n");
  
    for ( ns = 0; ns < nsor; ns++) {
       if(sors[ns].nsumx) sors[ns].sumx /= sors[ns].nsumx;
       if(sors[ns].nsums) sors[ns].sums /= sors[ns].nsums;
       printf("\n %s\n\n",sors[ns].source); 
       for (nb = 0; nb < nbl; nb++) {
         if(sors[ns].bln[nb])
           sors[ns].blsor[nb] /= sors[ns].bln[nb];
       } 
       printf(" bl     X      n      S      n\n");
       printf(" all   %4.2f  %3d     %4.2f  %3d\n",sors[ns].sumx,sors[ns].nsumx,
             sors[ns].sums,sors[ns].nsums); 
       for(nb=0; nb<nbl/2; nb++)
       printf(" %c%c    %4.2f  %3d     %4.2f  %3d\n",bl[nb][0],bl[nb][1],sors[ns].blsor[nb],
              sors[ns].bln[nb],sors[ns].blsor[nbl/2+nb],sors[ns].bln[nbl/2+nb]);
    }


/* print for each baseline, the ratios by source */

       printf("\n\n......by baseline, by source:\n\n");

       for(nb=0; nb<nbl/2; nb++)
       {
          printf("\n\n  baseline %c%c\n\n  source    X     n      S     n\n\n",bl[nb][0],bl[nb][1]);
      printf(" --all--   %4.2f  %3d    %4.2f  %3d\n",blsum[nb],bln[nb],blsum[nbl/2+nb],bln[nbl/2+nb]);
      for(ns=0; ns<nsor; ns++)
      {
             printf(" %8s  %4.2f  %3d    %4.2f  %3d\n",sors[ns].source,sors[ns].blsor[nb],sors[ns].bln[nb],
              sors[ns].blsor[nbl/2+nb],sors[ns].bln[nbl/2+nb]);
      }
       }

 /*  output ascii file of sors.source, .blsum  .bln  for all sources  */

       sprintf(outname,"snrout.%4d",exp);              /* make output name  snrout.xxxx */
       if((fp=fopen(outname,"w")) == NULL)
       {
      printf("\n.............can't open output file: %s",outname);
      exit(8);
       }
       fprintf(fp,"snrout for %d\n",exp);
       for(ns=0; ns<nsor; ns++)
       {  
      fprintf(fp,"%s\n",sors[ns].source);
      for(nb=0; nb<nbl/2; nb++)
      {
         if(sors[ns].bln[nb] || sors[ns].bln[nbl/2+nb])
         {
        fprintf(fp,"%c%c %4.2f %2d %4.2f %2d\n",bl[nb][0],bl[nb][1],sors[ns].blsor[nb],
                sors[ns].bln[nb],sors[ns].blsor[nbl/2+nb],sors[ns].bln[nbl/2+nb]);
         }
      }
       }


    return(0);
} 

void addsec(char ydt[])
{
  char inp[4];
  int dt[4],id,in,i,n;

   /*  printf("\n idential time: shift %s ",ydt);  */

  id = 0; in = 3;
  for(n=0;n<4;n++)
  {
    strncpy(inp,ydt+2+id+n*2,in);
    dt[n] = atoi(inp);
    id = 2; in = 2; inp[2] = '\0';
  }
                
  dt[3]++;                              /* add 1 sec */
  if(dt[3] == 60) {dt[3] = 0; dt[2]++;}   /* check for roll over */
  if(dt[2] == 60) {dt[2] = 0; dt[1]++;}
  if(dt[1] == 24) {dt[1] = 0; dt[0]++;}
                          
  ydt[2] = dt[0]/100 +'0'; dt[0] %= 100;
  id = 0;
  for (n=0; n<4; n++)
  {
    ydt[3+id+n*2] = dt[n] / 10 + '0';
    ydt[4+id+n*2] = dt[n] % 10 + '0';
    id = 1;
  }
  /* printf(" to %s \n",ydt); */
}

/* Function parse_station_name. Read in the line and the number of '-' 
 * occurring in line (n) minus the first one => n = 2* number of baselines 
 * (accounting for both X and S band, return two letter code [2ch] (or in the 
 * old version of sked the one letter code [1ch]) reference
 * station (ref) and remote station (rem) */

int parse_station_name (char *in_line, int n, char *ref, char *rem)
{

  int str_index = 0;
  int num_minus = 0;


  while (str_index < strlen(in_line) && num_minus < n + 1) {
    if (*(in_line+str_index) == '-'){
     num_minus++;
    }


    str_index++;    
  }

  if (str_index == strlen (in_line)){
    return(1);
  }
  else {
    strncpy (ref, in_line+str_index - 3, 2);
    strncpy (rem, in_line+str_index, 2);
    ref[2] = '\0';
    rem[2] = '\0';
    return(0);
  }
} 
