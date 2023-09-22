/*
   split scans from one big filterbank/kurtosis file into separate files

   Randall Wayth. July 2010.

   This is a simple utility to read a file of filerbank/kurtosis data
   and write out a separate file for each scan that is present in the
   original file. Output files are the input file with ".scanXXX"
   appended where XXX is the scan index.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include "difx_fb.h"

#define PROGNAME "split_fb_scan"
#define MAX_SCANS 128
#define MAX_CHANS 1024

/* function prototypes */
void openOutfile(char *filename,int scan_id);
void parse_cmdline(const int argc, char * const argv[]);
void print_usage();

/* global vars */
char *infilename=NULL;
FILE *outfiles[MAX_SCANS];
FILE *fpin=NULL;
int debug=0;

int main(int argc, char *argv[]) {
    int i,v,n_chans;
    float data[MAX_CHANS];
    ChunkHeader header;

    if (argc < 2) print_usage();

    parse_cmdline(argc,argv);

    for (i=0; i< MAX_SCANS; i++) outfiles[i] = NULL;

    /* open input file */
    if ((fpin=fopen(infilename,"r"))==NULL) {
        fprintf(stderr,"Cannot open input file %s\n",infilename);
        exit(1);
    }

    /* process all packets */
    /* read a packet header */
    while ((fread(&header,sizeof(ChunkHeader),1,fpin)) ==1) {
        n_chans = header.n_channels;
        assert(n_chans < MAX_CHANS);

        /* read the packet data */
        if (fread(data,sizeof(float)*n_chans,1,fpin) != 1) continue;    // EOF or something.

        /* new scan? Open a new output file */
        if (outfiles[header.scan_id] == NULL) openOutfile(infilename,header.scan_id);

        /* write the packet to the appropriate outfile */
        v = fwrite(&header,sizeof(ChunkHeader),1,outfiles[header.scan_id]);
	if(v != 1) {
            fprintf(stderr,"Error writing for scan %d",header.scan_id);
            exit(1);
	}

        v = fwrite(data,sizeof(float)*n_chans,1,outfiles[header.scan_id]);
	if(v != 1) {
            fprintf(stderr,"Error writing for scan %d",header.scan_id);
            exit(1);
        }
    }

    /* tidy up */
    return 0;
}


void openOutfile(char *filename,int scan_id) {
    char temp[FILENAME_MAX];
    sprintf(temp,"%s.scan%03d",filename,scan_id);
    if ((outfiles[scan_id]=fopen(temp,"w"))==NULL) {
        fprintf(stderr,"Unable to open output file name <%s>\n",temp);
        exit(1);
    }    
    if (debug) fprintf(stdout,"Creating new scan file: %s\n",temp);
}

/*****************************
******************************/
void parse_cmdline(const int argc, char * const argv[]) {
    char *optstring = "di:";
    int result=0;

    while ( (result = getopt(argc, argv, optstring)) != -1 ) {
        switch (result) {
          case 'i': infilename = optarg;
            break;
          case 'd': debug = 1;
            break;
          default:
              fprintf(stderr,"unknown option: %c\n",result);
              print_usage();
        }
    }
}

/*****************************
******************************/
void print_usage() {
    fprintf(stderr,"Usage: %s [options]\n\n",PROGNAME);
    fprintf(stderr,"options are:\n");
    fprintf(stderr,"-i filename\tThe name of the input file. No default.\n");
    fprintf(stderr,"-d         \tenable debugging output\n");
    exit(1);
}
