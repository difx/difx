/**
 * Tool to generate SIGPROC filterbank header with
 * the command line inputs that are provided.
 *
 * Used in conjunction with 'filterbankprep.py'
 * and the mark5access filterbank program 'm5fb'.
 *
 * Written by A. Seymour
 * $ gcc -Wall m5fb_makeheader.c -o m5fb_makeheader
 */

/* filterbank.h - include file for filterbank and related routines */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int strings_equal (const char *, const char *);

void error_message(char *message) /*includefile */
{
  fprintf(stderr,"ERROR: %s\n",message);
  exit(1);
}

/* 
	some useful routines written by Jeff Hagen for swapping
	bytes of data between Big Endian and  Little Endian formats:

Big Endian - most significant byte in the lowest memory address, which
is the address of the data.  

Since TCP defines the byte ordering for network data, end-nodes must
call a processor-specific convert utility (which would do nothing if
the machine's native byte-ordering is the same as TCP's) that acts on
the TCP and IP header information only. In a TCP/IP packet, the first
transmitted data is the most significant byte.

Most UNIXes (for example, all System V) and the Internet are Big
Endian. Motorola 680x0 microprocessors (and therefore Macintoshes),
Hewlett-Packard PA-RISC, and Sun SuperSPARC processors are Big
Endian. The Silicon Graphics MIPS and IBM/Motorola PowerPC processors
are both Little and Big Endian (bi-endian).

Little Endian - least significant byte in the lowest-memory address,
which is the address of the data. 

The Intel 80X86 and Pentium and DEC Alpha RISC processors are Little Endian. 
Windows NT and OSF/1 are Little Endian. 
Little Endian is the less common UNIX implementation. 

The term is used because of an analogy with the story Gulliver's
Travels, in which Jonathan Swift imagined a never-ending fight between
the kingdoms of the Big-Endians and the Little-Endians, whose only
difference is in where they crack open a hard-boiled egg.

*/
void swap_short( unsigned short *ps ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = ( unsigned char *)ps;
  t = pc[0];
  pc[0] = pc[1];
  pc[1] = t;
}

void swap_int( int *pi ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pi;

  t = pc[0];
  pc[0] = pc[3];
  pc[3] = t;

  t = pc[1];
  pc[1] = pc[2];
  pc[2] = t;
}

void swap_float( float *pf ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pf;

  t = pc[0];
  pc[0] = pc[3];
  pc[3] = t;

  t = pc[1];
  pc[1] = pc[2];
  pc[2] = t;
}

void swap_ulong( unsigned long *pi ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pi;

  t = pc[0];
  pc[0] = pc[3];
  pc[3] = t;

  t = pc[1];
  pc[1] = pc[2];
  pc[2] = t;
}

void swap_long( long *pi ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pi;

  t = pc[0];
  pc[0] = pc[3];
  pc[3] = t;

  t = pc[1];
  pc[1] = pc[2];
  pc[2] = t;
}

void swap_double( double *pd ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pd;

  t = pc[0];
  pc[0] = pc[7];
  pc[7] = t;

  t = pc[1];
  pc[1] = pc[6];
  pc[6] = t;

  t = pc[2];
  pc[2] = pc[5];
  pc[5] = t;

  t = pc[3];
  pc[3] = pc[4];
  pc[4] = t;

}

void swap_longlong( long long *pl ) /* includefile */
{
  unsigned char t;
  unsigned char *pc;

  pc = (unsigned char *)pl;

  t = pc[0];
  pc[0] = pc[7];
  pc[7] = t;

  t = pc[1];
  pc[1] = pc[6];
  pc[6] = t;

  t = pc[2];
  pc[2] = pc[5];
  pc[5] = t;

  t = pc[3];
  pc[3] = pc[4];
  pc[4] = t;
}

int little_endian() /*includefile*/
{
  char *ostype;

  if((ostype = (char *)getenv("OSTYPE")) == NULL )
    error_message("environment variable OSTYPE not set!");
  if (strings_equal(ostype,"linux")) return 1;
  if (strings_equal(ostype,"hpux")) return 0;
  if (strings_equal(ostype,"solaris")) return 0;
  if (strings_equal(ostype,"darwin")) return 0;
  fprintf(stderr,"Your OSTYPE environment variable is defined but not recognized!\n");
  fprintf(stderr,"Consult and edit little_endian in swap_bytes.c and then recompile\n");
  fprintf(stderr,"the code if necessary... Contact dunc@naic.edu for further help\n");
  exit(0);
}

int big_endian() /*includefile*/
{
  return (!little_endian());
}


/* input and output files and logfile (filterbank.monitor) */
FILE *input, *output, *logfile;
char  inpfile[80], outfile[80];

/* global variables describing the data */
char rawdatafile[80], source_name[80];
int machine_id, telescope_id, data_type, nchans, nbits, nifs, scan_number,
  barycentric,pulsarcentric; /* these two added Aug 20, 2004 DRL */
int is_bary, is_pulsarcentric;
double tstart,mjdobs,tsamp,fch1,foff,refdm,az_start,za_start,src_raj,src_dej;
double gal_l,gal_b,header_tobs,raw_fch1,raw_foff;
int nbeams, ibeam;
/* added 20 December 2000    JMC */
double srcl,srcb;
double ast0, lst0;
long wapp_scan_number;
char project[8];
char culprits[24];
double analog_power[2];

/* added frequency table for use with non-contiguous data */
double frequency_table[4096]; /* note limited number of channels */
long int npuls; /* added for binary pulse profile format */

double time_offset;

/* global variables describing the operating mode */
float start_time, final_time, clip_threshold;

int obits, sumifs, headerless, headerfile, swapout, invert_band;
int compute_spectra, do_vanvleck, hanning, hamming, zerolagdump;
int headeronly;
char ifstream[8];

void send_string(char *string) /* includefile */
{
  int len;
  len=strlen(string);
  if (swapout) swap_int(&len);
  fwrite(&len, sizeof(int), 1, output);
  if (swapout) swap_int(&len);
  fwrite(string, sizeof(char), len, output);
  /*fprintf(stderr,"%s\n",string);*/
}

void send_float(char *name,float floating_point) /* includefile */
{
  send_string(name);
  if (swapout) swap_float(&floating_point);
  fwrite(&floating_point,sizeof(float),1,output);
  /*fprintf(stderr,"%f\n",floating_point);*/
}

void send_double (char *name, double double_precision) /* includefile */
{
  send_string(name);
  if (swapout) swap_double(&double_precision);
  fwrite(&double_precision,sizeof(double),1,output);
  /*fprintf(stderr,"%f\n",double_precision);*/
}

void send_int(char *name, int integer) /* includefile */
{
  send_string(name);
  if (swapout) swap_int(&integer);
  fwrite(&integer,sizeof(int),1,output);
  /*fprintf(stderr,"%d\n",integer);*/
}

void send_long(char *name, long integer) /* includefile */
{
  send_string(name);
  if (swapout) swap_long(&integer);
  fwrite(&integer,sizeof(long),1,output);
  /*fprintf(stderr,"%ld\n",integer);*/
}

void send_coords(double raj, double dej, double az, double za) /*includefile*/
{
  if ((raj != 0.0) || (raj != -1.0)) send_double("src_raj",raj);
  if ((dej != 0.0) || (dej != -1.0)) send_double("src_dej",dej);
  if ((az != 0.0)  || (az != -1.0))  send_double("az_start",az);
  if ((za != 0.0)  || (za != -1.0))  send_double("za_start",za);
}

int strings_equal (const char *string1, const char *string2) /* includefile */
{
  if (!strcmp(string1,string2)) {
    return 1;
  }
  return 0;
}

int main (int argc, char *argv[])
{
  int i;
  output=stdout;
  strcpy(source_name,"ATJ1745-2900");
  machine_id=0;     /* you will need to edit+align with aliases.c in sigproc */
  telescope_id=10;      /* ditto */
  fch1=21956.0;        /* centre freq of highest channel */
  foff=-.125;          /* channel bw <0 means that it's in highest freq first */
  nchans=2048;
  src_raj=174540.164;  /* RA in hhmmss.s (J2000) */
  src_dej=-290029.818; /* Dec in ddmmss.s (J2000) */
  az_start=0.0;        /* telescope azimuth at start of obs (deg) */
  za_start=0.0;        /* telescope zenith at start of obs (deg) */
  tstart=50000.0;      /* MJD of start time (topocentric) */
  tsamp=32.0e-3;       /* sampling time (s) */
  nbits=8;             /* number of bits per sample */
  nifs=1;
  is_bary=0;
  is_pulsarcentric=0;

  i=1;
  while (i<argc) {
    if (strings_equal(argv[i],"-fch1")) {
      fch1=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-foff")) {
      foff=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-nchans")) {
      nchans=atoi(argv[++i]);
    } else if (strings_equal(argv[i],"-tstart")) {
      tstart=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-az_start")) {
      az_start=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-za_start")) {
      za_start=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-tsamp")) {
      tsamp=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-src_raj")) {
      src_raj=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-src_dej")) {
      src_dej=atof(argv[++i]);
    } else if (strings_equal(argv[i],"-source_name")) {
      strcpy(source_name,argv[++i]);
    } else if (strings_equal(argv[i],"-nifs")) {
      nifs=atoi(argv[++i]);
    } else if (strings_equal(argv[i],"-telescope_id")) {
      telescope_id=atoi(argv[++i]);
    } else if (strings_equal(argv[i],"-bary")) {
      is_bary=1;
    } else if (strings_equal(argv[i],"-pcentric")) {
      is_pulsarcentric=1;
    } else if (strings_equal(argv[i],"-nbits")) {
      nbits=atoi(argv[++i]);
    }
    i++;
  }

  /* broadcast the header parameters to the output stream */
  send_string("HEADER_START");
  if (!strings_equal(source_name,"")) {
    send_string("source_name");
    send_string(source_name);
  }
  send_int("machine_id",machine_id);
  send_int("telescope_id",telescope_id);
  send_coords(src_raj,src_dej,az_start,za_start);
  send_int("data_type",1);  // 1=filterbank, 2=time series, ...
  send_double("fch1",fch1); // f[ch] = fch1 + ch * foff
  send_double("foff",foff);
  send_int("nchans",nchans);
  send_int("nbits",nbits);
  send_int("nbeams",1);
  send_int("ibeam",1);
  send_double("tstart",tstart);
  send_double("tsamp",tsamp);
  send_int("nifs",nifs);
  send_int("barycentric",is_bary);
  send_int("pulsarcentric",is_pulsarcentric);
  send_string("HEADER_END");

  return 0;
}
