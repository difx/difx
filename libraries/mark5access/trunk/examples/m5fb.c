/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & Chris Phillips            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: m5fb.c 0001 2012-05-25 01:15:33Z RichardDOdson $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision: 0001 $
// $Author: RichardDodson $
// $LastChangedDate: 2012-05-25 09:15:33 +0800 (Fri, 25 May 2012) $
//
//============================================================================

// Change this to configure detection, if possible
#define USEGETOPT 1

#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <fftw3.h>
#include <math.h>
#include <signal.h>
#include "../mark5access/mark5_stream.h"

#if USEGETOPT
#include <getopt.h>
#endif

const char program[] = "m5fb";
const char author[]  = "Richard Dodson";
//  Copied extensively from m5spec by Walter Brisken & Chris Phillips
const char version[] = "1.0";
const char verdate[] = "20130606";

int die = 0;

typedef enum {VLBA=1, DBBC, NOPOL} polmodetype;

struct hd_info { int nchan; int nint; float freq; float max; float min; float mean; int nbit; char polid[16]; char *source;} ;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	printf("\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	signal(SIGINT, oldsiginthand);
}

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 filterbank.  Can use VLBA, Mark3/4, and Mark5B formats using the\nmark5access library.\n\n");
	printf("Usage : %s <infile> <dataformat> <nchan> <dint> <outfile> [<offset>]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <nchan> is the number of channels to make per IF\n\n");
	printf("  <dint> is the integration time in microseconds\n\n");
	printf("  <outfile> is the name of the output file\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");
	printf("\n\n\n");
	printf("The following options are supported\n\n");
	printf("    -dbbc      Assume dBBC polarisation order (all Rcp then all Lcp)\n\n");
	printf("    -nopol     Do not compute cross pol terms\n\n");
	printf("    -I         Compute intensity correlation\n\n");
	printf("    -a         Write ascii output\n\n");
	printf("    -p         String for pol terms. RLRL etc\n\n");
	printf("    -i         String for IF terms. ULUL etc\n\n");
	printf("    -help      This list\n\n");
}

int harvestComplexData(struct mark5_stream *ms, double **spec, fftw_complex **zdata, fftw_complex **zx, int nchan, int nint, int chunk, long long *total, long long *unpacked)
{
	fftw_plan *plan;
	double complex **cdata;
	int j,status,fftmode=0;

	if(nchan<0)
	{
		nchan = -nchan;
		fftmode = 1;
	}
	plan = (fftw_plan *)malloc(ms->nchan*sizeof(fftw_plan));
	cdata = (double complex **)malloc(ms->nchan*sizeof(double complex *));
	for(j = 0; j < ms->nchan; ++j)
	{
		cdata[j] = (double complex*)malloc(nchan*sizeof(double complex));
		plan[j] = fftw_plan_dft_1d(nchan, cdata[j], zdata[j], FFTW_FORWARD, FFTW_MEASURE);
	}

	for(j = 0; j < nint; ++j)
	{
		int i;

		if(die)
		{
   			status=-1;
		//	return status;
			break;
		}

		status = mark5_stream_decode_double_complex(ms, chunk , (double complex**)cdata);
		if(status < 0)
		{
			break;
		}
		else
		{
			*total += chunk;
			*unpacked += status;
		}

		if(ms->consecutivefails > 5)
		{
			break;
		}

		for(i = 0; i < ms->nchan; ++i)
		{
			int fi;
			if (fftmode)
			{
				for(fi=0; fi<nchan;++fi)
				{
					cdata[i][fi]  = cabs(cdata[i][fi]);
					cdata[i][fi] *= cdata[i][fi];
				}
			}
			/* FFT */
			fftw_execute(plan[i]);
		}

		for(i = 0; i < ms->nchan; ++i)
		{
			int c;

			for(c = 0; c < nchan; ++c)
			{
				double re, im;

				re = creal(zdata[i][c]);
				im = cimag(zdata[i][c]);
				spec[i][c] += re*re + im*im;
			}
		}

		for(i = 0; i < ms->nchan/2; ++i)
		{
			int c;

			for(c = 0; c < nchan; ++c)
			{
				zx[i][c] += zdata[2*i][c]*~zdata[2*i+1][c];
			}
		}
	}

	for(j = 0; j < ms->nchan; ++j)
	{
		fftw_destroy_plan(plan[j]);
		free(cdata[j]);
	}
	free(plan);
	free(cdata);

	return status;
}

int harvestRealData(struct mark5_stream *ms, double **spec, fftw_complex **zdata, fftw_complex **zx, int nchan, int nint, int chunk, long long *total, long long *unpacked, polmodetype polmode)
{
	static fftw_plan *plan=NULL;
	static double **data=NULL;
	int j, status,fftmode=0;

	if(nchan<0)
	{
		nchan = -nchan;
		fftmode = 1;
	}
	if(!data)
	{
		plan = (fftw_plan *)malloc(ms->nchan*sizeof(fftw_plan));
		data = (double **)malloc(ms->nchan*sizeof(double *));
		for(j = 0; j < ms->nchan; ++j)
		{
			data[j] = (double *)malloc((chunk+2)*sizeof(double));
			plan[j] = fftw_plan_dft_r2c_1d(nchan*2, data[j], zdata[j], FFTW_MEASURE);
		}
	}

	for(j = 0; j < nint; ++j)
	{
		int i;

		if(die)
		{
   			status=-1;
		//	return status;
			break;
		}

		status = mark5_stream_decode_double(ms, chunk, data);
		if(status < 0)
		{
			break;
		}
		else
		{
			*total += chunk;
			*unpacked += status;
		}

		if(ms->consecutivefails > 5)
		{
			break;
		}

		for(i = 0; i < ms->nchan; ++i)
		{
			int fi;
			if (fftmode)
			{
				for(fi=0; fi<nchan;++fi)
				{
					data[i][2*fi] = data[i][2*fi]*data[i][2*fi] + data[i][2*fi+1]*data[i][2*fi+1];
					data[i][2*fi+1] = 0;
				}
			}
			/* FFT */
			fftw_execute(plan[i]);
		}

		for(i = 0; i < ms->nchan; ++i)
		{
			int c;

			for(c = 0; c < nchan; ++c)
			{
				double re, im;

				re = creal(zdata[i][c]);
				im = cimag(zdata[i][c]);
				spec[i][c] += re*re + im*im;
			}
		}

		if (polmode==VLBA)
		{
			for(i = 0; i < ms->nchan/2; ++i)
			{
				int c;

				for(c = 0; c < nchan; ++c)
				{
					zx[i][c] += zdata[2*i][c]*~zdata[2*i+1][c];
				}
			}
		}
		else if (polmode==DBBC)
		{
			for(i = 0; i < ms->nchan/2; ++i)
			{
				int c;

				for(c = 0; c < nchan; ++c)
				{
					zx[i][c] += zdata[i][c]*~zdata[i+ms->nchan/2][c];
				}
			}
		}
	}

	if (status<0) {
		for(j = 0; j < ms->nchan; ++j)
		{
			fftw_destroy_plan(plan[j]);
			free(data[j]);
		}
		free(plan);
		free(data);
	}

	printf("\t\t\tTime Processed Appx: %.4e s\r",ms->framens*1e-9*ms->framenum);
	return status;
}

int print_header(struct mark5_stream *ms, struct hd_info hi,FILE *fo)
{
	char tmp[32];
	int h, m, s, i;

	h = ms->sec;
	h = h/3600;
	i = ms->sec-h*3600;
	m = i/60;
	s = i-m*60;

	strncpy(tmp,"KVNTN",5);
	//i=(int) strlen(ms->streamname);
	//strncpy(tmp,(char *)strchr((char *)(ms->streamname+i+3),'_')+1,32);
	//(strchr(tmp,'_'))[0]='\0';
/*
  fprintf(fo,"Site            : %s\n",tmp);
  fprintf(fo,"Observer        : %s\n","Nemo");
  fprintf(fo,"Proposal        : %s\n","XX");
  fprintf(fo,"Array Mode      : %s\n","SD");
  fprintf(fo,"Observing Mode  : \n");
  fprintf(fo,"Date            : %s\n","0/0/0");//mjd2date(ms->mjd+56000));
  fprintf(fo,"Num Antennas    : \n");
  fprintf(fo,"Antennas List   : \n");
  fprintf(fo,"Num Channels    : %d\n",hi.nchan);
  fprintf(fo,"Channel width   : %f\n",ms->samprate/2.0E6/hi.nchan);
  fprintf(fo,"Frequency Ch.1  : %f\n",hi.freq);
  fprintf(fo,"Sampling Time   : %d\n",hi.nint);
  fprintf(fo,"Num bits/sample : 8\n");
  fprintf(fo,"Data Format     : integer binary, little endian\n");
  fprintf(fo,"Polarizations   : LL\n");
  fprintf(fo,"MJD             : %d\n",ms->mjd+56000);
  fprintf(fo,"UTC             : %02d:%02d:%02d\n",h,m,s);
  fprintf(fo,"Source          : J1745-2900\n");
  fprintf(fo,"Coordinates     : 17:45:40.0383, -29:00:28.06\n");
  fprintf(fo,"Coordinate Sys  : J2000\n");
  fprintf(fo,"Drift Rate      : 0.0, 0.0\n");
  fprintf(fo,"Obs. Length     : \n");
  fprintf(fo,"Bad Channels    : \n");
  fprintf(fo,"Bit shift value : \n");
*/
	printf("Default Header\n");
	printf("Site            : %s\n",tmp);
	printf("Observer        : %s\n","Nemo");
	printf("Proposal        : %s\n","XX");
	printf("Array Mode      : %s\n","SD");
	printf("Observing Mode  : \n");
	printf("Date            : %s\n","0/0/0");//mjd2date(ms->mjd+56000));
	printf("Num Antennas    : \n");
	printf("Antennas List   : \n");
	printf("Num Channels    : %d\n",hi.nchan);
	printf("Channel width   : %f\n",ms->nchan*ms->samprate/2.0E6/hi.nchan);
	printf("Frequency Ch.1  : %f\n",hi.freq);
	printf("Sampling Time   : %d\n",hi.nint);
	printf("Num bits/sample : 8\n");
	printf("Data Format     : integer binary, little endian\n");
	printf("Polarizations   : %s\n",hi.polid);
	printf("MJD             : %d\n",ms->mjd+56000);
	printf("UTC             : %02d:%02d:%02d\n",h,m,s);
	printf("Source          : J1745-2900\n");
	printf("Coordinates     : 17:45:40.0383, -29:00:28.06\n");
	printf("Coordinate Sys  : J2000\n");
	printf("Drift Rate      : 0.0, 0.0\n");
	printf("Obs. Length     : \n");
	printf("Bad Channels    : \n");
	printf("Bit shift value : \n");

	i = ftell(fo);
	bzero(tmp,32);

	return 0;
}

int spec(const char *filename, const char *formatname, int nchan, int nint, const char *outfile, long long offset, polmodetype polmode, int output_bin, char* ifid, char* polid)
{
	struct mark5_stream *ms;
	double **spec;
	fftw_complex **zdata, **zx;
	int i, c, first=1;
	int status=0;
	int chunk,count;
	long long total, unpacked;
	FILE *out;
	double f, sum, max, min;
	double x, y;
	int docomplex;
	int fftmode=0;
	struct hd_info hinfo;

	count =0 ;
	total = unpacked = 0;
	if(nchan < 0)
	{
		nchan = -nchan;
		fftmode = 1;
	}


	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("Error: problem opening %s\n", filename);

		return EXIT_FAILURE;
	}

	mark5_stream_print(ms);

	if(ms->complex_decode != 0)
	{
		printf("Complex decode\n");
		docomplex = 1;
		chunk = nchan;
	}
	else
	{
		docomplex = 0;
		chunk = 2*nchan;
	}
	printf("Using %d u-seconds integration, ",nint);
	nint *= (float) (ms->samprate*1E-6/chunk);
	printf("Which is %d samples (%d samples per FFT bin)\n",nint*nchan,nint);

	out = fopen(outfile, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", outfile);
		delete_mark5_stream(ms);

		return EXIT_FAILURE;
	}

	spec = (double **)malloc(ms->nchan*sizeof(double *));
	zdata = (fftw_complex **)malloc(ms->nchan*sizeof(fftw_complex *));
	zx = (fftw_complex **)malloc((ms->nchan/2)*sizeof(fftw_complex *));
	for(i = 0; i < ms->nchan; ++i)
	{
		spec[i] = (double *)calloc(nchan, sizeof(double));
		zdata[i] = (fftw_complex *)malloc((nchan+2)*sizeof(fftw_complex));
	}
	for(i = 0; i < ms->nchan/2; ++i)
	{
		zx[i] = (fftw_complex *)calloc(nchan, sizeof(fftw_complex));
	}

	while(status>=0)
	{
		if(docomplex)
		{
			status=harvestComplexData(ms, spec, zdata, zx, (1-fftmode*2)*nchan, nint, chunk, &total, &unpacked);
		}
		else
		{
			status=harvestRealData(ms, spec, zdata, zx, (1-fftmode*2)*nchan, nint, chunk, &total, &unpacked, polmode);
		}

		//fprintf(stderr, "Pass %d: %Ld / %Ld samples unpacked\n", ++count, unpacked, total);

		// Needs to be read in some how
		/*float uplow[16]={22315.00,-22347.00,22347.00,-22379.00,
				 42893.00,-42925.00,42925.00,-42957.00,
				 86353.00,-86385.00,86385.00,-86417.00,
				 86417.00,-86449.00,86449.00,-86481.00};
		float uplow[16]={21700.00,-21732.00,21732.00,-21764.00,
							 21764.00,-21796.00,21796.00,-21828.00,
							 21828.00,-21860.00,21860.00,-21892.00,
							 21892.00,-21924.00,21924.00,-21956.00};*/
		float *uplow;
		uplow=(float *) malloc(sizeof(float)*strlen(ifid));
		for(i=0; i < strlen(ifid); i++)
		{
			uplow[i] = (ifid[i]=='U')? 1 : -1;
		}

		char *tmp;
		int nif=ms->nchan; // nif is introduced so that we can have blank IFs or short reads
		if(strlen(ifid) < nif)
		{
			nif = strlen(ifid);
			if(first) { printf("Shortening IFs to %d\n",nif); }
		}
		//char *tmp;tmp=calloc(nchan*4,sizeof(char));

		if(!output_bin || first)
		{ // normalize across all ifs/channels
			max = sum = 0.0;
			min = 1e32;
			for(c = 0; c < nchan; ++c)
			{
				for(i = 0; i < ms->nchan; ++i)
				{
					sum += spec[i][c];
					if (spec[i][c]>max) max=spec[i][c];
					if (spec[i][c]<min) min=spec[i][c];
				}
			}
			f = ms->nchan*nchan/sum;
			// Add 10% to max and min
			if (max>0) {max *=1.1;}
			else {max *=0.9;}
			if (min<0) {min *=1.1;}
			else {min *=0.9;}
		}

		if (first) { printf("Scale factor used is %E\n",f); }

		if (output_bin)
		{
			if (first)
			{
				tmp=calloc(nchan*nif,sizeof(char));
				//	    hinfo.nchan=nchan;
				hinfo.nchan=nchan*nif;
				hinfo.nint=nint/(ms->samprate*1E-6/chunk);
				hinfo.freq=(uplow[ms->nchan-1]<0)
					? -uplow[ms->nchan-1] /* Lower Side */
					: uplow[ms->nchan-1]+ms->samprate/2.0E6; /* Upper */
				hinfo.max=max;
				hinfo.min=min;
				hinfo.mean=1/f;
				hinfo.nbit=8;
				strncpy(hinfo.polid,polid,nif);

				first=print_header(ms,hinfo,out);
			}

			bzero(tmp,nchan*nif*sizeof(char));
			for(i = 0; i<ms->nchan && i<nif; ++i)
			{
				for(c = 0; c < nchan; ++c)
				{
					//for(i = 3; i < 0; --i) for(c = 0; c < nchan; ++c) {
					//int j= (uplow[i]<0)? (i+1)*nchan-c-1:i*nchan+c;
					int j= (uplow[i]>0) ? ((nif-i)*nchan-c-1) : ((nif-1-i)*nchan+c);
					// Top out for scale
					if (spec[i][c]>max) spec[i][c]=max;
					if (spec[i][c]<min) spec[i][c]=min;
					tmp[j] = 255*((spec[i][c]-min)/(max-min));
				}
			}
			ssize_t nwr = fwrite(tmp,sizeof(char),nchan*nif,out);
			printf("%ld/%d samples: \r", (long)nwr, count++);
			//fwrite(tmp,sizeof(char),nchan*4,out);
		}
		else
		{
			for(c = 0; c < nchan; ++c)
			{
				fprintf(out, "%f ", (double)c*ms->samprate/(2.0e6*nchan));
				for(i = 0; i < ms->nchan; ++i)
				{
					fprintf(out, " %f", f*spec[i][c]);
				}

				if(polmode != NOPOL)
				{
					for(i = 0; i < ms->nchan/2; ++i)
					{
						x = creal(zx[i][c])*f;
						y = cimag(zx[i][c])*f;
						fprintf(out, "  %f %f", sqrt(x*x+y*y), atan2(y, x));
					}
				}
				fprintf(out, "\n");
			}
		}

		// Zero the integrations
		for(i = 0; i < ms->nchan; ++i)
		{
			bzero(spec[i],nchan*sizeof(double));
			bzero(zdata[i],(nchan+2)*sizeof(fftw_complex));
		}
		for(i = 0; i < ms->nchan/2; ++i)
		{
			bzero(zx[i],nchan*sizeof(fftw_complex));
		}

	} // end of file

	fclose(out);

	for(i = 0; i < ms->nchan; ++i)
	{
		free(zdata[i]);
		free(spec[i]);
	}
	for(i = 0; i < ms->nchan/2; ++i)
	{
		free(zx[i]);
	}
	free(zx);
	free(zdata);
	free(spec);
	delete_mark5_stream(ms);

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int nchan, nint,nif,npol;
	int output_bin=1;
	int retval,fftmode=0;
	polmodetype polmode = VLBA;
	char *ifid="ULULULULULULULUL",*polid="LLLLLLLLLLLLLLLL",*tmp; // 16 IFs swapping Upper Lower, All LHC
#if USEGETOPT
	int opt;
	struct option options[] = {
		{"dbbc", 0, 0, 'B'},
		{"nopol", 0, 0, 'P'},
		{"intensity", 0, 0, 'I'},
		{"ascii", 0, 0, 'a'},
		{"polid", 1, 0, 'p'},
		{"ifid", 1, 0, 'i'},
		{"help", 0, 0, 'h'},
		{0, 0, 0, 0}
	};

	while ((opt = getopt_long_only(argc, argv, "BPIahp:i:", options, NULL)) != EOF)
	{
		switch (opt)
		{
			case 'B': // DBBC Pol mode (all Rcp then all LCP)
			polmode = DBBC;
			printf("Assuming DBBC polarisation order\n");
			break;

			case 'P': // Don't compute cross pols
				polmode = NOPOL;
				printf("Not computing cross pol terms\n");
				break;

			case 'I': // compute Intensity FFT
				fftmode = 1;
				printf("Computing Intensity FFT\n");
				break;

			case 'a': // Ascii output
				output_bin=0;
				printf("Outputing ASCII file\n");
				break;

			case 'p': // polstring
				polid=strdup(optarg);
				printf("Pol ID string: %s\n",polid);
				break;

			case 'i': // polstring
				ifid=strdup(optarg);
				printf("IF ID string: %s\n",ifid);
				break;

			case 'h': // help
				usage(argv[0]);
				return EXIT_SUCCESS;
				break;
		}
	}

#else
	int optind=1;
#endif

	oldsiginthand = signal(SIGINT, siginthand);


	if((argc-optind) == 1)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<11;
		char *buffer;
		FILE *in;
		int r;

		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}

		in = fopen(argv[1], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open file '%s'\n", argv[1]);

			return EXIT_FAILURE;
		}

		buffer = malloc(bufferlen);

		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error, buffer read failed.\n");

			fclose(in);
			free(buffer);

			return EXIT_FAILURE;
		}
		else
		{
			mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);

			mf = new_mark5_format_from_stream(
					new_mark5_stream_memory(buffer,
					bufferlen/2)
				);

			print_mark5_format(mf);
			delete_mark5_format(mf);
		}

		fclose(in);
		free(buffer);

		return EXIT_SUCCESS;
	}

	else if(argc-optind < 5)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	nchan = atol(argv[optind+2]);
	nint  = atol(argv[optind+3]);
	npol=strlen(polid);
	nif=strlen(ifid);
	printf("Number of IFs labelled: %d\nNumber of POLs labelled: %d\n",nif,npol);
	if(npol != nif)
	{
		printf("These don't match assuming missing are same as last");
		if(npol < nif)
		{
			tmp=strdup(polid);
			polid=malloc(nif*sizeof(char));strcpy(polid,tmp);
			for(retval=npol; retval < nif; retval++)
			{
				polid[retval]=polid[npol-1];
			}
		}
		if(npol > nif)
		{
			tmp=strdup(ifid);
			ifid=malloc(npol*sizeof(char));
			strcpy(ifid,tmp);
			for(retval=nif; retval < npol; retval++)
			{
				ifid[retval]=ifid[npol-1];
			}
		}
	}
	if(nint <= 0)
	{
		nint = 2000000000L;
	}

	if((argc-optind) > 5)
	{
		offset=atoll(argv[optind+5]);
	}

	retval = spec(
		argv[optind], argv[optind+1], (1-fftmode*2)*nchan, nint,
		argv[optind+4], offset, polmode, output_bin, ifid, polid
	);

	return retval;
}
