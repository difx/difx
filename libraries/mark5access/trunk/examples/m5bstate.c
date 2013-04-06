//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5bstate";
const char author[]  = "Alessandra Bertarini";
const char version[] = "1.1";
const char verdate[] = "2011 Sep 12";

int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	printf("\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	signal(SIGINT, oldsiginthand);
}

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 state counter.  Can use VLBA, Mark3/4, and Mark5B "
		"formats using the\nmark5access library.\n\n");
	printf("Usage : %s <infile> <dataformat> <nframes> [<offset>]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <nframes> is the number of frames to bstate-erize\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");

	return 0;
}

int bstate(const char *filename, const char *formatname, int nframes,
	   long long offset)
{
	struct mark5_stream *ms;
        double **data;
        long **bstate;
	int i, j, k, status;
	int chunk, nif, nstates;
	long long total, unpacked;
	double a, x;
        int sum;       
        double *gfact;

/* a is required for Haystack gain calculation*/
        a = 8 * (M_PI - 3) / (3 * M_PI * (4 - M_PI));


	total = unpacked = 0;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("Error: problem opening %s\n", filename);

		return 0;
	}

	mark5_stream_print(ms);

       /*53601 is the max no. of frames for Mark5B that would overflow bstate storage in worst case */
        if(nframes <= 0 || nframes > 53601)
        {
                printf("\nWARNING: nframes out of range, setting to 1000\n\n");
                nframes = 1000;
        }


        chunk = ms->framesamples;
	nif = ms->nchan;

        /* bstate 2nd dim. is either 2 for the 1bit: ++ -- or 4 for the 2 bits ++ + - -- */
        if(ms->nbit == 1) 
        {
                nstates = 2;
        }
        else if(ms->nbit == 2)
        {
                nstates = 4;
        }
        else 
        {
                printf("Error: invalid bit sampling %d, must be either 1 or 2\n", ms->nbit);

                return 0;
        }

        data = (double **)malloc(nif*sizeof(double *));
        bstate = (long **)malloc(nif*sizeof(long *));
        /*Haystack gain's calculation*/
        gfact = (double *)malloc(nif*sizeof(double));
 
        for(i = 0; i < nif; i++)
        {
                data[i] = (double *)malloc((chunk+2)*sizeof(double)); 
                bstate[i] = (long *)malloc((nstates)*sizeof(long)); 
        }

        /* initialize bstate variable to zeroes*/
        for(i = 0; i < nif; i++)
        {
                for(j = 0; j < nstates; j++)
                {
                       bstate[i][j] = 0;
                }
        }
        
 
	for(j = 0; j < nframes; j++)
	{
		if(die)
		{
			break;
		}

		status = mark5_stream_decode_double(ms, chunk, data);

               if(status < 0)
                {
                        break;
                }
                else
                {
                        total += chunk;
                        unpacked += status;
                }

                if(ms->consecutivefails > 5)
                {
                        break;
                }


                for(i = 0; i < nif; i++) 
                {

                       for(k = 0; k < chunk; k++)
                       {
                        /*       printf("%lf\n", data[i][k]); */
                        /* now start to count the states from data[i][k]*/

                               if (ms->nbit == 1)
                               {
                                      if (data[i][k] > 0) bstate[i][1]++;
                                      if (data[i][k] < 0) bstate[i][0]++;
                               }
                               else if (ms->nbit == 2)
                               {
                                      if (data[i][k] > 0 && data[i][k] < 2.) bstate[i][2]++;
                                      if (data[i][k] > 2.) bstate[i][3]++;
                                      if (data[i][k] < 0 && data[i][k] > -2.) bstate[i][1]++;
                                      if (data[i][k] < -2.) bstate[i][0]++;
                               }
 
                       }
                } 

	}

	fprintf(stderr, "%Ld / %Ld samples unpacked\n", unpacked, total);

        /* header of the output bstate table based on Haystack bstate output*/
        if (ms->nbit == 1)
        {
                 printf("\nCh    -      +         -      +     gfact\n");
        }
        else if (ms->nbit == 2)
        {
                 printf("\nCh    --      -     +     ++        --      -      +     ++     gfact\n");
        }

	/* normalize */
	for(i = 0; i < nif; i++)
	{
                printf("%2d ", i);
                sum = 0.0;
		for(j = 0; j < nstates; j++)
		{
			sum += bstate[i][j];
		}
                for(j = 0; j < nstates; j++)
                {
                       printf("%7ld ", bstate[i][j]);
                }
                printf("    ");
                for(j = 0; j < nstates; j++)
                {
                       printf("%5.1f  ", (float)bstate[i][j]/sum * 100.);
                }
/* Haystack gain correction calculation */

                x = (double) (bstate[i][1] + bstate[i][2]) / sum;
                gfact[i] = sqrt (-4 / (M_PI * a) - log (1 - x*x)
                           + 2 * sqrt (pow (2 / (M_PI * a) + log (1 - x*x) / 2, 2)
                           - log (1-x*x)/a)) / 0.91;
                printf("%5.2lf", gfact[i]);  
                printf("\n");
	}


	for(i = 0; i < nif; i++)
	{
		free(data[i]);
		free(bstate[i]);
	}
	free(data);
	free(gfact);
	free(bstate);

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int nframes;

	oldsiginthand = signal(SIGINT, siginthand);

	if(argc == 2)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<11;
		char *buffer;
		FILE *in;
		int r;

		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			return usage(argv[0]);
		}

		in = fopen(argv[1], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open file '%s'\n", argv[1]);
			return 0;
		}

		buffer = malloc(bufferlen);

		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error, buffer read failed.\n");
		}
		
		else
		{
			mf = new_mark5_format_from_stream(
				new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);

			mf = new_mark5_format_from_stream(
				new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);
		}

		fclose(in);
		free(buffer);

		return 0;
	}

	else if(argc < 4)
	{
		return usage(argv[0]);
	}

	nframes  = atol(argv[3]);

	if(argc > 4)
	{
		offset=atoll(argv[4]);
	}

	bstate(argv[1], argv[2], nframes, offset);

	return 0;
}

