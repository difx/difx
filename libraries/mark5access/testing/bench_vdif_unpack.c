/***************************************************************************
 *   Benchmark for the VDIF unpackers in format_vdif.c                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Times the unpackers the way mpifxcorr drives them: an unpacker stream with
 * headers and mark5_unpack[_complex]_with_offset() walking through a buffer
 * of VDIF frames, unpacking a fixed number of samples per call into the same
 * (cache resident) output arrays.  Per-frame overhead in the library
 * (validation, blanker) is therefore included, as it is in real use.
 *
 * Repeats are interleaved across modes (all modes once, then all modes
 * again, ...) and the best time for each mode is reported, so a burst of
 * system activity or a spell on an efficiency core spoils one repeat of many
 * modes rather than every repeat of one mode.
 *
 * By default each channel's output array is a separate allocation, as in
 * mpifxcorr.  When those are a power of 2 bytes apart (e.g. 16 KiB, which is
 * what macOS malloc does for one 8000 byte 1-bit frame per call) every
 * channel maps to the same L1 cache set and modes with many channels slow
 * down greatly.  -a stagger places the arrays in one block with an extra
 * cache line between channels, which isolates the speed of the unpacker
 * itself.
 *
 * Results can be written as CSV (-o) and compared with an earlier run (-b)
 * to measure the effect of changes to the unpackers.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#ifdef __APPLE__
#include <pthread.h>
#include <sys/qos.h>
#endif
#include "vdif_common.h"

#define FPS 10000

struct options
{
	const char *patterns;
	const char *csvout;
	const char *baseline;
	int databytes;
	int bufbytes;		/* approximate size of input frame buffer */
	int callsamples;	/* samples per channel per unpack call; 0 = one frame */
	double mintime;		/* seconds per timing repeat */
	int repeats;
	int count;		/* also benchmark high-state counters */
	int stagger;		/* output arrays in one block, offset to avoid cache set aliasing */
};

/* One thing to time: a decoder or a counter for one mode */
struct benchcase
{
	const struct vdif_mode *m;
	int docount;
	char name[40];
	int callsamples;
	double best;		/* best seconds per call so far */
};

struct baseline
{
	char name[40];
	double msps;
};

static void usage(const char *pgm)
{
	printf("Usage: %s [options]\n\n", pgm);
	printf("Benchmarks every VDIF unpacker in format_vdif.c.\n\n");
	printf("Options:\n");
	printf("  -m <patterns>  only modes matching comma separated glob patterns,\n");
	printf("                 e.g. -m 'real-2bit-*,cplx-8bit-4ch'\n");
	printf("  -f <bytes>     VDIF payload bytes per frame [default 8000]\n");
	printf("  -B <bytes>     input buffer size [default 4194304]\n");
	printf("  -n <samples>   samples per channel per unpack call [default: one frame]\n");
	printf("  -t <seconds>   minimum time per repeat [default 0.05]\n");
	printf("  -r <n>         repeats per mode, best is reported [default 5]\n");
	printf("  -c             also benchmark the 2-bit high-state counters\n");
	printf("  -a <layout>    output arrays: 'separate' allocations per channel [default]\n");
	printf("                 or 'stagger' (one block, avoids cache set aliasing)\n");
	printf("  -o <file>      write results as CSV\n");
	printf("  -b <file>      compare against a CSV written earlier with -o\n");
	printf("  -h             this help\n");
}

static int load_baseline(const char *filename, struct baseline *base, int maxbase)
{
	FILE *in;
	char line[512];
	int n = 0;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open baseline file %s\n", filename);

		return -1;
	}
	while(fgets(line, sizeof(line), in) && n < maxbase)
	{
		char *comma;

		if(line[0] == '#' || strncmp(line, "mode,", 5) == 0)
		{
			continue;
		}
		comma = strchr(line, ',');
		if(!comma || comma - line >= (int)sizeof(base[n].name))
		{
			continue;
		}
		memcpy(base[n].name, line, comma - line);
		base[n].name[comma - line] = 0;
		base[n].msps = atof(comma + 1);
		++n;
	}
	fclose(in);

	return n;
}

static const struct baseline *find_baseline(const struct baseline *base, int nbase, const char *name)
{
	int i;

	for(i = 0; i < nbase; ++i)
	{
		if(strcmp(base[i].name, name) == 0)
		{
			return base + i;
		}
	}

	return 0;
}

static float **alloc_output(int nchan, size_t nfloat, int stagger)
{
	float **out;
	int c;

	out = (float **)calloc(nchan, sizeof(float *));
	if(stagger)
	{
		/* round up to a cache line, then add one more per channel */
		size_t stride = ((nfloat + 15) & ~(size_t)15) + 16;
		float *block;

		if(posix_memalign((void **)&block, 64, stride*nchan*sizeof(float)) != 0)
		{
			return 0;
		}
		memset(block, 0, stride*nchan*sizeof(float));
		for(c = 0; c < nchan; ++c)
		{
			out[c] = block + stride*c;
		}
	}
	else
	{
		for(c = 0; c < nchan; ++c)
		{
			out[c] = (float *)calloc(nfloat, sizeof(float));
		}
	}

	return out;
}

static void free_output(float **out, int nchan, int stagger)
{
	int c;

	if(stagger)
	{
		free(out[0]);
	}
	else
	{
		for(c = 0; c < nchan; ++c)
		{
			free(out[c]);
		}
	}
	free(out);
}

/* Time one repeat of one case, updating bc->best.  The frame buffer is
 * shared by all modes: the unpacker does not check the nchan/nbit fields
 * of the headers, only that frames are valid.  Returns 0 on success. */
static int time_case(struct benchcase *bc, const struct options *o, const unsigned char *buf, int nframes)
{
	const struct vdif_mode *m = bc->m;
	struct mark5_stream *ms;
	float **out;
	unsigned int *highstates;
	int framebytes = o->databytes + VDIF_HEADER_BYTES;
	int framesamples = vdif_mode_framesamples(m, o->databytes);
	int callsamples = bc->callsamples;
	int nchunk, chunk, pass;
	double t = 0.0;
	long long ncalls = 0;

	nchunk = (int)((long long)nframes*framesamples/callsamples);
	if(nchunk < 1)
	{
		return -1;
	}

	ms = new_mark5_stream_absorb(new_mark5_stream_unpacker(0), vdif_mode_new_format(m, o->databytes, FPS));
	if(!ms)
	{
		fprintf(stderr, "%s: cannot create unpacker\n", m->name);

		return -1;
	}

	out = alloc_output(m->nchan, (size_t)callsamples*(m->iscomplex ? 2 : 1), o->stagger);
	highstates = (unsigned int *)calloc(m->nchan, sizeof(unsigned int));

	for(pass = 0; pass < 2; ++pass)		/* pass 0 warms caches and branch predictors */
	{
		double t0 = now_seconds();

		ncalls = 0;
		do
		{
			for(chunk = 0; chunk < nchunk; ++chunk)
			{
				int offset = chunk*callsamples;

				if(bc->docount)
				{
					/* counters have no with_offset entry point: position by frame */
					mark5_unpack(ms, buf + (size_t)(offset/framesamples)*framebytes, out, 0);
					ms->readposition = (int)((long long)(offset % framesamples)*m->blockbits/8);
					mark5_stream_count_high_states(ms, callsamples, highstates);
				}
				else if(m->iscomplex)
				{
					mark5_unpack_complex_with_offset(ms, buf, offset, (mark5_float_complex **)out, callsamples);
				}
				else
				{
					mark5_unpack_with_offset(ms, buf, offset, out, callsamples);
				}
			}
			ncalls += nchunk;
			t = now_seconds() - t0;
		}
		while(pass == 1 && t < o->mintime);
	}

	if(t/ncalls < bc->best)
	{
		bc->best = t/ncalls;
	}

	free_output(out, m->nchan, o->stagger);
	free(highstates);
	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	struct vdif_mode modes[MAX_VDIF_MODES];
	static struct benchcase cases[2*MAX_VDIF_MODES];
	static struct baseline base[2*MAX_VDIF_MODES];
	struct options o;
	FILE *csv = 0;
	unsigned char *buf;
	struct vdif_mode headermode;
	int nmodes, ncase = 0, nbase = 0, nframes, i, opt, pass, rep;
	double logsum = 0.0;
	int nratio = 0;
	uint64_t rng = 12345;

	memset(&o, 0, sizeof(o));
	o.databytes = 8000;
	o.bufbytes = 4 << 20;
	o.mintime = 0.05;
	o.repeats = 5;

	while((opt = getopt(argc, argv, "m:f:B:n:t:r:ca:o:b:h")) != -1)
	{
		switch(opt)
		{
		case 'm': o.patterns = optarg; break;
		case 'f': o.databytes = atoi(optarg); break;
		case 'B': o.bufbytes = atoi(optarg); break;
		case 'n': o.callsamples = atoi(optarg); break;
		case 't': o.mintime = atof(optarg); break;
		case 'r': o.repeats = atoi(optarg); break;
		case 'c': o.count = 1; break;
		case 'a':
			if(strcmp(optarg, "stagger") == 0)
			{
				o.stagger = 1;
			}
			else if(strcmp(optarg, "separate") != 0)
			{
				fprintf(stderr, "Unknown output layout %s\n", optarg);

				return EXIT_FAILURE;
			}
			break;
		case 'o': o.csvout = optarg; break;
		case 'b': o.baseline = optarg; break;
		case 'h': usage(argv[0]); return EXIT_SUCCESS;
		default: usage(argv[0]); return EXIT_FAILURE;
		}
	}

	if(o.databytes <= 0 || o.databytes % 8 != 0)
	{
		fprintf(stderr, "Frame payload size must be a positive multiple of 8\n");

		return EXIT_FAILURE;
	}

#ifdef __APPLE__
	/* ask to be run on a performance core */
	pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0);
#endif

	nmodes = vdif_enumerate_modes(modes, MAX_VDIF_MODES);
	nmodes = vdif_filter_modes(modes, nmodes, o.patterns);

	for(pass = 0; pass <= o.count; ++pass)
	{
		for(i = 0; i < nmodes; ++i)
		{
			struct benchcase *bc = cases + ncase;
			int gran = vdif_mode_granularity(&modes[i]);

			if(pass == 1 && !modes[i].hascount)
			{
				continue;
			}
			if(o.databytes % vdif_mode_min_databytes(&modes[i]) != 0)
			{
				fprintf(stderr, "%s: frame size %d is not a multiple of %d; skipping\n",
					modes[i].name, o.databytes, vdif_mode_min_databytes(&modes[i]));
				continue;
			}
			bc->m = &modes[i];
			bc->docount = pass;
			bc->best = 1.0e30;
			snprintf(bc->name, sizeof(bc->name), "%s%s", pass ? "count-" : "", modes[i].name);
			bc->callsamples = o.callsamples > 0 ? o.callsamples : vdif_mode_framesamples(&modes[i], o.databytes);
			bc->callsamples -= bc->callsamples % gran;
			if(bc->callsamples <= 0)
			{
				bc->callsamples = gran;
			}
			++ncase;
		}
	}
	if(ncase == 0)
	{
		fprintf(stderr, "No modes selected\n");

		return EXIT_FAILURE;
	}

	if(o.baseline)
	{
		nbase = load_baseline(o.baseline, base, 2*MAX_VDIF_MODES);
		if(nbase < 0)
		{
			return EXIT_FAILURE;
		}
	}

	/* one buffer of valid frames, plus a spare since decoders step onto the
	 * frame following the last one they finish */
	nframes = o.bufbytes/(o.databytes + VDIF_HEADER_BYTES);
	if(nframes < 2)
	{
		nframes = 2;
	}
	vdif_mode_init(&headermode, 0, 2, 1);
	buf = (unsigned char *)malloc((size_t)(nframes + 1)*(o.databytes + VDIF_HEADER_BYTES));
	vdif_generate_frames(buf, &headermode, o.databytes, nframes + 1, FPS, 0, &rng);

	printf("VDIF unpacker benchmark: %d byte frames, %d frame buffer, best of %d x %.2f s, %s output arrays\n",
		o.databytes, nframes, o.repeats, o.mintime, o.stagger ? "staggered" : "separate");
	if(o.callsamples > 0)
	{
		printf("Unpacking %d samples per channel per call\n", o.callsamples);
	}
	else
	{
		printf("Unpacking one frame per call\n");
	}

	for(rep = 0; rep < o.repeats; ++rep)
	{
		fprintf(stderr, "\rrepeat %d/%d", rep + 1, o.repeats);
		for(i = 0; i < ncase; ++i)
		{
			if(time_case(&cases[i], &o, buf, nframes) != 0)
			{
				cases[i].best = 0.0;
			}
		}
	}
	fprintf(stderr, "\r                \r");

	if(o.csvout)
	{
		csv = fopen(o.csvout, "w");
		if(!csv)
		{
			fprintf(stderr, "Cannot open %s for write\n", o.csvout);

			return EXIT_FAILURE;
		}
		fprintf(csv, "mode,Msamples_per_s,input_MB_per_s,output_GB_per_s,ns_per_sample\n");
	}

	printf("\n%-24s %10s %10s %10s %10s", "mode", "Msamp/s", "in MB/s", "out GB/s", "ns/samp");
	if(nbase > 0)
	{
		printf(" %10s %8s", "base", "speedup");
	}
	printf("\n");

	for(i = 0; i < ncase; ++i)
	{
		const struct benchcase *bc = cases + i;
		const struct vdif_mode *m = bc->m;
		double msps, inMBps, outGBps, nspersample;

		if(bc->best <= 0.0)
		{
			continue;
		}

		/* Msamp/s counts all channels; complex samples count once */
		msps = (double)bc->callsamples*m->nchan/bc->best/1.0e6;
		inMBps = (double)bc->callsamples*m->blockbits/8.0/bc->best/1.0e6;
		outGBps = bc->docount ? 0.0 : (double)bc->callsamples*m->nchan*(m->iscomplex ? 8 : 4)/bc->best/1.0e9;
		nspersample = 1.0e9*bc->best/bc->callsamples;

		printf("%-24s %10.1f %10.1f %10.2f %10.3f", bc->name, msps, inMBps, outGBps, nspersample);
		if(nbase > 0)
		{
			const struct baseline *b = find_baseline(base, nbase, bc->name);

			if(b && b->msps > 0.0)
			{
				double ratio = msps/b->msps;

				printf(" %10.1f %7.2fx", b->msps, ratio);
				logsum += log(ratio);
				++nratio;
			}
		}
		printf("\n");

		if(csv)
		{
			fprintf(csv, "%s,%.3f,%.3f,%.4f,%.4f\n", bc->name, msps, inMBps, outGBps, nspersample);
		}
	}

	if(nratio > 0)
	{
		printf("\nGeometric mean speedup over %d modes: %.3fx\n", nratio, exp(logsum/nratio));
	}

	if(csv)
	{
		fclose(csv);
		printf("\nResults written to %s\n", o.csvout);
	}

	free(buf);

	return EXIT_SUCCESS;
}
