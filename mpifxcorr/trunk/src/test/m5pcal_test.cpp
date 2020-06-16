#include <mark5access/mark5_stream.h>

#include "pcal.h"

#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

void usage()
{
	printf("Usage: m5pcal_test <infile> <format> <if_nr> <spacing Hz> <offset from 0Hz in Hz> <0:usb|1:lsb>\n");
}

int main(int argc, char** argv)
{
	const int chunksize = 1*1024*1024;
	const char* format;
	const char* infile;
	int if_nr = 0;
	double bw_hz;
	double pcal_interval_hz = 1e6;
	int pcal_offset_hz = 0;
	int lsb = 0;
	int done = 0;
	int i, n;

    struct mark5_stream *ms;
	mark5_float_complex **data_complex;
	float **data_real;

	PCal* pc;
	cf32* tonedata;

	if (argc != 7) {
		usage();
		return -1;
	}

	infile = argv[1];
	format = argv[2];
	if_nr = atoi(argv[3]);
	pcal_interval_hz = atof(argv[4])*1e6;
	pcal_offset_hz = (int)(atof(argv[5])*1e6);
	lsb = (argv[6][0] == '1');

	// Open file
    ms = new_mark5_stream_absorb(
            new_mark5_stream_file(infile, 0),
            new_mark5_format_generic_from_string(format) );

	// Allocate decoded-data arrays according to nr of recorded channels
	data_complex = (mark5_float_complex **)malloc(ms->nchan*sizeof(mark5_float_complex *));
	data_real = (float**)malloc(ms->nchan*sizeof(float *));
	for(i = 0; i < ms->nchan; ++i) {
		data_complex[i] = (mark5_float_complex *)malloc(chunksize*sizeof(mark5_float_complex));
		data_real[i] = (float *)malloc(chunksize*sizeof(float));
	}

	// Allocate PCal extractor
	PCal::setMinFrequencyResolution(1e6);
	if (ms->iscomplex) {
		bw_hz = ms->samprate;
	} else {
		bw_hz = ms->samprate / 2;
	}

	pc = PCal::getNew(bw_hz, pcal_interval_hz, pcal_offset_hz, 0, ms->iscomplex, lsb);

	tonedata = vectorAlloc_cf32(pc->getLength());

	printf("Args: interval %lf Hz, offset %d Hz, lsb?:%d  file:complex=%d\n", pcal_interval_hz, pcal_offset_hz, lsb, ms->iscomplex);
	printf("PCal extractor : length=%d nbins=%d\n", pc->getLength(), pc->getNBins());

	// Read file
	while (!done) {

		pc->clear();

		if (ms->iscomplex) {
			n = mark5_stream_decode_complex(ms, chunksize, data_complex);
			pc->extractAndIntegrate((float*)data_complex[if_nr], chunksize);
		} else {
			n = mark5_stream_decode(ms, chunksize, data_real);
			pc->extractAndIntegrate(data_real[if_nr], chunksize);
		}
		done = (n <= 0);

		n = pc->getFinalPCal(tonedata);

		printf("Decoded n = %d samples, pcal data are:\n", n);

		for (i = 0; i < pc->getLength(); i++) {
			float phi = (180/M_PI)*std::atan2(tonedata[i].im, tonedata[i].re);
			float amp = sqrt(tonedata[i].im*tonedata[i].im + tonedata[i].re*tonedata[i].re);
			float freq = pcal_offset_hz + pcal_interval_hz * (lsb ? (pc->getLength() - i) : i);
			printf("   tone %d : baseband %8.3f MHz : phase %+7.2f amp %6.2f\n", i, freq*1e-6, phi, amp);
		}
		printf("\n");

	}

	return 0;
}
