#include <mark5access/mark5_stream.h>

#include "pcal.h"

#include <complex>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

void usage()
{
	printf(
		"\n"
		"Usage: m5pcal_test [--conj] <infile> <format> <if_nr> <spacing Hz> <offset from 0Hz in Hz> <0,S:ssb | 1,D:dsb> <0,U:usb | 1,L:lsb>\n\n"
		"Optional arguments:\n"
		"   --conj   Conjugate the raw signal (invert the sideband) before phase-cal extraction. For complex data only.\n\n"
		"Arguments:\n"
		"   infile   Input file\n"
		"   format   Data format for mark5access decoder library, <FORMAT>-<Mbps>-<nchan>-<nbit>\n"
		"   if_nr    Zero based index of the IF from which to extract phase-cal\n"
		"   spacing  Phase cal tone spacing in Hz\n"
		"   offset   Phase cal tone offset from band edge; is translated into FFT bin number interally\n"
		"   bandtype Either single sideband (0 or S), or double sideband (1 or D)\n"
		"   sideband Either upper sideband (0 or U), or lower sideband (1 or L)\n\n"
	);
}

int main(int argc, char** argv)
{
	const int chunksize = 1*1024*1024;
	const double min_binResolution_Hz = 10e3;

	const char* format;
	const char* infile;
	int if_nr = 0;
	double bw_hz;
	double pcal_interval_hz = 1e6;
	int pcal_offset_hz = 0;
	int lsb = 0, ssb = 1;
	bool done = false;
	bool conjugate = false;
	int i, n;
    size_t sample_offset = 0;

	struct mark5_stream *ms;
	mark5_float_complex **data_complex;
	float **data_real;

	PCal* pc;
	cf32* tonedata;

	if (argc != 8 && argc != 9) {
		usage();
		return -1;
	}

	if (strcmp(argv[1], "--conj") == 0) {
		conjugate = true;
		argv++;
	}

	infile = argv[1];
	format = argv[2];
	if_nr = atoi(argv[3]);
	pcal_interval_hz = atof(argv[4])*1e6;
	pcal_offset_hz = (int)(atof(argv[5])*1e6);
	ssb = (argv[6][0] == '0') || (argv[6][0] == 'S');
	lsb = (argv[7][0] == '1') || (argv[7][0] == 'L');

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
	PCal::setMinFrequencyResolution(min_binResolution_Hz);
	if (ms->iscomplex) {
		bw_hz = ms->samprate;
	} else {
		bw_hz = ms->samprate / 2;
	}

	Configuration::datasampling data_type = ms->iscomplex ? Configuration::COMPLEX : Configuration::REAL;
	Configuration::complextype band_type = ssb ? Configuration::SINGLE : Configuration::DOUBLE;

	pc = PCal::getNew(bw_hz, pcal_interval_hz, pcal_offset_hz, sample_offset, data_type, band_type);

	tonedata = vectorAlloc_cf32(pc->getLength());

	printf("Args: interval %lf Hz, offset %d Hz, %s %s %s data %s\n", pcal_interval_hz, pcal_offset_hz,
		ssb ? "SSB" : "DSB",
		lsb ? "LSB" : " USB",
		ms->iscomplex ? "complex" : "real",
		ms->iscomplex && conjugate ? " conjugated to flip USB/LSB sense" : ""
	);
	printf("PCal extractor : length=%d nbins=%d\n", pc->getLength(), pc->getNBins());

	// Read file
	ssize_t nsamples_total = 0;
	while (!done) {

		// Prepare for next extraction period
		pc->clear();
		pc->adjustSampleOffset(nsamples_total);

		// Get data
		if (ms->iscomplex) {
			n = mark5_stream_decode_complex(ms, chunksize, data_complex);
			if (conjugate) {
				for (i = 0; i < n; i++) {
					data_complex[if_nr][i] = std::conj(data_complex[if_nr][i]);
				}
			}
			pc->extractAndIntegrate((float*)data_complex[if_nr], chunksize);
		} else {
			n = mark5_stream_decode(ms, chunksize, data_real);
			pc->extractAndIntegrate(data_real[if_nr], chunksize);
		}
		done = (n <= 0);
		nsamples_total += n;

		// Extract pcal data
		n = pc->getFinalPCal(tonedata);
		printf("Extracted pcal from n = %d samples, final pcal data are:\n", n);

		for (i = 0; i < pc->getLength(); i++) {
			float phi = (180/M_PI)*std::atan2(tonedata[i].im, tonedata[i].re);
			float amp = sqrt(tonedata[i].im*tonedata[i].im + tonedata[i].re*tonedata[i].re);
			float bbfreq = pcal_offset_hz + pcal_interval_hz * i;
			double skyfreq = 1000.0 * pcal_interval_hz*1e6; // any
			skyfreq = lsb ? (skyfreq - bbfreq) : (skyfreq + bbfreq);
			printf("   tone %d : baseband %8.3f MHz  'sky' e.g. %8.3f MHz : phase %+7.2f amp %6.2f\n", i, bbfreq*1e-6, skyfreq*1e-6, phi, amp);
		}
		printf("\n");

	}

	return 0;
}
