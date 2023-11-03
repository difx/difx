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
		"Usage: m5pcal_test [--conj] <infile> <format> <if_nr> <spacing Hz[/denom]> <offset from 0 Hz in Hz[/denom]> <0,S:ssb | 1,D:dsb> <0,U:usb | 1,L:lsb>\n\n"
		"Optional arguments:\n"
		"   --conj   Conjugate the raw signal (invert the sideband) before phase-cal extraction. For complex data only.\n\n"
		"Arguments:\n"
		"   infile   Input file\n"
		"   format   Data format for mark5access decoder library, <FORMAT>-<Mbps>-<nchan>-<nbit>\n"
		"   if_nr    Zero based index of the IF from which to extract phase-cal\n"
		"   spacing  Phase cal tone spacing in Hz, with optional denominator greater than 1\n"
		"   offset   Phase cal tone offset from band edge in Hz; is translated into FFT bin number interally\n"
		"   bandtype Either single sideband (0 or S), or double sideband (1 or D)\n"
		"   sideband Either upper sideband (0 or U), or lower sideband (1 or L)\n\n"
	);
}

int main(int argc, char** argv)
{
	size_t chunksize;
	const double min_binResolution_Hz = 10e3;

	char* denom_arg;
	const char* format;
	const char* infile;
	int if_nr = 0;
	double bw_hz;
	long pcal_denom = 0;
	long pcal_interval_hz = 1e6;
	long pcal_offset_hz = 0;
	int lsb = 0, ssb = 1;
	bool done = false;
	bool conjugate = false;
	int i, n;
	size_t sample_offset = 0;
	double T;

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
	denom_arg = strchr(argv[4], '/');
	if (denom_arg) {
		denom_arg[0] = '\0';
		pcal_denom = atoi(denom_arg+1);
	}
	pcal_interval_hz = atol(argv[4]);
	denom_arg = strchr(argv[5], '/');
	if (denom_arg) {
		denom_arg[0] = '\0';
		if (pcal_denom > 0 && pcal_denom != atoi(denom_arg+1)) {
			printf("Error: Denominator for tone spacing (/%ld) and tone offset (/%d) must be identical!\n", pcal_denom, atoi(denom_arg+1));
			return 1;
		} else {
			pcal_denom = atoi(denom_arg+1);
		}
	}
	pcal_offset_hz = atol(argv[5]);
	ssb = (argv[6][0] == '0') || (argv[6][0] == 'S');
	lsb = (argv[7][0] == '1') || (argv[7][0] == 'L');

	// Open file
	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(infile, 0),
		new_mark5_format_generic_from_string(format) );
	if (if_nr >= ms->nchan) {
		printf("Error: Specified 0-based IF nr %d, but input file has only %d IFs (0..%d).\n", if_nr, ms->nchan, ms->nchan-1);
		return 1;
	}

	// Allocate decoded-data arrays according to nr of recorded channels
	chunksize = ms->samprate / 100;
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
	const fraction tonespacing(pcal_interval_hz, pcal_denom);
	const fraction toneoffset(pcal_offset_hz, pcal_denom);
	pc = PCal::getNew(bw_hz, tonespacing, toneoffset, sample_offset, data_type, band_type);
	if (!pc) {
		printf("Error: failed to find suitable PCal extractor for the given tone parameters.\n");
		return 1;
	}

	tonedata = vectorAlloc_cf32(pc->getLength());

	printf("Args: interval %ld Hz, offset %ld Hz, %s %s %s data %s\n", pcal_interval_hz, pcal_offset_hz,
		ssb ? "SSB" : "DSB",
		lsb ? "LSB" : " USB",
		ms->iscomplex ? "complex" : "real",
		ms->iscomplex && conjugate ? " conjugated to flip USB/LSB sense" : ""
	);
	printf("PCal extractor : length=%d\n", pc->getLength());

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
		if (ms->iscomplex) {
			T = double(nsamples_total) / bw_hz;
		} else {
			T = double(nsamples_total) / (2*bw_hz);
		}

		printf("%8.3lf sec ", T);
		for (i = 0; i < pc->getLength(); i++) {
			float phi = (180/M_PI)*std::atan2(tonedata[i].im, tonedata[i].re);
			float amp = sqrt(tonedata[i].im*tonedata[i].im + tonedata[i].re*tonedata[i].re);
			double bbfreq = double(pcal_offset_hz + pcal_interval_hz * long(i)) / pcal_denom;
			//double skyfreq = 18174.0e6 + bbfreq; // arbitraty, depends on 1st LO...
			//skyfreq = lsb ? (skyfreq - bbfreq) : (skyfreq + bbfreq);
			//printf("   tone %d : baseband %8.5f MHz  'sky' e.g. %8.5f MHz : phase %+7.2f amp %6.2f\n", i, bbfreq*1e-6, skyfreq*1e-6, phi, amp);
			//printf("   %.3lf sec tone %d %9.5f MHz : %+7.2f deg, amp %6.2f\n", T, i, bbfreq*1e-6, phi, amp);
			printf("%9.5f MHz: %+7.2f deg %6.2f  ", bbfreq*1e-6, phi, amp);
		}
		printf("\n");

	}

	return 0;
}
