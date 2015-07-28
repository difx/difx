#ifndef __VEX_STREAM_H__
#define __VEX_STREAM_H__

#include <iostream>
#include <string>
#include <vector>
#include <difxio.h>
#include <regex.h>

class VexStream
{
public:
	// keep in sync with dataFormatNames
	enum DataFormat
	{
		FormatNone = 0,		// no format specified
		FormatVDIF,
		FormatLegacyVDIF,
		FormatMark5B,
		FormatVLBA,
		FormatVLBN,
		FormatMark4,
		FormatKVN5B,
		FormatLBASTD,
		FormatLBAVSOP,
		FormatS2,

		NumDataFormats		// must be last line in list; also serves as error code
	};

	static char DataFormatNames[NumDataFormats+1][16];

	VexStream() : sampRate(0.0), nBit(0), nRecordChan(0), VDIFFrameSize(0), singleThread(false), dataSampling(SamplingReal) {}
	double dataRateMbps() const { return sampRate*nBit*nRecordChan/1000000.0; }
	static enum DataFormat stringToDataFormat(const std::string &str);
	static bool isSingleThreadVDIF(const std::string &str);
	bool parseThreads(const std::string &threadList);	// colon separated list of numbers
	void setVDIFSubformat(const std::string &str);
	bool parseFormatString(const std::string &formatName);
	bool isTrackFormat() const;	// true for formats where tracks are assigned
	bool isVDIFFormat() const;
	bool isLBAFormat() const;
	bool formatHasFanout() const;
	void setFanout(int fan);
	int snprintDifxFormatName(char *outString, int maxLength) const;
	int dataFrameSize() const;

	double sampRate;		// [Hz]
	unsigned int nBit;		// bits per sample
	unsigned int nRecordChan;	// number of recorded channels (per thread, for VDIF)
	unsigned int nThread;		// number of threads
	unsigned int fanout;		// 1, 2 or 4 (VLBA, Mark4 and related formats only)
	unsigned int VDIFFrameSize;	// size of one logical block of data
	bool singleThread;		// true for single thread VDIF
	std::vector<int> threads;	// ordered list of threads for VDIF
	enum DataFormat format;
	enum SamplingType dataSampling;	// Real or Complex

private:
	static bool Init();		// called to initialize the regexes below
	static bool isInit;		// something to collect the output of the above function
	static regex_t matchType1;	// of form <fmt>/<threads>/<size>/<bits>	(VDIF specific, threads separated by colons)
	static regex_t matchType2;	// of form <fmt>/<size>/<bits>			(VDIF specific)
	static regex_t matchType3;	// of form <fmt><size>				(VDIF specific)
	static regex_t matchType4;	// of form <fmt>1_<fanout>			(VLBA, VLBN, Mark4 specific)
	static regex_t matchType5;	// of form <fmt>_<size>-<Mbps>-<nChan>-<bits>	(VDIF specific)
	static regex_t matchType6;	// of form <fmt>-<Mbps>-<nChan>-<bits>
	static regex_t matchType7;	// of form <fmt>1_<fanout>-<Mbps>-<nChan>-<bits> (VLBA, VLBN, Mark4 only)
};

std::ostream& operator << (std::ostream &os, const VexStream &x);

#endif
