/***************************************************************************
 *   Copyright (C) 2015-2021 by Walter Brisken & Adam Deller               *
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

#ifndef __VEX_STREAM_H__
#define __VEX_STREAM_H__

#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <difxio.h>
#include <regex.h>
#include "vex_thread.h"

// FIXME: change singleThread to be a bool() which looks at (nThread == 1) ???

class VexStream
{
public:
	// keep in sync with dataFormatNames
	enum DataFormat
	{
		FormatNone = 0,		// no format specified
		FormatVDIF,
		FormatLegacyVDIF,
		FormatCODIF,
		FormatMark5B,
		FormatVLBA,
		FormatVLBN,
		FormatMark4,
		FormatKVN5B,
		FormatLBASTD,
		FormatLBAVSOP,		// implied if S2_data_source = VLBA
		FormatS2,

		NumDataFormats		// must be last line in list; also serves as error code
	};

	static char DataFormatNames[NumDataFormats+1][16];

	VexStream() : sampRate(0.0), nBit(0), nRecordChan(0), VDIFFrameSize(0), singleThread(false), format(FormatNone), dataSampling(SamplingReal), dataSource(DataSourceUnspecified), alignmentPeriod(1), difxTsys(0.0) {}

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
	size_t nPresentChan() const;	// Looks through listed channels and excludes those that are in the threadsAbsent set
	bool recordChanAbsent(int recChan) const;	// return true if this record channel is not in the data stream
	bool recordChanIgnore(int recChan) const;	// return true if this record channel should not be correlated
	unsigned int nThread() const { return threads.size(); }		// number of threads

	VexThread *getVexThreadByLink(const std::string threadLink);
	VexThread *getVexThreadByThreadId(int threadId);

	double sampRate;		// [Hz]
	unsigned int nBit;		// bits per sample
	unsigned int nRecordChan;	// number of recorded channels (per thread, for VDIF)
	unsigned int fanout;		// 1, 2 or 4 (VLBA, Mark4 and related formats only)
	unsigned int VDIFFrameSize;	// size of one logical block of data
	bool singleThread;		// true for single thread VDIF
	std::vector<VexThread> threads;	// ordered list of threads for VDIF
	std::set<int> threadsAbsent;	// threads that are expected to be absent in the data
	std::set<int> threadsIgnore;	// threads that are expected to be present but should not be correlated
	enum DataFormat format;
	enum SamplingType dataSampling;	// Real or Complex
	enum DataSource dataSource;
	unsigned int alignmentPeriod;   // in seconds; always 1 for VDIF, can be >1 for CODIF
	double difxTsys;		// The DiFX .input file TSYS value for this datastream
	std::string streamLink;		// First parameter of a vex2 DATASTREAMS:datasteam line
	std::string streamName;		// Third parameter of a vex2 DATASTREAMS:datasteam line

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
	static regex_t matchType8;	// of form <fmt>/<size> or <fmt>-<size>
        static regex_t matchType9;      // of form <fmt>/<period seconds>/<size>/<bits> (CODIF specific)
};

bool isVDIFFormat(VexStream::DataFormat format);

std::ostream& operator << (std::ostream &os, const VexStream &x);

#endif
