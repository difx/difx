/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken & Adam Deller               *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdlib>
#include <cstring>
#include <regex.h>
#include <sstream>
#include "vex_stream.h"
#include "vex_utility.h"

regex_t VexStream::matchType1;
regex_t VexStream::matchType2;
regex_t VexStream::matchType3;
regex_t VexStream::matchType4;
regex_t VexStream::matchType5;
regex_t VexStream::matchType6;
regex_t VexStream::matchType7;
regex_t VexStream::matchType8;

bool VexStream::isInit(Init());	// force execution of the function below to initialize regular expressions
bool VexStream::Init()
{
	int v;

	// of form <fmt>/<threads>/<size>/<bits>	VDIF only     ex: INTERLACEDVDIF/3:2:1:0/5032/2
	// <threads> is colon or comma separated list of thread Ids.  ex: 1:2:3:4 or 5,7,9,11,13,15,17,18,19
	v = regcomp(&matchType1, "^([A-Z]*VDIF[A-Z]*)/([0-9:,]*)/([1-9]+[0-9]*)/([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType1 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt>/<size>/<bits>		VDIF only
	v = regcomp(&matchType2, "^([A-Z]*VDIF[A-Z]*)/([1-9]+[0-9]*)/([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType2 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt><size>			VDIF only
	v = regcomp(&matchType3, "^([A-Z]*VDIF[A-Z]*)([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType3 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt>1_<fanout>	VLBA, Mark4, VLBN only
	v = regcomp(&matchType4, "^([A-Z]+)1_([124])$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType4 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt>_<size>-<Mbps>-<nChan>-<bits>	VDIF only
	v = regcomp(&matchType5, "^([A-Z]*VDIF[A-Z]*)_([1-9]+[0-9]*)-([1-9]+[0-9]*)-([1-9]+[0-9]*)-([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType5 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt>-<Mbps>-<nChan>-<bits>
	v = regcomp(&matchType6, "^([A-Z]+)-([1-9]+[0-9]*)-([1-9]+[0-9]*)-([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType6 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form <fmt>1_<fanout>-<Mbps>-<nChan>-<bits>	Mark4, VLBA, VLBN only
	v = regcomp(&matchType7, "^([A-Z]+)1_([1-9]+[0-9]*)-([1-9]+[0-9]*)-([1-9]+[0-9]*)-([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType7 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	// of form (<fmt>/<bits> or <fmt>-<bits>) or (<fmt>/<size> or <fmt>-<size>  VDIF only)
	v = regcomp(&matchType8, "^([A-Z]*VDIF[A-Z]*)[-/]([1-9]+[0-9]*)$", REG_EXTENDED);
	if(v != 0)
	{
		std::cerr << "Developer Error: VexStream::Init(): compiling matchType8 failed" << std::endl;

		exit(EXIT_FAILURE);
	}

	return true;
}

char VexStream::DataFormatNames[NumDataFormats+1][16] = 
{
	"NONE",
	"VDIF",
	"VDIFL",
	"MARK5B",
	"VLBA",
	"VLBN",
	"MKIV",
	"KVN5B",
	"LBASTD",
	"LBAVSOP",
	"S2",

	"Error"		// keep at the end
};

bool VexStream::isSingleThreadVDIF(const std::string &str)
{
	if(strcasecmp(str.c_str(), "VDIF") == 0 ||
	   strcasecmp(str.c_str(), "VDIFL") == 0 ||
	   strcasecmp(str.c_str(), "VDIFC") == 0)
	{
		return true;
	}

	return false;
}

enum VexStream::DataFormat VexStream::stringToDataFormat(const std::string &str)
{
	if(strcasecmp(str.c_str(), "VDIF") == 0 ||
	   strcasecmp(str.c_str(), "VDIFC") == 0 ||
	   strcasecmp(str.c_str(), "VDIFD") == 0 ||
	   strcasecmp(str.c_str(), "INTERLACEDVDIF") == 0 ||
	   strcasecmp(str.c_str(), "INTERLACEDVDIFC") == 0 ||
	   strcasecmp(str.c_str(), "INTERLACEDVDIFD") == 0)
	{
		return FormatVDIF;
	}
	else if(strcasecmp(str.c_str(), "VDIFL") == 0)
	{
		return FormatLegacyVDIF;
	}
	else if(strcasecmp(str.c_str(), "Mark5B") == 0 ||
	        strcasecmp(str.c_str(), "MK5B") == 0)
	{
		return FormatMark5B;
	}
	else if(strcasecmp(str.c_str(), "VLBA") == 0)
	{
		return FormatVLBA;
	}
	else if(strcasecmp(str.c_str(), "VLBN") == 0)
	{
		return FormatVLBN;
	}
	else if(strcasecmp(str.c_str(), "MKIV") == 0 ||
	        strcasecmp(str.c_str(), "Mark4") == 0)
	{
		return FormatMark4;
	}
	else if(strcasecmp(str.c_str(), "KVN5B") == 0)
	{
		return FormatKVN5B;
	}
	else if(strcasecmp(str.c_str(), "LBA") == 0 ||
	        strcasecmp(str.c_str(), "LBASTD") == 0)
	{
		return FormatLBASTD;
	}
	else if(strcasecmp(str.c_str(), "LBAVSOP") == 0)
	{
		return FormatLBAVSOP;
	}
	else if(strcasecmp(str.c_str(), "S2") == 0)
	{
		return FormatS2;
	}

	return NumDataFormats;
}

// FIXME: move to parserhelp
bool VexStream::parseThreads(const std::string &threadList)
{
	int i, t;
	char c;

	threads.clear();

	t = -1;
	i = 0;
	do
	{
		c = threadList[i];
		if(c >= '0' && c <= '9')
		{
			if(t < 0)
			{
				t = 0;
			}
			t *= 10;
			t += (c - '0');
		}
		else if(c == ':' || c == ',' || c == 0)
		{
			if(t < 0)
			{
				threads.clear();

				return false;
			}
			if(t >= 1024)
			{
				threads.clear();

				return false;
			}
			threads.push_back(t);
			t = -1;
		}
		else
		{
			threads.clear();
			
			return false;
		}
		++i;
	}
	while(c > 0);

	return true;
}

static int matchInt(const std::string &str, const regmatch_t &match)
{
	return atoi(str.substr(match.rm_so, match.rm_eo-match.rm_so).c_str());
}

// Accepts strings of the following formats and populates appropriate members:

// VDIF/<size>/<bits>  -> format=VDIF,  VDIFFrameSize=<size>, nBit = <bits>, nThread=1, threads=[]
// VDIFL/<size>/<bits> -> format=VDIFL, VDIFFrameSize=<size>, nBit = <bits>, nThread=1, threads=[]
// VDIF_<size>-<Mbps>-<nChan>-<bits>   -> format=VDIF,  VDIFFrameSize=<size>+32, nBit=<bits>, nThread=1, threads=[], nChan=<nChan>
// VDIFL_<size>-<Mbps>-<nChan>-<bits>  -> format=VDIFL, VDIFFrameSize=<size>+16, nBit=<bits>, nThread=1, threads=[], nChan=<nChan>
// INTERLACEDVDIF/0:1:16:17/1032/2
void VexStream::setVDIFSubformat(const std::string &str)
{
	if(strcasecmp(str.c_str(), "VDIFC") == 0 || strcasecmp(str.c_str(), "INTERLACEDVDIFC") == 0)
	{
		dataSampling = SamplingComplex;
	}
	else if(strcasecmp(str.c_str(), "VDIFD") == 0 || strcasecmp(str.c_str(), "INTERLACEDVDIFD") == 0)
	{
		dataSampling = SamplingComplexDSB;
	}
}

bool VexStream::parseFormatString(const std::string &formatName)
{
	const int MaxMatches = 6;
	regmatch_t match[MaxMatches];

	nThread = 0;
	singleThread = false;


	for(int df = 0; df < NumDataFormats; ++df)
	{
		if(strcasecmp(formatName.c_str(), DataFormatNames[df]) == 0)
		{
			// Here we match the format name but get no additional information
			format = static_cast<DataFormat>(df);
			singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

			return true;
		}
	}

	if(regexec(&matchType1, formatName.c_str(), 5, match, 0) == 0)
	{
		// of form <fmt>/<threads>/<size>/<bits>
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		setVDIFSubformat(formatName.substr(0, match[1].rm_eo));
		bool rv = parseThreads(formatName.substr(match[2].rm_so, match[2].rm_eo-match[2].rm_so));
		if(rv == false)
		{
			std::cerr << "Error parsing colon separated thread numbers.  String was '" << formatName.substr(match[2].rm_so, match[2].rm_eo-match[2].rm_so) << "'." << std::endl;
		}
		nThread = threads.size();
		VDIFFrameSize = matchInt(formatName, match[3]);
		nBit = matchInt(formatName, match[4]);

		return true;
	}
	else if(regexec(&matchType2, formatName.c_str(), 4, match, 0) == 0)
	{
		// of form <fmt>/<size>/<bits>
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		setVDIFSubformat(formatName.substr(0, match[1].rm_eo));
		VDIFFrameSize = matchInt(formatName, match[2]);
		nBit = matchInt(formatName, match[3]);
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}
	else if(regexec(&matchType3, formatName.c_str(), 3, match, 0) == 0)
	{
		// of form <fmt><size>	VDIF only
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		setVDIFSubformat(formatName.substr(0, match[1].rm_eo));
		VDIFFrameSize = matchInt(formatName, match[2]);
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}
	else if(regexec(&matchType4, formatName.c_str(), 3, match, 0) == 0)
	{
		// of form <fmt>1_<fanout>
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		if(format != FormatVLBA && format != FormatVLBN && format != FormatMark4)
		{
			format = NumDataFormats;

			return false;
		}
		fanout = matchInt(formatName, match[2]);

		return true;
	}
	else if(regexec(&matchType5, formatName.c_str(), 6, match, 0) == 0)
	{
		// of form <fmt>_<size>-<Mbps>-<nChan>-<bits>	VDIF only
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		setVDIFSubformat(formatName.substr(0, match[1].rm_eo));
		VDIFFrameSize = matchInt(formatName, match[2]);
		// Mbps not captured
		nRecordChan = matchInt(formatName, match[4]);
		nBit = matchInt(formatName, match[5]);
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}
	else if(regexec(&matchType6, formatName.c_str(), 5, match, 0) == 0)
	{

		// of form <fmt>-<Mbps>-<nChan>-<bits>
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		// Mbps not captured
		nRecordChan = matchInt(formatName, match[3]);
		nBit = matchInt(formatName, match[4]);
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}
	else if(regexec(&matchType7, formatName.c_str(), 6, match, 0) == 0)
	{
		// of form <fmt>1_<fanout>-<Mbps>-<nChan>-<bits>	Mark4, VLBA, VLBN only
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		if(format != FormatVLBA && format != FormatVLBN && format != FormatMark4)
		{
			format = NumDataFormats;

			return false;
		}
		fanout = matchInt(formatName, match[2]);
		// Mbps not captured
		nRecordChan = matchInt(formatName, match[4]);
		nBit = matchInt(formatName, match[5]);

		return true;
	}
	else if(regexec(&matchType8, formatName.c_str(), 3, match, 0) == 0)
	{
		// of form (<fmt>/<bits> or <fmt>-<bits>) or (<fmt>/<size> or <fmt>-<size>  VDIF only)
		format = stringToDataFormat(formatName.substr(0, match[1].rm_eo));
		if(format == NumDataFormats)
		{
			return false;
		}
		setVDIFSubformat(formatName.substr(0, match[1].rm_eo));
		int v = matchInt(formatName, match[2]);
		if(v > 32 && isVDIFFormat())
		{
			VDIFFrameSize = v;	
		}
		else
		{
			nBit = v;
		}
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}
	else
	{
		// of form <fmt>
		format = stringToDataFormat(formatName);
		if(format == NumDataFormats)
		{
			return false;
		}
		singleThread = isSingleThreadVDIF(formatName.substr(0, match[1].rm_eo));

		return true;
	}

	format = FormatNone;

	return false;
}

bool VexStream::isTrackFormat() const
{
	if(singleThread)
	{
		return true;
	}

	return (format == FormatVLBA || format == FormatVLBN || format == FormatMark4 || format == FormatMark5B || format == FormatKVN5B);
}

bool VexStream::isLBAFormat() const
{
	return (format == FormatLBASTD || format == FormatLBAVSOP);
}

bool VexStream::formatHasFanout() const
{
	return (format == FormatVLBA || format == FormatVLBN || format == FormatMark4);
}

bool VexStream::isVDIFFormat() const
{
	return ::isVDIFFormat(format);
}

void VexStream::setFanout(int fan)
{
	fanout = fan;
}

int VexStream::snprintDifxFormatName(char *outString, int maxLength) const
{
	int n;

	if(format < 0 || format > NumDataFormats)
	{
		std::cerr << "Developer error: VexStream::snprintDifxFormatName: format has illegal value" << std::endl;
		std::cerr << "VexStream =" << *this << std::endl;

		exit(EXIT_FAILURE);
	}
	if(format == FormatVDIF)
	{
		if(singleThread)
		{
			n = snprintf(outString, maxLength, "%s", DataFormatNames[format]);
		}
		else
		{
			std::stringstream fn;
			char sep;
			sep = '/';
			fn << "INTERLACEDVDIF";
			for(std::vector<int>::const_iterator it = threads.begin(); it != threads.end(); ++it)
			{
				fn << sep;
				fn << *it;
				sep = ':';
			}
			
			n = snprintf(outString, maxLength, "%s", fn.str().c_str());
		}
	}
	else if(format == FormatS2)
	{
		std::cerr << "Warning: S2 format encounted.  This could be LBAVSOP or LBASTD.  Defaulting to LBAVSOP!" << std::endl;

		n = snprintf(outString, maxLength, "%s", DataFormatNames[FormatLBAVSOP]);
	}
	else
	{
		n = snprintf(outString, maxLength, "%s", DataFormatNames[format]);
	}

	return n;
}

int VexStream::dataFrameSize() const
{
	int s = 0;

	switch(format)
	{
	case FormatVDIF:
	case FormatLegacyVDIF:
		s = VDIFFrameSize;
		break;
	case FormatVLBA:
	case FormatVLBN:
		s = 2520*fanout*nBit*nextPowerOf2(nRecordChan);
		break;
	case FormatMark4:
		s = 2500*fanout*nBit*nextPowerOf2(nRecordChan);
		break;
	case FormatMark5B:
	case FormatKVN5B:
		s = 10016;
		break;
	case FormatS2:
	case FormatLBAVSOP:
	case FormatLBASTD:
		s = 4096 + 10*nBit*nextPowerOf2(nRecordChan)*static_cast<int>(sampRate+0.5)/8;
		break;
	default:
		std::cerr << "Developer error: Format " << format << " not handled in VexStream::dataFrameSize().  This format may be called '" << DataFormatNames[format] << "'." << std::endl;

		exit(EXIT_FAILURE);
	}
	
	return s;
}

bool isVDIFFormat(VexStream::DataFormat format)
{
	return (format == VexStream::FormatVDIF || format == VexStream::FormatLegacyVDIF);
}

std::ostream& operator << (std::ostream &os, const VexStream &x)
{
	std::stringstream formatName;

	if(x.format >= 0 && x.format < VexStream::NumDataFormats)
	{
		formatName << VexStream::DataFormatNames[x.format];
	}
	else
	{
		formatName << "Illegal(" << x.format << ")";
	}

	os << " [format=" << formatName.str() << ", nBit=" << x.nBit << ", nRecordChan=" << x.nRecordChan << ", dataFrameSize=" << x.dataFrameSize() << ", nThread=" << x.nThread << ", singleThread=" << x.singleThread << ", sampRate=" << x.sampRate << ", tSys=" << x.difxTsys;
	if(!x.threads.empty())
	{
		for(std::vector<int>::const_iterator it = x.threads.begin(); it != x.threads.end(); ++it)
		{
			if(it == x.threads.begin())
			{
				os << ", threads=";
			}
			else
			{
				os << ",";
			}
			os << *it;
		}
	}
	os << "]";
	
	return os;
}
