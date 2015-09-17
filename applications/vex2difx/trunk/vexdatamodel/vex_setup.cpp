#include <algorithm>
#include <set>
#include "vex_setup.h"

int VexSetup::phaseCalIntervalMHz() const
{
	int p;
	int pc = 0;

	for(std::map<std::string,VexIF>::const_iterator it = ifs.begin(); it != ifs.end(); ++it)
	{
		p = it->second.phaseCalIntervalMHz;
		if(p > 0 && (p < pc || pc == 0))
		{
			pc = p;
		}
	}

	return pc;
}

const VexIF *VexSetup::getIF(const std::string &ifName) const
{
	for(std::map<std::string,VexIF>::const_iterator it = ifs.begin(); it != ifs.end(); ++it)
	{
		if(it->second.name == ifName)
		{
			return &it->second;
		}
	}

	return 0;
}

double VexSetup::firstTuningForIF(const std::string &ifName) const	// return Hz
{
	double tune = 0.0;	// [Hz]
	std::string chanName;

	for(std::vector<VexChannel>::const_iterator ch=channels.begin(); ch != channels.end(); ++ch)
	{
		if(ch->ifName == ifName && (chanName == "" || ch->name < chanName))
		{
			chanName = ch->name;
			tune = ch->bbcFreq;
		}
	}

	return tune;
}

double VexSetup::dataRateMbps() const
{
	double rate = 0;	// [Mbps]

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		rate += it->dataRateMbps();
	}

	return rate;
}

void VexSetup::sortChannels()
{
	sort(channels.begin(), channels.end());
}

bool VexSetup::hasUniqueRecordChans() const
{
	std::set<int> ids;

	for(std::vector<VexChannel>::const_iterator it = channels.begin(); it != channels.end(); ++it)
	{
		ids.insert(it->recordChan);
	}

	return (ids.size() == channels.size());
}

void VexSetup::assignRecordChans()
{
	int id = 0;

	sort(channels.begin(), channels.end());
	for(std::vector<VexChannel>::iterator it = channels.begin(); it != channels.end(); ++it)
	{
		it->recordChan = id++;
	}
}

void VexSetup::setPhaseCalInterval(int phaseCalIntervalMHz)
{
	// change IF phase cal values
	for(std::map<std::string,VexIF>::iterator it = ifs.begin(); it != ifs.end(); ++it)
	{
		it->second.phaseCalIntervalMHz = phaseCalIntervalMHz;
	}

	// weed out unwanted tones
	for(std::vector<VexChannel>::iterator it = channels.begin(); it != channels.end(); ++it)
	{
		if(phaseCalIntervalMHz <= 0)
		{
			it->tones.clear();
		}
		else
		{
			for(std::vector<unsigned int>::iterator tit = it->tones.begin(); tit != it->tones.end(); )
			{
				if(*tit % phaseCalIntervalMHz != 0)
				{
					tit = it->tones.erase(tit);
				}
				else
				{
					++tit;
				}
			}
		}
	}
}

void VexSetup::selectTones(enum ToneSelection selection, double guardBandMHz)
{
	for(std::vector<VexChannel>::iterator it = channels.begin(); it != channels.end(); ++it)
	{
		const VexIF *vif = getIF(it->ifName);
		it->selectTones(vif->phaseCalIntervalMHz, selection, guardBandMHz);
	}
}

int VexSetup::nRecordChan() const
{
	int rc = 0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		rc += it->nRecordChan;
	}

	return rc;
}

bool VexSetup::usesFormat(enum VexStream::DataFormat format) const
{
	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(it->format == format)
		{
			return true;
		}
	}

	return false;
}

unsigned int VexSetup::getBits() const
{
	unsigned int b1, b2;
	bool first = true;

	b1 = getMinBits();
	b2 = getMaxBits();

	if(b1 != b2)
	{
		if(first)
		{
			first = false;
			std::cerr << "Warning: VexSetup::getBits(): different number of bits on different datastreams for one antenna." << std::endl;
		}
	}

	return b2;
}

unsigned int VexSetup::getMinBits() const
{
	unsigned int nBit = 0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(nBit == 0 && it->nBit > 0)
		{
			nBit = it->nBit;
		}
		else if(it->nBit < nBit)
		{
			nBit = it->nBit;
		}
	}

	return nBit;
}

unsigned int VexSetup::getMaxBits() const
{
	unsigned int nBit = 0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(it->nBit > nBit)
		{
			nBit = it->nBit;
		}
	}

	return nBit;
}

double VexSetup::getLowestSampleRate() const
{
	double sr = 0.0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(sr == 0.0 && it->sampRate > 0.0)
		{
			sr = it->sampRate;
		}
		else if(it->sampRate < sr)
		{
			sr = it->sampRate;
		}
	}

	return sr;
}

double VexSetup::getHighestSampleRate() const
{
	double sr = 0.0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(it->sampRate > sr)
		{
			sr = it->sampRate;
		}
	}

	return sr;
}

double VexSetup::getAverageSampleRate() const
{
	double sr = 0.0;
	int n = 0;

	for(std::vector<VexStream>::const_iterator it = streams.begin(); it != streams.end(); ++it)
	{
		if(it->sampRate > 0.0)
		{
			sr += it->sampRate;
			++n;
		}
	}

	if(n > 0)
	{
		sr /= n;
	}

	return sr;
}

std::ostream& operator << (std::ostream &os, const VexSetup &x)
{
	os << "   Setup:" << std::endl;
	for(std::vector<VexChannel>::const_iterator it = x.channels.begin(); it != x.channels.end(); ++it)
	{
		os << "    Channel: " << *it << std::endl;
	}
	for(std::map<std::string,VexIF>::const_iterator it = x.ifs.begin(); it != x.ifs.end(); ++it)
	{
		os << "    IF: " << it->first << " " << it->second << std::endl;
	}
	for(std::vector<VexStream>::const_iterator it = x.streams.begin(); it != x.streams.end(); ++it)
	{
		os << "    Datastream: " << *it << std::endl;
	}

	return os;
}
