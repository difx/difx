#include <cstring>
#include "vex_antenna.h"

// get the clock epoch as a MJD value (with fractional component), negative 
// means not found.  Also fills in the first two coeffs, returned in seconds
double VexAntenna::getVexClocks(double mjd, double *coeffs) const
{
	double epoch = -1.0;

	for(std::vector<VexClock>::const_iterator it = clocks.begin(); it != clocks.end(); ++it)
	{
		if(it->mjdStart <= mjd)
		{
			epoch = it->offset_epoch;
			coeffs[0] = it->offset;
			coeffs[1] = it->rate;
		}
	}

	return epoch;
}

bool VexAntenna::hasData(const Interval &timerange) const
{
	bool rv = false;

	switch(dataSource)
	{
	case DataSourceNone:
		rv = false;
		break;
	case DataSourceNetwork:
		rv = true;
		break;
	case DataSourceFake:
		rv = true;
		break;
	case DataSourceFile:
		for(std::vector<VexBasebandData>::const_iterator it = files.begin(); it != files.end(); ++it)
		{
			if(it->overlap(timerange) > 0.0)
			{
				rv = true;
				break;
			}
		}
		break;
	case DataSourceModule:
		for(std::vector<VexBasebandData>::const_iterator it = vsns.begin(); it != vsns.end(); ++it)
		{
			if(it->overlap(timerange) > 0.0)
			{
				rv = true;
				break;
			}
		}
		break;
	case NumDataSources:
		// Should never come up.  print error?
		break;
	}

	return rv;
}

int VexAntenna::nDatastreamWithData(const Interval &timerange) const
{
	int n = 0;
 
	if(dataSource == DataSourceFile)
	{
		return nRepresentedDatastreams(files);
	}
	else if(dataSource == DataSourceModule)
	{
		return nRepresentedDatastreams(vsns);
	}

	return n;
}

void VexAntenna::removeBasebandData(int streamId)
{
	removeBasebandDataByStreamId(vsns, streamId);
	removeBasebandDataByStreamId(files, streamId);
}

bool isVLBA(const std::string &antName)
{
	if(strcasecmp(antName.c_str(), "Br") == 0 ||
	   strcasecmp(antName.c_str(), "Fd") == 0 ||
	   strcasecmp(antName.c_str(), "Hn") == 0 ||
	   strcasecmp(antName.c_str(), "Kp") == 0 ||
	   strcasecmp(antName.c_str(), "La") == 0 ||
	   strcasecmp(antName.c_str(), "Mk") == 0 ||
	   strcasecmp(antName.c_str(), "Nl") == 0 ||
	   strcasecmp(antName.c_str(), "Ov") == 0 ||
	   strcasecmp(antName.c_str(), "Pt") == 0 ||
	   strcasecmp(antName.c_str(), "Sc") == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

bool usesCanonicalVDIF(const std::string &antName)
{
	// Add here any known antennas that use VDIF thread ids that start at 0 for the first record channel and increment by 1 for each additional record channel
	if(isVLBA(antName) ||
	   strcasecmp(antName.c_str(), "Gb") == 0 ||
	   strcasecmp(antName.c_str(), "Eb") == 0 ||
	   strcasecmp(antName.c_str(), "Ar") == 0 ||
	   strcasecmp(antName.c_str(), "Y") == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

std::ostream& operator << (std::ostream &os, const VexAntenna &x)
{
	os << "Antenna " << x.name <<
		"\n  x=" << x.x << "  dx/dt=" << x.dx <<
		"\n  y=" << x.y << "  dy/dt=" << x.dy <<
		"\n  z=" << x.z << "  dz/dt=" << x.dz <<
		"\n  posEpoch=" << x.posEpoch <<
		"\n  axisType=" << x.axisType <<
		"\n  axisOffset=" << x.axisOffset <<
		"\n  tcalFrequency=" << x.tcalFrequency << std::endl;

	for(std::vector<VexClock>::const_iterator it = x.clocks.begin(); it != x.clocks.end(); ++it)
	{
		os << "  " << *it << std::endl;
	}
	// FIXME: print dataSource, files, vsns, ports here

	return os;
}
