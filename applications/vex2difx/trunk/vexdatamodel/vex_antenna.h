
#ifndef __VEX_ANTENNA_H__
#define __VEX_ANTENNA_H__

#include <iostream>
#include <string>
#include <vector>
#include <difxio.h>
#include "interval.h"
#include "vex_clock.h"
#include "vex_basebanddata.h"
#include "vex_networkdata.h"

class VexAntenna
{
public:
	VexAntenna() : x(0.0), y(0.0), z(0.0), dx(0.0), dy(0.0), dz(0.0), posEpoch(0.0), axisOffset(0.0), tcalFrequency(0), dataSource(DataSourceNone) {}

	double getVexClocks(double mjd, double * coeffs) const;
	bool hasData(const Interval &timerange) const;
	int nDatastreamWithData(const Interval &timerange) const;
	void removeBasebandData(int streamId);
	bool hasVSNs() const { return !vsns.empty(); }

	std::string name;
	std::string defName;	// Sometimes names get changed

	double x, y, z;		// (m) antenna position in ITRF
	double dx, dy, dz;	// (m/sec) antenna velocity
	double posEpoch;	// mjd
	std::string axisType;
	double axisOffset;	// (m)
	std::vector<VexClock> clocks;
	int tcalFrequency;	// Hz

	enum DataSource dataSource;

	// actual baesband data is associated with the antenna
	std::vector<VexBasebandData> vsns;	// indexed by vsn number
	std::vector<VexBasebandData> files;	// indexed by file number
	std::vector<VexNetworkData> ports;	// indexed by stream number
};

bool isVLBA(const std::string &antName);

bool usesCanonicalVDIF(const std::string &antName);

std::ostream& operator << (std::ostream &os, const VexAntenna &x);

#endif
