#ifndef __VEX_MODE_H__
#define __VEX_MODE_H__

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <difxio.h>
#include "vex_subband.h"
#include "vex_setup.h"

class VexMode
{
public:
	VexMode() {}

	int addSubband(double freq, double bandwidth, char sideband, char pol);
	int getPols(char *pols) const;
	int getBits() const;
	int getMinBits() const;
	int getMaxBits() const;
	int getMinSubbands() const;
	int nStream() const;
	const VexSetup* getSetup(const std::string &antName) const;
	double getLowestSampleRate() const;
	double getHighestSampleRate() const;
	double getAverageSampleRate() const;
	void swapPolarization(const std::string &antName);
	void setSampling(const std::string &antName, unsigned int streamId, enum SamplingType dataSampling);
	void setPhaseCalInterval(const std::string &antName, int phaseCalIntervalMHz);
	void selectTones(const std::string &antName, enum ToneSelection selection, double guardBandMHz);
	void generateRecordChans();

	std::string defName;

	std::vector<VexSubband> subbands;
	std::vector<VexSubband> zoombands;
	std::map<std::string,VexSetup> setups;	// indexed by antenna name
};

std::ostream& operator << (std::ostream &os, const VexMode &x);

#endif
