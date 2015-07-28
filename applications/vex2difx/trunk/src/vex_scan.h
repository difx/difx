#ifndef __VEX_SCAN_H__
#define __VEX_SCAN_H__

#include <ostream>
#include <string>
#include <map>
#include "interval.h"

class VexScan : public Interval
{
public:
	std::string defName;				// name of this scan
	std::string intent;				// intent of this scan

	std::string modeDefName;
	std::string sourceDefName;	
	std::map<std::string,Interval> stations;
	std::map<std::string,bool> recordEnable;	// This is true of the drive number is non-zero
	double size;					// [bytes] approx. correlated size
	double mjdVex;					// The start time listed in the vex file

	VexScan(): size(0), mjdVex(0.0) {};
	const Interval *getAntennaInterval(const std::string &antName) const;
	bool getRecordEnable(const std::string &antName) const;
};

std::ostream& operator << (std::ostream &os, const VexScan &x);

#endif
