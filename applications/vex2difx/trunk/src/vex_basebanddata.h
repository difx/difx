#ifndef __VEX_BASEBANDDATA_H__
#define __VEX_BASEBANDDATA_H__

#include <iostream>
#include <string>
#include <vector>
#include "interval.h"

class VexBasebandData : public Interval
{
	public:
	VexBasebandData() : streamId(-1) { }
	std::string filename;	// or VSN, ...
	int streamId;		// 0-based; -1 means not assigned.  Must be assigned for > 1 datastream operation

	VexBasebandData(const std::string &name, int sId, const Interval &timeRange) : Interval(timeRange), filename(name), streamId(sId) {} 
	VexBasebandData(const std::string &name, int sId, double start=-1.0e9, double stop=1.0e9) : Interval(start, stop), filename(name), streamId(sId) {}
};

// returns number of removed entries; negative streamId implies remove from all streams
int removeBasebandDataByStreamId(std::vector<VexBasebandData> &data, int streamId);

int nRepresentedDatastreams(const std::vector<VexBasebandData> &data);

std::ostream& operator << (std::ostream &os, const VexBasebandData &x);

#endif
