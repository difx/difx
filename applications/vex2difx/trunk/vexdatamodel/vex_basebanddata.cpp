#include <set>
#include "vex_basebanddata.h"

// negative streamId implies remove all
int removeBasebandDataByStreamId(std::vector<VexBasebandData> &data, int streamId)
{
	int n = 0;

	for(std::vector<VexBasebandData>::iterator it = data.begin(); it != data.end(); )
	{
		if(it->streamId == streamId || streamId < 0)
		{
			it = data.erase(it);
			++n;
		}
		else
		{
			++it;
		}
	}

	return n;
}

int nRepresentedDatastreams(const std::vector<VexBasebandData> &data)
{
	std::set<int> streams;

	for(std::vector<VexBasebandData>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		streams.insert(it->streamId);
	}

	return streams.size();
}

std::ostream& operator << (std::ostream &os, const VexBasebandData &x)
{
	os << "Baseband(" << x.filename << ", streamId=" << x.streamId << ", " << (const Interval&)x << ")";

	return os;
}

