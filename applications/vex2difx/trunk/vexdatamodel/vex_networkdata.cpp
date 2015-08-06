#include "vex_networkdata.h"

std::ostream& operator << (std::ostream &os, const VexNetworkData &x)
{
	os << "Network Data [ port=" << x.networkPort << ", windowSize=" << x.windowSize << " ]" << std::endl;

	return os;
}
