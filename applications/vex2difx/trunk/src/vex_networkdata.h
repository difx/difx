#ifndef __VEX_NETWORKDATA_H__
#define __VEX_NETWORKDATA_H__

#include <iostream>
#include <string>

class VexNetworkData
{
public:
	VexNetworkData() : windowSize(0) { }
	std::string networkPort;
	int windowSize;
};

std::ostream& operator << (std::ostream &os, const VexNetworkData &x);

#endif
