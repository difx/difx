#include "vex_subband.h"

bool operator == (const VexSubband &s1, const VexSubband &s2)
{
	if(s1.pol       != s2.pol       ||
	   s1.freq      != s2.freq      ||
	   s1.sideBand  != s2.sideBand  ||
	   s1.bandwidth != s2.bandwidth)
	{
		return false;
	}
	else
	{
		return true;
	}
}

std::ostream& operator << (std::ostream &os, const VexSubband &x)
{
	os << "[" << x.freq << " Hz, " << x.bandwidth << " Hz, sb=" << x.sideBand << ", pol=" << x.pol << "]";
	
	return os;
}
