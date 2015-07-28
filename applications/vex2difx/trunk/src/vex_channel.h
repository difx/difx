#ifndef __VEX_CHANNEL_H__
#define __VEX_CHANNEL_H__

#include <iostream>
#include <string>
#include <vector>
#include <difxio.h>

class VexChannel		// Antenna-specific baseband channel details
{
public:
	VexChannel() : recordChan(-1), subbandId(-1), bbcFreq(0.0), bbcBandwidth(0.0), bbcSideBand(' ') {}
	void selectTones(int toneIntervalMHz, enum ToneSelection selection, double guardBandMHz);
	char bandCode() const;
	friend bool operator ==(const VexChannel &c1, const VexChannel &c2);
	friend bool operator <(const VexChannel &c1, const VexChannel &c2);

	int recordChan;				// channel number on recorded media or threadnum on stream	(< 0 indicates non-recording)
	int streamId;				// stream number
	int subbandId;				// 0-based index; -1 means unset
	std::string ifName;			// name of the IF this channel came from
	double bbcFreq;				// sky frequency tuning of the BBC (Hz)
	double bbcBandwidth;			// bandwidth (Hz)
	char bbcSideBand;			// sideband of the BBC
	std::string name;
	std::string bbcName;			// name given in VEX of this channel in the BBC table
	std::vector<unsigned int> tones;	// pulse cal tones to extract, directly from PHASE_CAL_DETECT
	int threadId;				// thread Id for this channel (assigned based on channel names)
};

std::ostream& operator << (std::ostream &os, const VexChannel &x);

#endif
