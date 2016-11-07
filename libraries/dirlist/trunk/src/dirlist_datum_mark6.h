#ifndef __DIRLIST_DATUM_MARK6_H__
#define __DIRLIST_DATUM_MARK6_H__

#include <string>
#include "dirlist.h"

class DirListDatumMark6 : public DirListDatum
{
public:
	DirListDatumMark6() : length(0), nStream(0) {}
	virtual ~DirListDatumMark6() { }
	bool setFromSListString(const char *str);	// Takes single scan entry from the slist file and populates this data structure (see below)
	virtual long long getLength() const { return length; }
	virtual void print(std::ostream &os, bool doEOL = true) const;
	void setScanId(int id) { scanId = id; }
	int getScanId() const { return scanId; }
private:
	bool setKeyValue(std::string key, std::string value);

	long long length;
	int nStream;
	int scanId;
};

int loadMark6SList(DirList &D, const char *fileName, std::stringstream &error);

std::ostream& operator << (std::ostream &os, const DirListDatumMark6 &x);

// Note: A single entry from ah slist file looks something like the following line:
//
// {'status': 'recorded', 'num_str': 1, 'start_tm': 1426829460, 'create_time': '2015y079d05h29m22s', 'sn': 'bbb_ccc_aaa', 'dur': 1, 'spc': 0, 'size': '1.028'}
//
// Where the fields are as follows:
// status :
// num_str : number of independent VDIF streams coalesced
// start_tm : start time (UNIX time)           FIXME: is this time of first sample?
// create_time : vex format time string        FIXME: what does this actually represent?
// sn : scan name.  Usually <expt>_<stn>_<scan>
// dur : duration (in seconds)                 FIXME: always integral?
// spc :                                       FIXME: what is this?
// size : file size of scan (MB), floating point


#endif
