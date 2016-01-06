#ifndef __DIRLIST_DATUM_H__
#define __DIRLIST_DATUM_H__

#include <string>
#include <vector>
#include <ostream>

// corresponds to a single scan;
class DirListDatum
{
public:
	DirListDatum() : mjdStart(0), secStart(0.0), duration(0.0) {}
	virtual ~DirListDatum() {}
	const std::string &getName() const { return name; }
	int getMjdStart() const { return mjdStart; }
	double getSecStart() const { return secStart; }
	int getIntSecStart() const { return static_cast<int>(secStart); }
	int getIntNSStart() const { return static_cast<int>(1000000000.0*(secStart - static_cast<int>(secStart) )); }
	double getFullMjdStart() const { return mjdStart + secStart/86400.0; }
	int getFullMjdEnd() const { return mjdStart + (secStart+duration)/86400.0; }
	double getDuration() const { return duration; }
	void setName(const std::string &n) { name = n; }
	void setMjdStart(int mjd) { mjdStart = mjd; }
	void setSecStart(double sec) { secStart = sec; }
	void setStart(int mjd, double sec) { mjdStart = mjd; secStart = sec; }
	void setDuration(double dur) { duration = dur; }
	virtual void print(std::ostream &os, bool doEOL = true) const;
	
	void printComment(std::ostream &os, bool doEOL = true) const;
	bool hasComment() const { return !comment.empty(); }
	const std::string &getComment() const { return comment; }
	void setComment(const std::string &str) { comment = str; }
	void clearComment() { comment.clear(); }

	virtual long long getStartPointer() const { return 0; }	// FIXME: should throw
	virtual long long getLength() const { return 0; } // FIXME: should throw
	virtual int getTracks() const { return 1; } // FIXME: should throw
	virtual bool setFromTokens(const std::vector<std::string> &tokens);

	bool setFromOldFileListString(const char *str);

	bool operator<(const DirListDatum &rhs) const { return getFullMjdStart() < rhs.getFullMjdStart(); }

protected:
	// start time in MJD = mjdStart + secStart/86400.0
	int mjdStart;
	double secStart;
	double duration;		// [sec]
	std::string name;		// name of file or scan
	std::string comment;
};

std::ostream& operator << (std::ostream &os, const DirListDatum &x);

#endif
