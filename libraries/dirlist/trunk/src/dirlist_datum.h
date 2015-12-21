#ifndef __DIRLIST_DATUM_H__
#define __DIRLIST_DATUM_H__

#include <string>
#include <ostream>

// corresponds to a single scan;
class DirListDatum
{
public:
	DirListDatum() : mjdStart(0.0), mjdStop(0.0) {}
	~DirListDatum() {}
	const std::string &getName() const { return name; }
	double getMjdStart() const { return mjdStart; }
	double getMjdStop() const { return mjdStop; }
	void setName(const std::string &n) { name = n; }
	void setMjdStart(double mjd) { mjdStart = mjd; }
	void setMjdStop(double mjd) { mjdStop = mjd; }

private:
	double mjdStart, mjdStop;
	std::string name;		// name of file or scan
};

std::ostream& operator << (std::ostream &os, const DirListDatum &x);

#endif
