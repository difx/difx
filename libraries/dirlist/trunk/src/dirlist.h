#ifndef __DIRLIST_H__
#define __DIRLIST_H__

#include <ostream>
#include <utility>
#include <vector>
#include <string>
#include "dirlist_exception.h"
#include "dirlist_parameter.h"
#include "dirlist_datum.h"

// FIXME: move to class constant?
#define DIRLIST_IDENTIFIER_LINE	"VLBI baseband data listing"


class DirList
{
#if 0
	Not yet sure how to handle this.  May be cleaner to use a DirListParameter?

	enum FileType
	{
		UnknownFile,
		NativeFile,
		Mark5DirFile,
		VSumFile,
		Mark6SListFile
	};
#endif
public:
	DirList() : identifierLine(DIRLIST_IDENTIFIER_LINE) {};
	~DirList();
	void load(const char *filename);
	void clear();
	void setDefaultIdentifier() { identifier = DIRLIST_IDENTIFIER_LINE; }
	void setIdentifier(const std::string &str) { identifier = str; }
	void setParameter(const std::string &key, const std::string &value, const std::string &comment = "");
	void setParameter(const std::string &key, double value, const std::string &comment = "");
	void setParameter(const std::string &key, int value, const std::string &comment = "");
	void setParameter(const std::string &key, bool value, const std::string &comment = "");
	bool isParameterTrue(const std::string &key);
	bool isParameterFalse(const std::string &key);
	void addDatum(DirListDatum *datum);
	void setExperiments();
	void setStation();

private:
//	enum FileType fileType;

	std::string identifier;
	std::vector<DirListParameter *> parameter;	// vector so items have an order
	std::vector<DirListDatum *> data;		// pointers to scan data
};

std::ostream& operator << (std::ostream &os, const DirList &x);

#endif
