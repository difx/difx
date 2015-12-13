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


// FIXME... TBD
class DirListMark6 : public DirListDatum
{
};

class DirList
{
public:
	DirList() : identifierLine(DIRLIST_IDENTIFIER_LINE) {};
	~DirList();

private:
	std::string identifierLine;
	std::vector<DirListParameter *> parameter;	// vector so items have an order
	std::vector<DirListDatum *> data;		// pointers to scan data
};

std::ostream& operator << (std::ostream &os, const DirList &x);

#endif
