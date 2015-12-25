#include <algorithm>
#include "dirlist.h"

DirList::~DirList()
{
	clear();
}

// empties contents on the datastructure
void DirList::clear()
{
	identifier.clear();

	for(std::vector<DirListParameter *>::iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		delete *it;
	}
	parameters.clear();

	for(std::vector<DirListDatum *>::iterator it = data.begin(); it != data.end(); ++it)
	{
		delete *it;
	}
	data.clear();
}

// loads a file in the native .dirlist format
void DirList::load(const char *filename)
{
	clear();
}

DirListParameter *DirList::getParameter(const std::string &key)
{
	for(std::vector<DirListParameter *>::iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		if((*it)->getKey() == key)
		{
			return *it;
		}
	}

	return 0;
}

bool DirList::hasParameter(const std::string &key)
{
	for(std::vector<DirListParameter *>::iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		if((*it)->getKey() == key)
		{
			return true;
		}
	}

	return false;
}

bool DirList::isParameterTrue(const std::string &key)
{
	DirListParameter *P;

	P = getParameter(key);
	if(!P)
	{
		return false;
	}

	return P->isTrue();
}

bool DirList::isParameterFalse(const std::string &key)
{
	DirListParameter *P;

	P = getParameter(key);
	if(!P)
	{
		return false;
	}

	return P->isFalse();
}

void DirList::addDatum(DirListDatum *datum)
{
	data.push_back(datum);
}

bool compare_datum_star(const DirListDatum *a, const DirListDatum *b) 
{
	return (*a < *b);
}

void DirList::sort()
{
	std::sort(data.begin(), data.end(), compare_datum_star);
}

void DirList::setStationAndExperiments()
{
}

// Note: Now this algorithm is pretty simple.  It will not detect a common super-directory if there are different subdirectories.
void DirList::setPathPrefix()
{
	std::string common;
	bool first = true;
	size_t pos;

	if(hasParameter("pathPrefix"))
	{
		return;
	}

	for(std::vector<DirListDatum *>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		pos = (*it)->getName().rfind('/');
		if(pos == std::string::npos)
		{
			// no possibility of common string portion
			return;
		}
		else
		{
			++pos;
		}

		if(first)
		{
			common = (*it)->getName().substr(0, pos);
			first = false;
		}
		else
		{
			if(common != (*it)->getName().substr(0, pos))
			{
				return;
			}
		}
	}

	// if we got here then there is a common string

	setParameter("pathPrefix", common);
	for(std::vector<DirListDatum *>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		(*it)->setName((*it)->getName().substr(pos));
	}
}

void DirList::print(std::ostream &os) const
{
	os << identifier << std::endl;
	os << std::endl;
	
	for(std::vector<DirListParameter *>::const_iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		(*it)->print(os);
	}
	os << std::endl;

	for(std::vector<DirListDatum *>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		(*it)->print(os, false);
		if((*it)->hasComment())
		{
			(*it)->printComment(os, false);
		}
		os << std::endl;
	}
}
