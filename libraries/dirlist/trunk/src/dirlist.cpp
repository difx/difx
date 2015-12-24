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

void DirList::sort()
{
}

void DirList::setExperiments()
{
}

void DirList::setStation()
{
}

void DirList::setPathPrefix()
{
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
