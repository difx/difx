#include "dirlist.h"

// empties contents on the datastructure
void DirList::clear()
{
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
}

void DirList::setExperiments()
{
}

void DirList::setStation()
{
}


