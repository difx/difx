#include <vector>
#include <string>
#include <algorithm>
#include <cstdio>
#include <cstring>
#include <iostream>
#include "dirlist.h"
#include "dirlist_datum.h"
#include "dirlist_datum_mark5.h"
#include "utils.h"
#include "parse.h"

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
void DirList::load(const char *fileName)
{
	const int MaxLineLength = 512;
	FILE *in;
	char line[MaxLineLength+1];
	char *v;
	std::vector<std::string> tokens;
	std::string comment;
	bool hasData = false;
	DirListParameter *classParameter = 0;

	clear();
	setDefaultIdentifier();

	in = fopen(fileName, "r");
	if(!in)
	{
		throw DirListException("DirList::load(): Cannot open file.");
	}

	v = fgetsNoCR(line, MaxLineLength, in);
	if(strcmp(line, DIRLIST_IDENTIFIER_LINE) != 0)
	{
		fclose(in);

		throw DirListException("DirList::load(): Wrong identifier line.");
	}

	for(int lineNum = 2; !feof(in); ++lineNum)
	{
		v = fgetsNoCR(line, MaxLineLength, in);
		if(v == 0)
		{
			break;
		}
		
		tokenize(tokens, comment, line);

		if(tokens.empty())
		{
			continue;
		}

		if(tokens.size() < 3)
		{
			fclose(in);

			throw DirListException("DirList::load(): Data line with 1 or 2 tokens.  Line = ", lineNum);
		}

		if(tokens[1] == "=")	// It is a parameter
		{
			DirListParameter *P;

			if(hasData)
			{
				fclose(in);

				throw DirListException("DirList::load(): Parameter line found after data.  Line = ", lineNum);
			}

			P = new DirListParameter();
			bool ok = P->setFromTokens(tokens);
			if(!ok)
			{
				fclose(in);
				delete P;

				throw DirListException("DirList::load(): Unparsable parameter line.  Line = ", lineNum);
			}
			P->setComment(comment);
			addParameter(P);
		}
		else
		{
			DirListDatum *DD;

			if(!hasData)
			{
				hasData = true;
				classParameter = getParameter("class");
				if(!classParameter)
				{
					throw DirListException("DirList::load(): class parameter was not set.");
				}
			}

			if(classParameter->getValue() == "mark5")
			{
				DD = new DirListDatumMark5;
			}
			else if(classParameter->getValue() == "file")
			{
				DD = new DirListDatum;
			}
			else
			{
				throw DirListException("DirList::load(): unsupported class parameter.");
			}

			DD->setFromTokens(tokens);
			DD->setComment(comment);

			addDatum(DD);
		}
	}

	fclose(in);
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
