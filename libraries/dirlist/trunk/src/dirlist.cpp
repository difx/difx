#include <vector>
#include <set>
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
		throw DirListException("DirList::load(): Cannot open file.", DirListException::TypeCantOpen);
	}

	v = fgetsNoCR(line, MaxLineLength, in);
	if(strcmp(line, DIRLIST_IDENTIFIER_LINE) != 0)
	{
		fclose(in);

		throw DirListException("DirList::load(): Wrong identifier line.", DirListException::TypeWrongIdentifier);
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
			std::stringstream msg;

			msg << "DirList::load(): Data line with 1 or 2 tokens.  Line = " << lineNum;

			throw DirListException(msg.str(), DirListException::TypeParseError);
		}

		if(tokens[1] == "=")	// It is a parameter
		{
			DirListParameter *P;

			if(hasData)
			{
				fclose(in);
				std::stringstream msg;

				msg << "DirList::load(): Parameter line found after data.  Line = " << lineNum;

				throw DirListException(msg.str(), DirListException::TypeParseError);
			}

			P = new DirListParameter();
			bool ok = P->setFromTokens(tokens);
			if(!ok)
			{
				fclose(in);
				delete P;

				std::stringstream msg;

				msg << "DirList::load(): Unparsable parameter line.  Line = " << lineNum;

				throw DirListException(msg.str(), DirListException::TypeParseError);
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
					throw DirListException("DirList::load(): class parameter was not set.", DirListException::TypeParseError);
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

int DirList::sanityCheck() const
{
	return 0;
}

const DirListDatumMark5 *DirList::getMark5Scan(unsigned int index) const
{
	if(index >= data.size())
	{
		return 0;
	}
	if(getConstParameter("class")->getValue() != "mark5")
	{
		return 0;
	}

	return dynamic_cast<const DirListDatumMark5 *>(data[index]);
}

void DirList::addParameter(DirListParameter *param)
{
	for(std::vector<DirListParameter *>::iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		if((*it)->getKey() == param->getKey())
		{
			delete *it;
			parameters.erase(it);
			break;
		}
	}

	parameters.push_back(param);
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

const DirListParameter *DirList::getConstParameter(const std::string &key) const
{
	for(std::vector<DirListParameter *>::const_iterator it = parameters.begin(); it != parameters.end(); ++it)
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


static void split(const std::string &s, char delim, std::vector<std::string> &elems)
{
	std::stringstream ss(s);
	std::string item;
	while(std::getline(ss, item, delim))
	{
		elems.push_back(item);
	}
}

// BT127J2_HN_No0047BT127J2_HN_No0047
// NRAO+324_0008_TS036H_PT_No0001

// General strategy:
// 1. lop everything before last / (including / itself)
// 2. lop everything after first . (including . itself)
// 3. split string by _ separator
// 4. experiment is third to last field
// 5. station is second to last field
// 6. if number of stations found is not exactly 1, then don't do anything
void DirList::setStationAndExperiments()
{
	std::set<std::string> stations;
	std::set<std::string> expts;
	std::vector<std::string> elems;
	size_t start, stop, length;

	for(std::vector<DirListDatum *>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		const std::string &str = (*it)->getName();
		length = str.size();
		start = 0;
		stop = length;
		
		for(size_t i = 0; i < length; ++i)
		{
			if(str[i] == '/')
			{
				start = i+1;
				stop = length;
			}
			else if(str[i] == '.')
			{
				stop = i;
			}
		}

		split(str.substr(start, stop-start), '_', elems);
		if(elems.size() > 2)
		{
			expts.insert(elems[elems.size() - 3]);
			stations.insert(elems[elems.size() - 2]);
		}
		elems.clear();
	}

	if(stations.size() == 1)
	{
		std::stringstream value;

		for(std::set<std::string>::const_iterator it = expts.begin(); it != expts.end(); ++it)
		{
			if(!value.str().empty())
			{
				value << ",";
			}
			value << *it;
		}

		setParameter("station", *stations.begin());
		addParameter(new DirListParameter("experiments", value.str()));
	}
}

void DirList::setTimerange()
{
	double mjdStart = 1.0e9;
	double mjdStop = -1.0e9;
	double s, e;

	for(std::vector<DirListDatum *>::const_iterator it = data.begin(); it != data.end(); ++it)
	{
		s = (*it)->getFullMjdStart();
		e = s + (*it)->getDuration();
		if(s < mjdStart)
		{
			mjdStart = s;
		}
		if(e > mjdStop)
		{
			mjdStop = e;
		}
	}

	if(mjdStart < mjdStop)
	{
		setParameter("startMJD", mjdStart);
		setParameter("stopMJD", mjdStop);
	}
}

// returns true if comment contains nothing but whitespace
static bool isEmptyComment(const std::string &str)
{
	int l = str.size();

	for(int i = 0; i < l; ++i)
	{
		if(str[i] > ' ')
		{
			return false;
		}
	}

	return true;
}

void DirList::removeEmptyComments()
{
	for(std::vector<DirListDatum *>::iterator it = data.begin(); it != data.end(); ++it)
	{
		if(isEmptyComment((*it)->getComment()))
		{
			(*it)->clearComment();
		}
	}

	for(std::vector<DirListParameter *>::iterator it = parameters.begin(); it != parameters.end(); ++it)
	{
		if(isEmptyComment((*it)->getComment()))
		{
			(*it)->clearComment();
		}
	}
}

void DirList::organize()
{
	sort();
	setStationAndExperiments();
	setTimerange();
	removeEmptyComments();
}

// Note: Now this algorithm is pretty simple.  It will not detect a common super-directory if there are different subdirectories.
void DirList::setPathPrefix()
{
	std::string common;
	bool first = true;
	size_t pos = 0;

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
