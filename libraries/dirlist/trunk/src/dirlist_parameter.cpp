#include <cstdlib>
#include <sstream>
#include <iostream>
#include "parse.h"
#include "dirlist_parameter.h"
#include "dirlist_exception.h"

bool DirListParameter::setFromTokens(const std::vector<std::string> &tokens)
{
	std::stringstream ss;

	items.clear();

	setKey(tokens[0]);

	for(std::vector<std::string>::const_iterator it = tokens.begin() + 2; it != tokens.end(); ++it)
	{
		ss << *it;
	}
	value = ss.str();
	
	separateStringList(items, ss.str());

	return true;
}

int DirListParameter::getInt(unsigned int index) const
{
	if(index >= items.size())
	{
		throw DirListException("getInt(): index out of range.");
	}

	return atoi(items[index].c_str());
}

double DirListParameter::getDouble(unsigned int index) const
{
	if(index >= items.size())
	{
		throw DirListException("getDouble(): index out of range.");
	}

	return atof(items[index].c_str());
}

const std::string &DirListParameter::getString(unsigned int index) const
{
	if(index >= items.size())
	{
		throw DirListException("getString(): index out of range.");
	}

	return items[index];
}

void DirListParameter::setValue(const std::string &v)
{
	std::vector<std::string> stringList;
	std::vector<std::string>::const_iterator it;
	value = v;

	items.clear();

	separateStringList(stringList, v);
	for(it = stringList.begin(); it != stringList.end(); ++it)
	{
		items.push_back(unquoteString(*it));
	}
}

// Diagnostic print out
void DirListParameter::print() const
{
	std::cout << "DirListParameter" << std::endl;
	std::cout << "  Key = " << getKey() << std::endl;
	std::cout << "  Value = " << getValue() << std::endl;
	if(hasComment())
	{
		std::cout << "  Comment = " << getComment() << std::endl;
	}
	else
	{
		std::cout << "  hasComment = False" << std::endl;
	}
	std::cout << "  nItem = " << size() << std::endl;
	for(unsigned int i = 0; i < size(); ++i)
	{
		std::cout << "  Item[" << i << "] = " << getString(i) << std::endl;
	}
	std::cout << "  Full line = " << *this << std::endl;
}

void DirListParameter::print(std::ostream &os, bool doEOL) const
{
	os << getKey() << " = " << getValue();
	if(hasComment())
	{
		os << " #" << getComment();
	}
	if(doEOL)
	{
		os << std::endl;
	}
}

std::ostream& operator << (std::ostream &os, const DirListParameter &x)
{
	os << x.getKey() << " = " << x.getValue();
	if(x.hasComment())
	{
		os << " #" << x.getComment();
	}

	return os;
}
