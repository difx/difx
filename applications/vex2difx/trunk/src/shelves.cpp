#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include "shelves.h"

const std::string Shelves::NoLocation = "NONE";

int Shelves::load(const std::string &fileName)
{
	char s[1024], a[32], v[32], ms[32];
	std::ifstream is;
	int nWarn = 0;

	is.open(fileName.c_str());
	if(is.fail())
	{
		return 0;
	}

	for(int lineNum = 1; ; ++lineNum)
	{
		is.getline(s, 1024);
		if(is.eof())
		{
			break;
		}
		for(int i = 0; s[i]; ++i)
		{
			if(s[i] == '#')
			{
				s[i] = 0;
				break;
			}
		}

		if(strlen(s) < 5)
		{
			continue;
		}

		if(sscanf(s, "%31s%31s%31s", a, v, ms) != 3)
		{
			std::cerr << "Warning: line " << lineNum << " of " << fileName << " not parsable." << std::endl;

			++nWarn;
		}

		shelfMap[v] = ms;
	}

	is.close();

	return nWarn;
}

const std::string &Shelves::getShelf(const std::string &vsn) const
{
	std::map<std::string,std::string>::const_iterator it;
	it = shelfMap.find(vsn);
	if(it != shelfMap.end())
	{
		return it->second;
	}
	else
	{
		return NoLocation;
	}
}

int Shelves::snprintShelf(const std::string &vsn, char *outString, int maxLength) const
{
	std::map<std::string,std::string>::const_iterator it;
	it = shelfMap.find(vsn); 
	if(it != shelfMap.end())
	{
		return snprintf(outString, maxLength, "%s", it->second.c_str());
	}
	else
	{
		return snprintf(outString, maxLength, "NONE");
	}
}

std::ostream& operator << (std::ostream &os, const Shelves &x)
{
	os << "Shelf map:" << std::endl;
	for(std::map<std::string,std::string>::const_iterator it = x.shelfMap.begin(); it != x.shelfMap.end(); ++it)
	{
		os << "  " << it->first << " -> " << it->second << std::endl;
	}

	return os;
}

