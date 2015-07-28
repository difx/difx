#ifndef __SHELVES_H__
#define __SHELVES_H__

#include <iostream>
#include <map>
#include <string>

class Shelves
{
public:
	int load(const std::string &fileName);
	int snprintShelf(const std::string &vsn, char *outString, int maxLength) const;
	const std::string &getShelf(const std::string &vsn) const;
	bool hasShelf(const std::string &vsn) const { return (shelfMap.find(vsn) != shelfMap.end()); }
	void setShelf(const std::string &vsn, const std::string &location) { shelfMap[vsn] = location; }
	void clear() { shelfMap.clear(); }
	bool empty() { return shelfMap.empty(); }
	size_t size() { return shelfMap.size(); }

	std::map<std::string,std::string> shelfMap;	// maps VSN to shelf location
	static const std::string NoLocation;
};

std::ostream& operator << (std::ostream &os, const Shelves &x);

#endif
