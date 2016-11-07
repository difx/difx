#include <sstream>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cctype>
#include "dirlist_datum_mark6.h"

// Returns true if the value was parsed and used.
// It is not an error if it is not true.
bool DirListDatumMark6::setKeyValue(std::string key, std::string value)
{
	bool ok = true;

	if(key == "num_str")
	{
		nStream = atoi(value.c_str());
	}
	if(key == "dur")
	{
		duration = atoi(value.c_str());
	}
	if(key == "size")
	{
		length = static_cast<long long>(1000000.0*atof(value.c_str()) + 0.5);
	}
	if(key == "sn")
	{
		setName(value);
	}
	else
	{
		ok = false;
	}

	return ok;
}

// assume single set of enclosing { } with colon separated key:value pairs
// strings enclosed in single quotes
bool DirListDatumMark6::setFromSListString(const char *str)
{
	enum State
	{
		Outside,
		LookingForKey,
		GettingKey,
		LookingForColon,
		LookingForValue,
		GettingValue,
		LookingForComma,
		Done
	} state;

	bool inQuote = false;
	std::stringstream key;
	std::stringstream value;

	state = Outside;

	for(int i = 0; str[i]; ++i)
	{
		switch(state)
		{
		case Outside:
			if(str[i] == '{')
			{
				state = LookingForKey;
			}
			break;
		case LookingForKey:
			if(str[i] > ' ')
			{
				key.clear();
				key.str("");
				state = GettingKey;
				if(str[i] == '\'')
				{
					inQuote = true;
				}
				else
				{
					key << str[i];
				}
			}
			else if(str[i] == '}')
			{
				state = Done;
			}
			break;
		case GettingKey:
			if(str[i] == '\'')
			{
				if(inQuote)
				{
					state = LookingForColon;
					inQuote = false;
				}
				else
				{
					return false;
				}
			}
			else if(!inQuote && str[i] == ':')
			{
				state = LookingForValue;
			}
			else if(!inQuote && str[i] <= ' ')
			{
				state = LookingForColon;
			}
			else if(!inQuote && str[i] == '}')
			{
				return false;
			}
			else
			{
				key << str[i];
			}
			break;
		case LookingForColon:
			if(str[i] == ':')
			{
				state = LookingForValue;
			}
			else if(str[i] > ' ')
			{
				return false;
			}
			break;
		case LookingForValue:
			if(str[i] > ' ')
			{
				value.clear();
				value.str("");
				state = GettingValue;
				if(str[i] == '\'')
				{
					inQuote = true;
				}
				else
				{
					value << str[i];
				}
			}
			else if(str[i] == '}')
			{
				return false;
			}
			break;
		case GettingValue:
			if(str[i] == '\'')
			{
				if(inQuote)
				{
					setKeyValue(key.str(), value.str());
					state = LookingForComma;
					inQuote = false;
				}
				else
				{
					return false;
				}
			}
			else if(!inQuote && str[i] == ',')
			{
				setKeyValue(key.str(), value.str());
				state = LookingForKey;
			}
			else if(!inQuote && str[i] == '}')
			{
				setKeyValue(key.str(), value.str());
				state = Done;
			}
			else if(!inQuote && str[i] <= ' ')
			{
				setKeyValue(key.str(), value.str());
				state = LookingForComma;
			}
			else
			{
				value << str[i];
			}
			break;
		case LookingForComma:
			if(str[i] == ',')
			{
				state = LookingForKey;
			}
			else if(str[i] == '}')
			{
				state = Done;
			}
			else if(str[i] > ' ')
			{
				return false;
			}
			break;
		case Done:
			// leave loop: see below
			break;
		}
		if(state == Done)
		{
			break;
		}
	}

	return (state != Done);
}

void DirListDatumMark6::print(std::ostream &os, bool doEOL) const
{
	DirListDatum::print(os, false);
	os << " " << getLength();
	if(doEOL)
	{
		os << std::endl;
	}
}

std::ostream& operator << (std::ostream &os, const DirListDatumMark6 &x)
{
	os << dynamic_cast<const DirListDatum &>(x) << x.getLength();

	return os;
}


int loadMark6SList(DirList &D, const char *fileName, std::stringstream &error)
{
	enum State
	{
		Outside,
		LookingForId,
		GettingId,
		LookingForColon,
		LookingForData,
		GettingData,
		LookingForComma,
		Done
	} state;

	int nScan = 0;
	std::stringstream data;

	std::stringstream scanId;	// a number
	std::stringstream scanData;	// structure of the type to pass to setFromSListString

	const char *str;

	// 0. some initialization
	error.clear();
	error.str("");
	state = Outside;

	// 1. load full file into stringstream character array
	const int ReadBufSize = 1024;
	char readBuf[ReadBufSize + 1];
	FILE *in;
	int n;

	in = fopen(fileName, "r");
	if(!in)
	{
		error << "Mark6 SList file: " << fileName << " cannot be opened";

		return -1;
	}
	while(!feof(in))
	{
		n = fread(readBuf, 1, ReadBufSize, in);
		if(n > 0)
		{
			readBuf[n] = 0;
			data << readBuf;
		}
	}
	fclose(in);


	// 2. go through state machine

	str = data.str().c_str();

	for(int i = 0; str[i]; ++i)
	{
		switch(state)
		{
		case Outside:
			if(str[i] == '{')
			{
				state = LookingForId;
			}
			break;
		case LookingForId:
			scanId.clear();
			scanId.str();
			if(isdigit(str[i]))
			{
				state = GettingId;
				scanId << str[i];
			}
			else if(str[i] == '}')
			{
				state = Done;
			}
			else if(str[i] > ' ')
			{
				error << "Mark6 SList file: " << fileName << " is corrupt: parse error looking for scan number after " << nScan << " successful scans parsed";
				state = Done;
			}
			break;
		case GettingId:
			if(str[i] == ':')
			{
				state = LookingForData;
			}
			else if(str[i] <= ' ')
			{
				state = LookingForColon;
			}
			else if(isdigit(str[i]))
			{
				scanId << str[i];
			}
			else if(str[i] == '}')
			{
				state = Done;
			}
			else
			{
				error << "Mark6 SList file: " << fileName << " is corrupt: parse error looking reading scan number after " << nScan << " successful scans parsed";
				state = Done;
			}
			break;
		case LookingForColon:
			if(str[i] == ':')
			{
				state = LookingForData;
			}
			else if(str[i] > ' ')
			{
				error << "Mark6 SList file: " << fileName << " is corrupt: parse error looking for colon for scan Id " << scanId.str();
				state = Done;
			}
			break;
		case LookingForData:
			scanData.clear();
			scanData.str("");
			if(str[i] == '{')
			{
				state = GettingData;
				scanData << str[i];
			}
			else if(str[i] > ' ')
			{
				error << "Mark6 SList file: " << fileName << " is corrupt: unexpected character between colon and data for scan Id " << scanId.str();
				state = Done;
			}
			break;
		case GettingData:
			scanData << str[i];
			if(str[i] == '}')
			{
				DirListDatumMark6 *DM6;

				DM6 = new DirListDatumMark6;
				D.addDatum(DM6);
				bool ok = DM6->setFromSListString(scanData.str().c_str());
				if(ok)
				{
					++nScan;
					DM6->setScanId(atoi(scanId.str().c_str()));
				}
				else
				{
					error << "Mark6 SList file: " << fileName << " is corrupt: cannot parse data structure for scan Id " << scanId.str();
					state = Done;
				}

				scanId.clear();
				scanData.clear();
				scanId.str("");
				scanData.str("");
				state = LookingForComma;
			}
			break;
		case LookingForComma:
			if(str[i] == ',')
			{
				state = LookingForId;
			}
			else if(str[i] == '}')
			{
				state = Done;
			}
			else if(str[i] > ' ')
			{
				return false;
			}
			break;
		case Done:
			// leave loop: see below
			break;
		}
		if(state == Done)
		{
			break;
		}
	}

	if(error.str().empty())
	{
		D.organize();
	}
	else
	{
		return false;
	}

	return 0;
}
