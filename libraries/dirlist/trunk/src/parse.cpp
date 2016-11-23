#include <iostream>
#include <cstdio>
#include <cstdlib>
#include "parse.h"

// FIXME: handle exceptions in this file

// inputString here must have already been tokenized
void separateStringList(std::vector<std::string> &outputList, const std::string &inputString)
{
	unsigned int len = inputString.size();
	int start = -1, stop = -1;
	unsigned int state = 0;		// 0 = outside string, 1 = inside '', 2 = inside ""

	outputList.clear();

	for(unsigned int i = 0; ; ++i)
	{
		if(i == len || (state == 0 &&  inputString[i] == ',') )
		{
			if(start >= 0)
			{
				outputList.push_back(inputString.substr(start, stop-start+1));
				start = -1;
			}
			else
			{
				outputList.push_back("");
			}

			if(i == len)
			{
				break;
			}
		}
		else if(inputString[i] > ' ')
		{
			if(start < 0)
			{
				start = i;
			}
			stop = i;
		}
		if(inputString[i] == '\'')
		{
			if(state == 0)
			{
				state = 1;
			}
			else if(state == 1)
			{
				state = 0;
			}
		}
		else if(inputString[i] == '\"')
		{
			if(state == 0)
			{
				state = 2;
			}
			else if(state == 2)
			{
				state = 0;
			}
		}
	}
}

bool tokenize(std::vector<std::string> &tokens, std::string &comment, const std::string &inputString)
{
	unsigned int len = inputString.size();
	int start = -1, stop = -1;
	unsigned int state = 0;		// 0 = outside string, 1 = inside '', 2 = inside ""
	
	tokens.clear();
	comment.clear();

	for(unsigned int i = 0;; ++i)
	{
		if(i == len)
		{
			if(state == 0)
			{
				if(start >= 0)
				{
					tokens.push_back(inputString.substr(start, stop-start+1));
				}
			}
			else
			{
				return false;
			}

			break;
		}
		if(inputString[i] == '\'')
		{
			if(state == 0)
			{
				state = 1;
				start = i+1;
				stop = i;
			}
			else if(state == 1)
			{
				state = 0;
				tokens.push_back(inputString.substr(start, stop-start+1));
				start = -1;
			}
		}
		else if(inputString[i] == '\"')
		{
			if(state == 0)
			{
				state = 1;
				start = i+1;
				state = 2;
			}
			else if(state == 2)
			{
				state = 0;
				tokens.push_back(inputString.substr(start, stop-start+1));
				start = -1;
			}
		}
		else if(state == 0 && inputString[i] == '#')
		{
			if(start >= 0)
			{
				tokens.push_back(inputString.substr(start, stop-start+1));
			}
			comment = inputString.substr(i+1, len-i);

			break;
		}
		else if(inputString[i] == '=' || inputString[i] == ',')
		{
			if(start >= 0)
			{
				stop = i-1;
				tokens.push_back(inputString.substr(start, stop-start+1));
				start = -1;
			}
			tokens.push_back(inputString.substr(i, 1));
		}
		else if(state == 0 && inputString[i] <= ' ')
		{
			if(start >= 0)
			{
				tokens.push_back(inputString.substr(start, stop-start+1));
				start = -1;
			}
		}
		else
		{
			if(start < 0)
			{
				start = i;
			}
			stop = i;
		}
	}

	return true;
}

// For now, this is simple: just look to see if first and last quotes are same, and if so, return 1..-2
// otherwise return 0..-1
std::string unquoteString(const std::string &str)
{
	unsigned int len;

	len = str.size();

	if(len >= 2 && (str[0] == '\"' || str[0] == '\'') && str[0] == str[len-1])
	{
		return str.substr(1, len-2);
	}
	else
	{
		return str;
	}
}

// Turns a string into MJD 
// The following formats are allowd:
// 1. decimal mjd:  55345.113521
// 2. ISO 8601 dateTtime strings:  2009-03-08T12:34:56.121
// 3. VLBA-like time:   2009MAR08-12:34:56.121
// 4. vex time: 2009y245d08h12m24s"
double parseTime(const std::string &timeStr, std::stringstream &error)
{
	const double MJD_UNIX0=40587.0; // MJD at beginning of unix time
	const int TimeLength=54;
	double mjd;
	char str[TimeLength];
	char *p;
	int n;
	struct tm tm;
	char dummy;

	memset(&tm, 0, sizeof(struct tm));
	snprintf(str, TimeLength, "%s", timeStr.c_str());

	// Test for ISO 8601
	p = strptime(str, "%FT%T", &tm);
	if(!p)
	{
		//Test for VLBA-like
		p = strptime(str, "%Y%b%d-%T", &tm);
	}
	if(!p)
	{
		//Test for Vex
		p = strptime(str, "%Yy%jd%Hh%Mm%Ss", &tm);
	}
	if(p)
	{
		return mktime(&tm)/86400.0 + MJD_UNIX0;
	}

	n = sscanf(str, "%lf%c", &mjd, &dummy);
	if(n == 1)
	{
		// Must be straight MJD value
		return mjd;
	}

	error << "Time parse error";

	return 0.0;
}
