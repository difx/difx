/***************************************************************************
 *   Copyright (C) 2014-2015 by Chris Phillips & Walter Brisken            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <iostream>
#include <cstdio>
#include <cstring>
#include <sstream>
#include <cmath>
#include <cstdlib>
#include "timeutils.h"
#include "parserhelp.h"

enum charType whatChar(const char a)
{
    if (a=='+'||a=='-') 
      return SIGN;
    else if (a=='E'||a=='e')
      return (E);
    else if (a>='0'&&a<='9')
      return DIGIT;
    else if (a==' ')
      return SPACE;
    else if (a=='.')
      return DOT;
    else
      return CHARERROR;
}

int getDouble(std::string &value, double &x)
{
    enum stateType {START, STARTINT, INTEGER, DECIMAL, STARTEXP, EXPONENT, END, ERROR};
    enum stateType state = START;
    enum charType what;

    unsigned int i;
    for (i=0 ; i<value.length(); i++) {
      what = whatChar(value[i]);
      
      switch (state) {
      case START:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	  state = STARTINT;
	  break;
	case DIGIT:
	  state = INTEGER;
	  break;
	case SPACE:
	  break;
	case E:
	  state = ERROR;
	  break;
	case DOT:
	  state=DECIMAL;
	  break;
	}
	break;
	
      case STARTINT:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	case E:
	  state = ERROR;
	  break;
	case DIGIT:
	  state = INTEGER;
	  break;
	case SPACE:
	  break;
	case DOT:
	  state = DECIMAL;
	}
	break;

      case INTEGER:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case DIGIT:
	  break;
	case SIGN:
	case SPACE:
	  state = END;
	  break;
	case E:
	  state = STARTEXP;
	  break;
	case DOT:
	  state = DECIMAL;
	}
	break;
	
      case DECIMAL:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case DIGIT:
	  break;
	case SIGN:
	case SPACE:
	  state = END;
	  break;
	case E:
	  state = STARTEXP;
	  break;
	case DOT:
	  state = ERROR;
	  break;
	}
	break;
	
      case STARTEXP:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	case DIGIT:
	  state = EXPONENT;
	  break;
	case SPACE:
	case E:
	case DOT:
	  state = ERROR;
	  break;
	}
	break;
	
      case EXPONENT:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SPACE:
	case SIGN:
	  state = END;
	  break;
	case DIGIT:
	  break;
	case DOT:
	case E:
	  state = ERROR;
	  break;
	}
	break;

      case ERROR:
      case END:
	break;
	
      }
      
      if (state==ERROR) {
	std::cerr << "Error parsing \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      }
      if (state==END) break;
    }

    std::stringstream ss;
    if (state==START) {
      value = "";
      return 1;
    } else if (state==END) {
    } else {
      i = value.length();
    }
    ss << value.substr(0,i);
    ss >> x;
    value  = value.substr(i);

    return 0;
}
  
int getOp(std::string &value, int &plus) {
    enum charType what;

    unsigned int i;
    for (i=0 ; i<value.length(); i++) {
      what = whatChar(value[i]);
      
      if (what==CHARERROR) {
	std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      } else if (what==SPACE) {
	continue;
      } else if (what==SIGN) {
	if (value[i]=='+') {
	  plus = 1;
	} else {
	  plus = 0;
	} 
	value = value.substr(i+1);
	return(0);
      } else {
	std::cerr << "Unexpected character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      }
    }
    return 1; // Did not match anything
  }

// Read a string consisting of a series of additions and subtrations (only) and return a double
double parseDouble(const std::string &value)
{
  std::string str = value; // Copy as the procedure destroys the string
  
  int status, number=1, sign=-1;
  double thisvalue, result=0;
  while (str.length()) {
    if (number) {
      status = getDouble(str, thisvalue);
      if (status) break;
      if (sign==-1)
	result = thisvalue;
      else if (sign==1) 
	result += thisvalue;
      else
	result -= thisvalue;
      number = 0;
    
    } else  {
      status = getOp(str, sign);
      if (status) break;
      number = 1;
    }
  }

  return result;
}

// Turns a string into MJD 
// The following formats are allowd:
// 1. decimal mjd:  55345.113521
// 2. ISO 8601 dateTtime strings:  2009-03-08T12:34:56.121
// 3. VLBA-like time:   2009MAR08-12:34:56.121
// 4. vex time: 2009y245d08h12m24s"
double parseTime(const std::string &timeStr)
{
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
		tm.tm_mday = tm.tm_yday+1; // mktime not guaranteed to pick up dayno
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

	// No match
	std::cerr << std::endl;
	std::cerr << "Error: date not parsable: " << timeStr << std::endl;
	std::cerr << std::endl;
	std::cerr << "Allowable formats are:" << std::endl;
	std::cerr << "1. Straight MJD        54345.341944" << std::endl;
	std::cerr << "2. Vex formatted date  2009y245d08h12m24s" << std::endl;
	std::cerr << "3. VLBA-like format    2009SEP02-08:12:24" << std::endl;
	std::cerr << "4. ISO 8601 format     2009-09-02T08:12:24" << std::endl;
	std::cerr << std::endl;

	exit(EXIT_FAILURE);
}

double parseCoord(const char *str, char type)
{
	int sign = 1, l, n;
	double a, b, c;
	double v = -999999.0;

	if(type != ' ' && type != 'R' && type != 'D')
	{
		std::cerr << "Programmer error: parseCoord: parameter 'type' has illegal value = " << type << std::endl;
		
		exit(EXIT_FAILURE);
	}

	if(str[0] == '-')
	{
		sign = -1;
		++str;
	}
	else if(str[0] == '+')
	{
		++str;
	}

	l = strlen(str);

	if(sscanf(str, "%lf:%lf:%lf", &a, &b, &c) == 3)
	{
		v = sign*(a + b/60.0 + c/3600.0);
		if(type == 'D')
		{
			v *= M_PI/180.0;
		}
		else
		{
			v *= M_PI/12.0;
		}
	}
	else if(sscanf(str, "%lfh%lfm%lf", &a, &b, &c) == 3 && str[l-1] == 's' && type != 'D')
	{
		v = sign*(a + b/60.0 + c/3600.0);
		v *= M_PI/12.0;
	}
	else if(sscanf(str, "%lfd%lf'%lf\"", &a, &b, &c) == 3 && str[l-1] == '"' && type == 'D')
	{
		v = sign*(a + b/60.0 + c/3600.0);
		v *= M_PI/180.0;
	}
	else if(sscanf(str, "%lf%n", &a, &n) == 1)
	{
		if(n == l)
		{
			v = a;
		}
		else if(strcmp(str+n, "rad") == 0)
		{
			v = a;
		}
		else if(strcmp(str+n, "deg") == 0)
		{
			v = a*M_PI/180.0;
		}
		else
		{
			std::cerr << "Error parsing coordinate value " << str << std::endl;

			exit(EXIT_FAILURE);
		}
		v *= sign;
	}

	return v;
}

// From http://oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html
void split(const std::string &str, std::vector<std::string> &tokens, const std::string &delimiters)
{
	// Skip delimiters at beginning.
	std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
	// Find first "non-delimiter".
	std::string::size_type pos     = str.find_first_of(delimiters, lastPos);

	while(std::string::npos != pos || std::string::npos != lastPos)
	{
		// Found a token, add it to the vector.
		tokens.push_back(str.substr(lastPos, pos - lastPos));
		// Skip delimiters.  Note the "not_of"
		lastPos = str.find_first_not_of(delimiters, pos);
		// Find next "non-delimiter"
		pos = str.find_first_of(delimiters, lastPos);
	}
}

bool parseBoolean(const std::string &str)
{
	if(str[0] == '0' || str[0] == 'f' || str[0] == 'F' || str[0] == '-')
	{
		return false;
	}
	else
	{
		return true;
	}
}
