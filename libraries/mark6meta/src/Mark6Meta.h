/*******************************************************************************
* Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany 
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
********************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#ifndef MARK6META_H
#define	MARK6META_H

#include <string>
#include <vector>
#include <map>
#include "Mark6Module.h"

/**
 * Custom exception class for reporting Mark6 related metadata errors
 */
class Mark6InvalidMetadata : public std::exception 
{
private:
    std::string err_msg;

public:
    Mark6InvalidMetadata(std::string msg) : err_msg(msg) {};
    ~Mark6InvalidMetadata() throw() {};
    const char *what() const throw() { return this->err_msg.c_str(); };
};

class Mark6Meta {
public:
    Mark6Meta();
    Mark6Meta(const Mark6Meta& orig);
    virtual ~Mark6Meta();
    std::string getEMSN() const;
    void parse(std::string);
    void reset();
    const std::map<int, std::string> &getSerials();
    std::vector<std::string> getGroup() const;
  
private:
    std::string eMSN_m;
    std::vector<std::string> group_m;
    std::map<int,std::string> serials_m;
};

#endif	/* MARK6META_H */

