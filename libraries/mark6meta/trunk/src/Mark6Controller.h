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
// $Id: Mark6Module.h 7764 2017-05-16 18:23:07Z WalterBrisken $
// $HeadURL: $
// $LastChangedRevision: 7764 $
// $Author: WalterBrisken $
// $LastChangedDate: 2017-05-16 20:23:07 +0200 (Tue, 16 May 2017) $
//
//============================================================================
#ifndef MARK6CONTROLLER_H
#define	MARK6CONTROLLER_H

#include <string>
#include <vector>
#include <map>


class Mark6Controller {
public:
    Mark6Controller();
    virtual ~Mark6Controller();
    std::string getName() const;
    void setName(std::string name);
    std::string getPath() const;
    void setPath(std::string path);
    std::string getSysNum() const;
    void setSysNum(std::string path);
    int getOrder() const;
    void setOrder(int order);
    
private:
    
    std::string name_m;
    std::string path_m;
    std::string sysnum_m;
    int order_m;
};

#endif	/* MARK6CONTROLLER_H */

