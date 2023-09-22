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
#ifndef MARK6MODULE_H
#define	MARK6MODULE_H

#include <string>
#include <vector>
#include <map>

class Mark6DiskDevice;

class Mark6Module {
public:
    static const int MAXDISKS = 8;   // number of disks per mark6 module
    Mark6Module();
    Mark6Module(const Mark6Module& orig);
    virtual ~Mark6Module();
    void addDiskDevice(Mark6DiskDevice &device);
    void removeDiskDevice(Mark6DiskDevice &device);
    Mark6DiskDevice *getDiskDevice(int index);
    std::string getEMSN();
    void setEMSN(std::string eMSN);
    int getNumDiskDevices();
    int getNumTargetDisks();
    bool isComplete();
    void setGroupMembers(std::vector<std::string> groupMembers_m);
    std::vector<std::string> getGroupMembers() const;
    
private:
    
    std::string eMSN_m;
    //Mark6DiskDevice *diskDevices_m;   
    std::vector<std::string> groupMembers_m;
    std::map<int,Mark6DiskDevice> diskDevices_m;
    
    
};

#endif	/* MARK6MODULE_H */

