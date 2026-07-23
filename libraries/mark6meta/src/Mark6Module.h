/*******************************************************************************
* Copyright (C) 2024  Max-Planck-Institut für Radioastronomie, Bonn, Germany 
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
    virtual ~Mark6Module();
    void addDiskDevice(Mark6DiskDevice &device);
    void removeDiskDevice(Mark6DiskDevice &device);
    Mark6DiskDevice *getDiskDevice(int index);
    std::string getEMSN();
    std::string getVSN();
    unsigned long getCapacity();
    unsigned long getDatarate();
    void setVSN(std::string vsn);
    void setCapacity(unsigned long capacity);
    void setDatarate(unsigned long datarate);
    int getNumDiskDevices();
    int getNumTargetDisks();
    bool isComplete();
    void setGroupMembers(std::vector<std::string> groupMembers_m);
    std::vector<std::string> getGroupMembers() const;
    void updateMetaFromEMSN(std::string eMSN);
    
private:
    
    std::string vsn_m;
    unsigned long capacityMB_m;
    unsigned long datarate_m;
    std::string eMSN_m;
    //Mark6DiskDevice *diskDevices_m;   
    std::vector<std::string> groupMembers_m;
    std::map<int,Mark6DiskDevice> diskDevices_m;
    void resetEMSN();
    void clearMeta();
};

#endif	/* MARK6MODULE_H */

