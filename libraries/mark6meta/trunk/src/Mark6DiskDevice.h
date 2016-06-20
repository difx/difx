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
#ifndef MARK6DISKDEVICE_H
#define	MARK6DISKDEVICE_H

#include <string>
#include <vector>
#include "Mark6Meta.h"


class Mark6DiskDevice {
public:
    
    struct Mark6Partition
    {
        std::string deviceName;
        std::string mountPath;
        //std::string linkPath;
        // sort Mark6Partition by deviceName
        static bool sortByName(const Mark6Partition &lhs, const Mark6Partition &rhs) { return lhs.deviceName < rhs.deviceName; }
    };
    
    bool sortByName(const Mark6Partition &lhs, const Mark6Partition &rhs); 
    
    Mark6DiskDevice(std::string deviceName);
    Mark6DiskDevice();
    Mark6DiskDevice(const Mark6DiskDevice &device);
    virtual ~Mark6DiskDevice();
    void reset();
    void addPartition(std::string partitionName);
    std::vector<Mark6Partition> getPartitions() const;
    std::string getName() const;
    int mountDisk(std::string dataPath, std::string metaPath);
    int mountPartition(int partitionNumber, std::string mountPath);
    void unmountDisk();
    
    bool isMounted();
    void setFsType(std::string fsType_m);
    std::string getFsType() const;
    Mark6Meta &getMeta(); 
    void setDiskId(long diskId_m);
    long getDiskId() const;
    void setControllerId(int controllerId_m);
    int getControllerId() const;
    void setSerial(std::string serial_m);
    std::string getSerial() const;
    bool isValid();
    int getSlot() const;
    int getPosition() const;
    

private:

    std::string name_m;
    std::vector<Mark6Partition> partitions_m;
    bool isMounted_m;
    std::string fsType_m;
    long diskId_m;
    int controllerId_m;
    std::string serial_m;
    Mark6Meta meta_m;
    
};

#endif	/* MARK6DISKDEVICE_H */

