/* 
 * File:   Mark5DiskDevice.h
 * Author: oper
 *
 * Created on 28. September 2015, 14:33
 */

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
        std::string linkPath;
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
    int mountDisk(std::string mountPath);
    void unmountDisk(std::string mountPath);
    int linkDisk(std::string linkRootData, std::string linkRootMeta, int slot);
    int unlinkDisk();
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

