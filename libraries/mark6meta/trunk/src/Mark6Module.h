/* 
 * File:   Mark6Module.h
 * Author: Helge Rottmann (MPIfR)
 *
 * Created on 20. Oktober 2015, 15:08
 */

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

