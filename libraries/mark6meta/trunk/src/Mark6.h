/* 
 * File:   Mark6Meta.cpp
 * Author: Helge Rottmann (MPIfR)
 * 
 * Created on 30. September 2015, 12:23
 */

#ifndef MARK6_H
#define	MARK6_H

#include <string>
#include <vector>

#include "Mark6DiskDevice.h"
#include "Mark6Module.h"

/**
 * Custom exception class for reporting Mark6 related errors
 */
class Mark6Exception : public std::exception 
{
private:
    std::string err_msg;
    
public:
    Mark6Exception(std::string msg) : err_msg(msg) {};
    ~Mark6Exception() throw() {};
    const char *what() const throw() { return this->err_msg.c_str(); };
};

/**
 * Custom exception class for reporting Mark6 related mount errors
 */
class Mark6MountException : public std::exception 
{
private:
    std::string err_msg;

public:
    Mark6MountException(std::string msg) : err_msg(msg) {};
    ~Mark6MountException() throw() {};
    const char *what() const throw() { return this->err_msg.c_str(); };
};


class Mark6
{
private:
	static const int NUMSLOTS = 4;                                      // number of slots per mark6 unit
       
        
	int fd;		// file descriptor for the udev monitor
        struct udev *udev_m;
        struct udev_monitor *mon;
	std::vector<Mark6DiskDevice> mountedDevices_m;
	std::vector<Mark6DiskDevice> removedDevices_m;
        std::vector<Mark6DiskDevice> newDevices_m;
        std::string mountPath_m;
        Mark6Module modules_m[NUMSLOTS];
        std::string linkRootData_m;                 // default path for creating symbolic links to the mount points of the data partitions
        std::string linkRootMeta_m;                 // default path for creating symbolic links to the mount points of the meta partitions
        std::string slotIds_m[NUMSLOTS];
        
        void manageDeviceChange();
	void validateMountDevices();
        int parseControllerId(std::string devpath);
        long parseDiskId(std::string sasAddress);
public:
        Mark6();
	~Mark6();
	void pollDevices();
	void validateMountPoints();
	void createMountPoint (std::string path);
        int addPartitionDevice(std::string partitionName);
        bool isMounted(std::string deviceName);
        Mark6DiskDevice *getMountedDevice(std::string deviceName);
        void removeMountedDevice(Mark6DiskDevice device);
        int enumerateDevices();
        void cleanUp();
        void createMountLinks();
        int getSlot(std::string eMSN);
        int getFreeSlot();
        void sendStatusMessage();
        std::vector<Mark6DiskDevice> getMountedDevices() const {
            return mountedDevices_m;
        }
        
};

#endif	/* MARK6_H */
