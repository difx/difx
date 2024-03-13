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
#ifndef MARK6_H
#define	MARK6_H

#include <string>
#include <vector>
#include <poll.h>
#include <libudev.h>


#include "Mark6DiskDevice.h"
#include "Mark6Module.h"
#include "Mark6Controller.h"

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
//	int fd;		// file descriptor for the udev monitor
        pollfd pollItems_m[1];
        struct udev *udev_m;
        struct udev_monitor *mon;
        bool verifyDevices_m;
        bool initRawDevice_m;
	std::vector<Mark6DiskDevice> mountedDevices_m;
	std::vector<Mark6DiskDevice> removedDevices_m;
        std::vector<Mark6DiskDevice> newDevices_m;
        std::vector<Mark6DiskDevice> rawDevices_m;
        std::vector<std::string> newPartitions_m;
        std::vector<Mark6Controller> controllers_m;
        std::vector<Mark6Module> modules_m;
        std::vector<std::string>  slotIds_m;
        std::string linkRootData_m;                 // default path for creating symbolic links to the mount points of the data partitions
        std::string linkRootMeta_m;                 // default path for creating symbolic links to the mount points of the meta partitions
        std::string mountRootData_m;        //                
        std::string mountRootMeta_m;    
        std::string diskIds_m[Mark6Module::MAXDISKS];
        int numSlots_m;                 // the number of slots present in the mark6 system (2 per controller)
        int readControllerConfig();
        void writeControllerConfig();
        Mark6DiskDevice newDeviceFromUdev(udev_device *dev);
        void manageDeviceChange();
        int verifyDevices();
	void validateMountDevices();
        int parseControllerId(std::string devpath);
        long parseDiskId(std::string sasAddress, std::string driver);
        long parsePhyId(std::string sasPath);
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
        int enumerateControllers();
        int enumerateDevices();
        void cleanUp();
        int getSlot(std::string eMSN);
        void sendStatusMessage();
        void sendSlotStatusMessage();
        void sendActivityMessage(std::string vsn, int slot, std::string msg);
        void modInit(int slot, std::string eMSN);
        Mark6Controller *getControllerById(int id);
        std::vector<Mark6DiskDevice> getMountedDevices() const {
            return mountedDevices_m;
        }
        
};

#endif	/* MARK6_H */
