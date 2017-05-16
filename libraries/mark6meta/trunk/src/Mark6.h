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
        Mark6Module modules_m[NUMSLOTS];
        std::string linkRootData_m;                 // default path for creating symbolic links to the mount points of the data partitions
        std::string linkRootMeta_m;                 // default path for creating symbolic links to the mount points of the meta partitions
        std::string mountRootData_m;        //                
        std::string mountRootMeta_m;    
        std::string slotIds_m[NUMSLOTS];
        std::string diskIds_m[Mark6Module::MAXDISKS];
        
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
        int getSlot(std::string eMSN);
        void sendStatusMessage();
        std::vector<Mark6DiskDevice> getMountedDevices() const {
            return mountedDevices_m;
        }
        
};

#endif	/* MARK6_H */
