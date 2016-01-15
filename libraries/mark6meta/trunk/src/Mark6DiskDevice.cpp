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
#include "Mark6DiskDevice.h"
#include "Mark6.h"
#include "Mark6Meta.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <sys/mount.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>

using namespace std;

/**
 * Constructor
 * @param[in] deviceName the device name of the disk
 */
Mark6DiskDevice::Mark6DiskDevice(string deviceName) {
    name_m = deviceName;
    
    reset();   
}

Mark6DiskDevice::Mark6DiskDevice() {
    
    reset(); 
}

/**
 * Copy construcor
 * @param device
 */
Mark6DiskDevice::Mark6DiskDevice(const Mark6DiskDevice &device)
{
    name_m = device.name_m;
    partitions_m = device.partitions_m;
    isMounted_m = device.isMounted_m;
    fsType_m = device.fsType_m;
    diskId_m = device.diskId_m;
    controllerId_m = device.controllerId_m;
    serial_m = device.serial_m;
    meta_m = device.meta_m;
}

/**
 * Resets the disk device to the initial state
 */
void Mark6DiskDevice::reset()
{
    meta_m = Mark6Meta();
    isMounted_m = false;
    fsType_m = "xfs";
    controllerId_m = -1;
    diskId_m = -1;
    serial_m = "";
}

vector<Mark6DiskDevice::Mark6Partition> Mark6DiskDevice::getPartitions() const {
    return partitions_m;
}

/**
 * @return the device name of the disk
 */
std::string Mark6DiskDevice::getName() const {
    return name_m;
}

/**
 * Checks whether this is a vaid Mark6 disk device.
 * @returns true if valid; false otherwise 
 */
bool Mark6DiskDevice::isValid()
{
    
    if (diskId_m == -1)
        return(false);
    if (controllerId_m == -1)
        return(false);
    
    return(true);
}

/**
 * Determines the position of the disk device within the module. This number should range from 0 to 7.
 * @return the position of the disk with in the module
 */
int Mark6DiskDevice::getPosition() const {
    
    if (diskId_m == -1)
        return(-1);
    
    if ((diskId_m >= 0) && (diskId_m <= 7))
        return(diskId_m);
    else if ((diskId_m >= 8) && (diskId_m <= 15))
        return(diskId_m - 8);
    else
        return(-1);
    
}
/**
 * Determines the slot (bank) in which the device is located by inspecting the controllerId and the diskId. 
 * NOTE: numbering of the slots is off by 1 compared to the bank labels written on the Mark6; e.g. slot=0 refers to bank=1 and so on. 
 * @return the slot number of the disk device (0-3)
 */
int Mark6DiskDevice::getSlot() const {
    
    //cout << "device: " << name_m << " diskid= " << diskId_m << " controllerid= " << controllerId_m << endl;
    if ((controllerId_m == -1) || (diskId_m == -1))
        return(-1);
   
    if (controllerId_m == 0)
    {
        if (diskId_m <= 7)
            return(2);
        else if ((diskId_m > 7) && (diskId_m <= 15))
            return(3);
    }
    else if (controllerId_m == 1)
    {
        if (diskId_m <= 7)
            return(0);
        else if ((diskId_m > 7) && (diskId_m <= 15))
            return(1);
    }
    else
        return(-1);
}



/**
 * Checks whether the partition with the given name already exists. If not it will be added
 * @param[in] partitionName the name of the partition to add
 */
void Mark6DiskDevice::addPartition(std::string partitionName)
{
     for(vector<string>::size_type i = 0; i != partitions_m.size(); i++) {
         // check if partition already exists
         if (partitions_m[i].deviceName == partitionName)
             return;
     }
     
     Mark6Partition partition;
     partition.deviceName = partitionName;
     partition.mountPath = "";
     partition.linkPath = "";
     
     partitions_m.push_back(partition);
     
     // finally sort the partitions
     sort(partitions_m.begin(), partitions_m.end(), Mark6Partition::sortByName);
}

/**
 * Removes the symbolic links to the mount locations of the disk device. 
 * * The following logic is applied:
 * - the first partition is assumed to contain the mark6 data. It is linked under linkPath/slot/disk
 * - the second partition is assumed to contain the meta data. It is linked under linkPath/.meta/slot/disk
 * where slot runs from 1-4, and disk runs from 0-7
 * @return EXIT_SUCCESS in case of success, EXIT_FAILURE otherwise
 */
int Mark6DiskDevice::unlinkDisk()
{     
    int errorCount = 0;
    
    // loop over partitions
    for (unsigned int i=0; i < partitions_m.size(); i++)
    {
        
        struct stat file;
        string linkPath = partitions_m[i].linkPath;
        
        //cout << "trying to unlink partition " << i << " on disk " << name_m << " " << linkPath << endl;
        
        // check if partition is linked
        if (linkPath == "")
            continue;
        
        // check that link exists
        if (stat(linkPath.c_str(), &file) != 0)    
        {
            errorCount++;
            continue;
        }
        // check if this is really a symbolic link    
        lstat(linkPath.c_str(), &file);
        if (!S_ISLNK(file.st_mode))
        {
            errorCount++;
            continue;
        }
        
        if( remove( linkPath.c_str() ) != 0 )
        {
            throw Mark6Exception("Cannot remove symbolic link: " + linkPath);
        }
        //cout << " removed symbolic link " << linkPath << endl;
        partitions_m[i].linkPath = "";     
    }
    
    if (errorCount == 0)
        return(EXIT_SUCCESS);
    else
        return(EXIT_FAILURE);
}

/**
 * Creates symbolic links for all partitions on the disk device. The symbolic link is created under the given linkPath
 * and points to the mount point of the associated partition device. 
 * The following logic is applied:
 * - the first partition is assumed to contain the mark6 data. It is linked under linkPath/slot/disk
 * - the second partition is assumed to contain the meta data. It is linked under linkPath/.meta/slot/disk
 * where slot runs from 1-4, and disk runs from 0-7
 * @param[in] linkRootData the full path of the root directory under which the symbolic links for the data partitions are to be created
 * @param[in] linkRootMeta the full path of the root directory under which the symbolic links for the meta partitions are to be created
 * @param[in] slot the number of the module slot 
 * @return EXIT_SUCCESS in case of success, EXIT_FAILURE otherwise
 */
int Mark6DiskDevice::linkDisk(std::string linkRootData, std::string linkRootMeta, int slot)
{       
    
    
    // loop over partitions
    for (unsigned int i=0; i < partitions_m.size(); i++)
    {
        // check if partition is linked already
        if (partitions_m[i].linkPath != "")
            continue;
        // check if partition has been mounted
        if (partitions_m[i].mountPath == "")
            return(EXIT_FAILURE);
        // check if diskId is set
        if (diskId_m == -1)
            return(EXIT_FAILURE);
        
        // build link path
        stringstream ss;
        if (i == 0)
            ss << linkRootData << "/" << slot+1 << "/" <<  getPosition();
        else if (i ==1)
            ss << linkRootMeta << "/" << slot+1 << "/" <<  getPosition();
        
        string linkPath = ss.str();
        //cout << " creating symbolic link " <<  partitions_m[i].mountPath << " to " << linkPath << endl;
        
        if (symlink(partitions_m[i].mountPath.c_str(), linkPath.c_str()) != 0)
        {
            throw  Mark6Exception("Cannot create symbolic link: " + partitions_m[i].mountPath + " -> " +  linkPath);
        }
        
        partitions_m[i].linkPath = linkPath;
        
    }
   
    return(EXIT_SUCCESS);
}


/**
 * Mounts both partitions of this disk device. The partitions will be mounted under the given mount path plus the device name.
 * e.g. partition /dev/sdb1 will mounted under /mnt/mark6/mnt/sdb1 
 * @param[in] mountPath the path uder which the partitions will be mounted
 * @throws Mark6MountException in case the device cannot be mounted
 * @returns 1 if both partitions were mounted successfully, 0 otherwise
 */
int Mark6DiskDevice::mountDisk(string mountPath)
{
    string source = "";
    string dest = "";
    
    // verify that this disk has two partitions
    if (partitions_m.size() != 2)
        return(0);
    
    // mount both partitions
    for (int i=0; i<2; i++)
    {
        source = "/dev/" + partitions_m[i].deviceName;
        dest =  mountPath + partitions_m[i].deviceName;
        
        int ret = mount(source.c_str(), dest.c_str(), fsType_m.c_str(), MS_MGC_VAL | MS_RDONLY , "");
    
        if (ret == -1)
        {
            isMounted_m = false;
            throw Mark6MountException (string("Cannot mount  device " + source + " under " + dest));
        }
        
        partitions_m[i].mountPath = dest;
    }
   
    // now read metadata
    meta_m.parse(dest);    
        
    isMounted_m = true;
    //mountPath_m = dest;
    
    return(1);
}

void Mark6DiskDevice::unmountDisk(string mountPath)
{
    for (int i=0; i<2; i++)
    {
        string dest =  mountPath + partitions_m[i].deviceName;
        
        int ret = umount2(dest.c_str(), MNT_FORCE);
    
        if (ret == -1)
        {
            throw Mark6MountException (string("Cannot unmount  device " + dest ));
        }
    }
    
    isMounted_m = false;
}

/**
 *
 * @return true if this disk device including all contained partitions is mounted; false otherwise
 */
bool Mark6DiskDevice::isMounted()
{
    return(isMounted_m);
}

void Mark6DiskDevice::setFsType(std::string fsType_m) {
    this->fsType_m = fsType_m;
}

std::string Mark6DiskDevice::getFsType() const {
    return fsType_m;
}

Mark6Meta &Mark6DiskDevice::getMeta() {
    return meta_m;
}

void Mark6DiskDevice::setDiskId(long diskId_m) {
    this->diskId_m = diskId_m;
}

/**
 * Gets the id (= position) of the disk on the SAS controller. This number will range from 0 to 15 as there
 * are two modules per controller. If you want to get the position of the device within a module use 
 * getPosition() instead.
 * @return the disk id 
 */
long Mark6DiskDevice::getDiskId() const {
    return diskId_m;
}

void Mark6DiskDevice::setControllerId(int controllerId_m) {
    this->controllerId_m = controllerId_m;
}

int Mark6DiskDevice::getControllerId() const {
    return controllerId_m;
}

void Mark6DiskDevice::setSerial(std::string serial_m) {
    this->serial_m = serial_m;
}

std::string Mark6DiskDevice::getSerial() const {
    return serial_m;
}

/*std::string Mark6DiskDevice::getMountPath() const {
    return mountPath_m;
}*/

/**
 * Destructor
 */
Mark6DiskDevice::~Mark6DiskDevice() {
}

