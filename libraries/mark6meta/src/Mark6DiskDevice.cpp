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
#include "Mark6DiskDevice.h"
#include "Mark6.h"
#include "Mark6Meta.h"
#include "Mark6Util.h"

#include <algorithm>
#include <errno.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <string.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>

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
 * Copy constructor
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
    sasAddress_m = device.sasAddress_m;
    capacityMB_m = device.capacityMB_m;
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
    capacityMB_m = 0;
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
 * Checks whether this is a valid Mark6 disk device.
 * @returns true if valid; false otherwise 
 */
bool Mark6DiskDevice::isValid()
{
    // cout << diskId_m << " " << controllerId_m << endl;
    if (diskId_m == -1)
        return(false);
    if (controllerId_m == -1)
        return(false);
    
    return(true);
}

/**
 * Determines the position of the disk device within the module. This number should range from 0 to 7.
 * @return the position of the disk with in the module
 * @return -1 in case of failure
 */
int Mark6DiskDevice::getPosition() const {
    
    if (diskId_m == -1)
        return(-1);
    
    if ((diskId_m >= 0) && (diskId_m <= 7))
        return(diskId_m);
    else if ((diskId_m >= 8) && (diskId_m <= 15))
        return(diskId_m - 8);

    return(-1);
    
}
/**
 * Determines the slot (bank) in which the device is located by inspecting the controllerId and the diskId. 
 * NOTE: numbering of the slots is off by 1 compared to the bank labels written on the Mark6; e.g. slot=0 refers to bank=1 and so on. 
 * @return the slot number of the disk device (0-3), -1 in case of error
 */
int Mark6DiskDevice::getSlot() const {
    
   //cout << "device: " << name_m << " diskid= " << diskId_m << " controllerid= " << controllerId_m << endl;
    if ((controllerId_m == -1) || (diskId_m == -1))
        return(-1);

    if (diskId_m <= 7)
        return(controllerId_m*2);
    else if ((diskId_m > 7) && (diskId_m <= 15))
        return(controllerId_m*2+1);
   
    return(-1);
}

void Mark6DiskDevice::createPartitions()
{
    string cmd = "";

    cmd = "parted -s -- /dev/" + name_m + " mktable gpt ";
    execCommand(cmd.c_str());
    cmd = "parted -s -- /dev/" + name_m + " mkpart primary xfs 1 -100M ";
    execCommand(cmd.c_str());
    cmd = "parted -s -- /dev/" + name_m + " mkpart primary xfs -100M 100%";
    execCommand(cmd.c_str());

//  partcmd = ['sudo', 'mkfs.xfs', '-f', disks[i]['disk'] + '1']
    cmd = "mkfs.xfs -f /dev/" + name_m + "1";
    execCommand(cmd.c_str());

    cmd = "mkfs.xfs -f /dev/" + name_m + "2";
    execCommand(cmd.c_str());


}

void Mark6DiskDevice::clearPartitions()
{
    string cmd = "";

    cmd = "parted -s /dev/" + name_m + " rm 1 ";
    execCommand(cmd.c_str()) ;
    cmd = "parted -s /dev/" + name_m + " rm 2 ";
    execCommand(cmd.c_str()) ;
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
     
     partitions_m.push_back(partition);
     
     // finally sort the partitions
     sort(partitions_m.begin(), partitions_m.end(), Mark6Partition::sortByName);
}




int Mark6DiskDevice::mountPartition(int partitionNumber, string mountPath)
{
    struct stat file;
    string source = "";
    //string dest = "";
    
    // verify that the partition number is valid
    if (partitionNumber > partitions_m.size())
        return(0);
    
    // check that mount point exists
    if (stat(mountPath.c_str(), &file) != 0)
    {
        throw Mark6MountException (string("Mount point " + mountPath + " does not exist." ));
    }
    
    
    source = "/dev/" + partitions_m[partitionNumber].deviceName;

    int ret = mount(source.c_str(), mountPath.c_str(), fsType_m.c_str(), MS_MGC_VAL | MS_RDONLY | MS_NOATIME, "");


    if (ret == -1)
    {
        char buffer[ 256 ];
        char * errorMsg = strerror_r( errno, buffer, 256 );
        partitions_m[partitionNumber].mountPath = "";
        isMounted_m = false;
        throw Mark6MountException (string("Cannot mount  partition " + source + " under " + mountPath + " Reason: " + errorMsg));
    }

    partitions_m[partitionNumber].mountPath = mountPath;
    
   
    // now read metadata
    //meta_m.parse(dest);    
        
    isMounted_m = true;
    //mountPath_m = dest;
    
    return(1);
}

/**
 * Mounts both partitions (data and metadata) of this disk device. The data partitions will be
 * mounted under the given dataPath; the metadata partition under the metaPath. In case both
 * partitions have been mounted successfully the meta data is read and isMounted() will return true.
 * @param[in] dataPath the path under which the data partition will be mounted
 * @param[in] metaPath the path under which the metadata partition will be mounted
 * @throws Mark6MountException in case the device cannot be mounted
 * @returns 1 if both partitions were mounted successfully, 0 otherwise
 */
int Mark6DiskDevice::mountDisk(string dataPath, string metaPath, bool readwrite)

{
    struct stat file;
    string source = "";
    
    //string dest = "";
    
    // verify that this disk has two partitions
    if (partitions_m.size() != 2)
        return(0);
       
    unsigned long  mountOpts =  MS_MGC_VAL | MS_RDONLY | MS_NOATIME;

    if (readwrite)
        mountOpts = MS_MGC_VAL | MS_NOATIME;


    // mount both partitions
    for (int i=0; i<2; i++)
    {
        source = "/dev/" + partitions_m[i].deviceName;
        
        stringstream  dest;
        if (i == 0)
            dest << dataPath << getSlot()+1 << "/" << getPosition();
        else if (i==1)
            dest << metaPath << getSlot()+1 << "/" <<  getPosition();
        
        // cout << "mounting: " << source << " on "  << dest.str() << " controller: " << controllerId_m << " sasadress: " << sasAddress_m << endl;
        
        if (stat(dest.str().c_str(), &file) != 0)
        {
            throw Mark6MountException (string("Mount point " + dest.str() + " does not exist." ));
        }
        
        int ret = mount(source.c_str(), dest.str().c_str(), fsType_m.c_str(), mountOpts, "");
    
        if (ret == -1)
        {
            char buffer[ 256 ];
            char *errorMsg = strerror_r( errno, buffer, 256 );

            isMounted_m = false;
            throw Mark6MountException (string("Cannot mount  device " + source + " at " + dest.str() + ".  Reason: " + errorMsg));
        }
        
        partitions_m[i].mountPath = dest.str();
    }
   
    isMounted_m = true;
    //mountPath_m = dest;
    
    
    return(1);
}

void Mark6DiskDevice::makeDataDir(string rootPath) {

    stringstream ss;
    string path;
    
    ss << rootPath << "/" <<  getSlot()+1 << "/" << getPosition() << "/data";
    ss >> path;
    cout << "Creating: " << path << endl;
    mkdir (path.c_str(), S_IRUSR| S_IWUSR | S_IRGRP | S_IWGRP |S_IROTH | S_IWOTH);
    chmod(path.c_str(), S_IRWXU| S_IRWXG | S_IRWXO);
}

  

/**
 * Writes the meta information (eMSN, serials, state) to the .meta partition of the disk device
 **/
void Mark6DiskDevice::writeMeta(Mark6Meta &meta, string rootMetaPath)
{

    ofstream file;


    file.open ((partitions_m[1].mountPath + "/eMSN").c_str());
    cout << "HR write Meta :" << meta.getEMSN() << endl;
    file << meta.getEMSN();
    file.close();
    chmod((partitions_m[1].mountPath + "/eMSN").c_str(), S_IRUSR| S_IWUSR | S_IRGRP | S_IWGRP |S_IROTH | S_IWOTH);

    file.open ((partitions_m[1].mountPath + "/disk_sn").c_str());
    map<int,string> serials = meta.getSerials();
    for (map<int, string>::iterator it=serials.begin(); it!=serials.end(); ++it)
        file << it->first << ":" << it->second << '\n';
    file.close();
    chmod((partitions_m[1].mountPath + "/disk_sn").c_str(), S_IRUSR| S_IWUSR | S_IRGRP | S_IWGRP |S_IROTH | S_IWOTH);

    file.open ((partitions_m[1].mountPath + "/state").c_str());
    file << "erased";
    file.close();
    chmod((partitions_m[1].mountPath + "/state").c_str(), S_IRUSR| S_IWUSR | S_IRGRP | S_IWGRP |S_IROTH | S_IWOTH);


    // attach new meta information to device
    meta_m = meta;
}

void Mark6DiskDevice::parseMeta()
{
    meta_m.parse(partitions_m[1].mountPath);
}

void Mark6DiskDevice::unmountDisk()
{
    for (int i=0; i<2; i++)
    {
        
        string dest =  partitions_m[i].mountPath;
        
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

void Mark6DiskDevice::setSasAddress(std::string sasAddress) {
    this->sasAddress_m = sasAddress;
}

std::string Mark6DiskDevice::getSasAddress() const {
    return sasAddress_m;
}

/*std::string Mark6DiskDevice::getMountPath() const {
    return mountPath_m;
}*/

/**
 * Destructor
 */
Mark6DiskDevice::~Mark6DiskDevice() {
}

