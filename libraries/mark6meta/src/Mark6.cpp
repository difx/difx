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
#include <poll.h>
#include <libudev.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>
#include <string>
#include <vector>
#include <stdexcept>
#include <dirent.h>
#include <sys/stat.h>
#include <errno.h>
#include <difxmessage.h>
#include <set>
#include <unistd.h>
#include <ctype.h>

#include "Mark6.h"
#include "Mark6DiskDevice.h"
#include "Mark6Util.h"
#include "Mark6Meta.h"
#include "Mark6Controller.h"
#include <sys/mount.h>

using namespace std;

/**
 * Constructor
 */
Mark6::Mark6(void)
{
    //set defaults 
    mountRootData_m = "/mnt/disks/";               
    mountRootMeta_m =  mountRootData_m + ".meta/";
    verifyDevices_m = false;

    // treat uninitialized disks (needed for modInit mode)
    initRawDevice_m = false;
    
    // Create the udev object 
    udev_m = udev_new();
    if (!udev_m) {
            throw Mark6Exception (string("Cannot create new udev object.") );
    }
    mon = udev_monitor_new_from_netlink(udev_m, "udev");
    udev_monitor_filter_add_match_subsystem_devtype(mon, "block", NULL);
    udev_monitor_enable_receiving(mon);

    // create the poll item
    pollItems_m[0].fd = udev_monitor_get_fd(mon);
    pollItems_m[0].events = POLLIN;
    pollItems_m[0].revents = 0;

    // identify SAS controllers
    int numControllerDetect = enumerateControllers();

    // look for controller config in /etc/default
    int numControllerConf = readControllerConfig();
    if (numControllerConf == -1)
    {
        // write the config
        writeControllerConfig();
        numControllerConf = readControllerConfig();
    }

    if (numControllerDetect == 0)
        throw Mark6Exception (string("Did not find any SAS controllers. Is this a mark6?") );
    else if (numControllerDetect < numControllerConf) 
        throw Mark6Exception (string("Detected fewer SAS controllers than defined in the configuration.") );

    numSlots_m = numControllerDetect * 2;
    

    // create array holding slot ids
    for (int i=0; i < numSlots_m; i++)
    {
        stringstream ss;
        ss << i+1;
        slotIds_m.push_back( ss.str() );
    }
    // create array holding disk ids
    for (int i=0; i < Mark6Module::MAXDISKS; i++)
    {
        stringstream ss;
        ss << i;
        diskIds_m[i] = ss.str();
    }

    for (int i=0; i < numSlots_m; i++)
        {
            modules_m.push_back(Mark6Module());
        }
    //
    // remove any remaining mounts
    cleanUp();

    
    // check if required mount points exists; create them if not        
    validateMountPoints();


    if (enumerateDevices() > 0)
    {
	manageDeviceChange();
    } 
}

/**
 * Destructor
 */
Mark6::~Mark6()
{
	// destroy the udev monitor
	udev_monitor_unref(mon);
	//
	//destroy the udev object
	udev_unref(udev_m);
}

/**
 * Check if a directory exists at the given location. If not it will be created.
 * @param[in] path the full path of he directory to be created
 */
void  Mark6::createMountPoint(string path)
{
	int ret;
        struct stat sb;

        if (stat(path.c_str(), &sb))
	{
        	ret = mkdir(path.c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
		if (ret == 0)
		{
			//cout << "created mount point: " << path << endl;
			return;
		}
		else if (ret == EACCES)
		{
			throw Mark6Exception (string("no permission to create mount point : " + path) );
		} 
		else
		{
			throw Mark6Exception (string("error creating mount point: " + path ) );
		}			
	}
}

/**
 * Finds the slot associated with module having the given eMSN.
 * NOTE: the slot determined by this function does not necessarily correspond to the 
 * physical slot in the mark6 machine
 * @param[in] the module eMSN
 * @returns the number of the slot associated with the given eMSN; -1 in case no slot with a matching eMSN can be found
 */
int Mark6::getSlot(string eMSN)
{
    
     for (int iSlot = 0; iSlot < numSlots_m; iSlot++) 
        {
            if (modules_m[iSlot].getEMSN() == eMSN)
            {
                return(iSlot);
            }   
        }
     
     return(-1);
}


void Mark6::modInit (int slot, string vsn)
{
    Mark6DiskDevice *device;
    map<int,string> serials;
    stringstream ss;
    char message[64]; 

    cout << "Starting modinit" << endl;
    cout << "Number of slots: " << numSlots_m << endl;

    if ((slot < 0) || (slot > numSlots_m))
        throw  Mark6Exception("Invalid slot number");

    initRawDevice_m = true;
    manageDeviceChange();

    cout << "Number of disks in slot " << slot << ": " << modules_m[slot].getNumDiskDevices() << endl;

    if  (modules_m[slot].getNumDiskDevices() < 1)
        return;

    

    // meta data object
    Mark6Meta meta = Mark6Meta();
    meta.setEMSN(vsn, modules_m[slot].getDiskDevice(0)->capacityMB_m *  modules_m[slot].getNumDiskDevices(), 4096, modules_m[slot].getNumDiskDevices());

    cout << " Disk capacity: " << modules_m[slot].getDiskDevice(0)->capacityMB_m << endl;
    cout << " Module meta: " << modules_m[slot].getCapacity() << " " << modules_m[slot].getVSN() << " " << modules_m[slot].getDatarate() << endl;

    
    // determine disk serials
    for (int diskIdx = 0; diskIdx <= modules_m[slot].getNumDiskDevices(); diskIdx++)
    {   
        device = modules_m[slot].getDiskDevice(diskIdx);
        if (device != NULL)
        {   
            serials.insert(pair<int,string>(diskIdx, device->getSerial()));
        }
    }
    meta.setSerials(serials);

    std::cout << "serials contains:\n";
    map<int, string>::iterator it = serials.begin();
    for (it=serials.begin(); it!=serials.end(); ++it)
        std::cout << it->first << " => " << it->second << '\n';

    
  
    for (int diskIdx = 0; diskIdx <= modules_m[slot].getNumDiskDevices(); diskIdx++)
    {
        device = modules_m[slot].getDiskDevice(diskIdx);
        if (device != NULL)
        {
            snprintf (message, 64, "modinit disk %d of %d", diskIdx+1, Mark6Module::MAXDISKS);
            sendActivityMessage(vsn, slot+1, string(message));
            
            //cout << "HR disk: " << diskIdx << " name: " << device->getName() << " serial: " << device->getSerial() << endl;
            //
            if (device->isMounted())
                //unmount device
                device->unmountDisk();

            //remove existing partitions
            device->clearPartitions();
               
            // create new xfs-formated partitions
            device->createPartitions();
              
            //mount device read-write
            device->mountDisk(mountRootData_m, mountRootMeta_m, true);

            // create meta information
            device->writeMeta(meta, mountRootMeta_m);
            device->makeDataDir(mountRootData_m);

            // unmount device 
            device->unmountDisk();
    
            //remount read-only
            device->mountDisk(mountRootData_m, mountRootMeta_m, false);
        }
    }
    
    // update module meta information 
    vector<string> empty;
    modules_m[slot].setGroupMembers(empty);
    modules_m[slot].updateMetaFromEMSN(meta.getEMSN());

    initRawDevice_m = false;
    manageDeviceChange();
    sendActivityMessage(vsn, slot+1, "done");
    
}


void Mark6::sendActivityMessage(string vsn, int slot, string message)
{
    DifxMessageMark6Activity dm;

/*    typedef struct
{
        enum Mark6State state;
        char activeVsn[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
        unsigned int status;
        int scanNumber;
        char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
        long long position;     
        float rate;             
        double dataMJD;
} DifxMessageMark6Activity;
*/
    dm.state = MARK6_STATE_INITIALIZING;
    strncpy(dm.activeVsn, vsn.c_str() , DIFX_MESSAGE_MARK6_MSN_LENGTH);
    strncpy(dm.scanName, message.c_str() , DIFX_MESSAGE_MAX_SCANNAME_LEN);

    dm.scanNumber = 0;
    dm.position = slot;
    dm.rate = 0.0;
    dm.dataMJD = 0.0;

    cout << "activity: " << vsn << " " << dm.activeVsn << " " << vsn.c_str() <<  " " << strlen(dm.activeVsn) << endl;
    difxMessageSendMark6Activity(&dm);

}

/**
 * Sends out a Mark6SlotStatusMessage
 */
void Mark6::sendSlotStatusMessage()
{
    //clog << "Mark6::sendStatusMessage" << endl;
    DifxMessageMark6SlotStatus dm;
    
    memset(&dm, 0, sizeof(DifxMessageMark6SlotStatus));

    
    // set module group for each bank
    for (int slot=0; slot < numSlots_m; slot++)
    {
        // slot
        dm.slot = slot+1;

        // get MSN
//        string msn = "";
 //       msn = modules_m[slot].getVSN().c_str();

        strncpy(dm.msn,  modules_m[slot].getVSN().c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
                
        // construct group string
        string group = "";
        for (int i=0; i < modules_m[slot].getGroupMembers().size(); i++)
        {
            string groupMsn =  modules_m[slot].getGroupMembers()[i].substr(0,DIFX_MESSAGE_MARK6_MSN_LENGTH);;
            group += groupMsn;
            if (i+1 < modules_m[slot].getGroupMembers().size())
                group += ":";
        }
        strncpy(dm.group, group.c_str(), DIFX_MESSAGE_MARK6_GROUP_LENGTH);
        // number of disks
        dm.numDisks = modules_m[slot].getNumDiskDevices();
        // number of missing disks
        dm.numMissingDisks = modules_m[slot].getNumTargetDisks() - modules_m[slot].getNumDiskDevices();

        difxMessageSendMark6SlotStatus(&dm);
    }


}

/**
 * Sends out a Mark6StatusMessage
 */
void Mark6::sendStatusMessage()
{
    DifxMessageMark6Status dm;
    
    memset(&dm, 0, sizeof(DifxMessageMark6Status));
    
    // set bank MSNs  
    strncpy(dm.msn1, modules_m[0].getEMSN().c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
    strncpy(dm.msn2, modules_m[1].getEMSN().c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
    strncpy(dm.msn3, modules_m[2].getEMSN().c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
    strncpy(dm.msn4, modules_m[3].getEMSN().c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);

    // set module group for each bank
    for (int slot=0; slot < numSlots_m; slot++)
    {
        // get MSN
        string msn = "";
        msn = modules_m[slot].getEMSN().substr(0,DIFX_MESSAGE_MARK6_MSN_LENGTH);
                
        // construct group string
        string group = "";
        for (int i=0; i < modules_m[slot].getGroupMembers().size(); i++)
        {
            string groupMsn =  modules_m[slot].getGroupMembers()[i].substr(0,DIFX_MESSAGE_MARK6_MSN_LENGTH);;
            group += groupMsn;
            if (i+1 < modules_m[slot].getGroupMembers().size())
                group += ":";
        }
        //cout << "Group " << slot << " " << group << endl;
        
        int missing = modules_m[slot].getNumTargetDisks() - modules_m[slot].getNumDiskDevices();
        
       
        if (slot == 0)
        {
            strncpy(dm.msn1, msn.c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
            strncpy(dm.group1, group.c_str(), DIFX_MESSAGE_MARK6_GROUP_LENGTH);
            dm.bank1Disks = modules_m[slot].getNumDiskDevices();
            dm.bank1MissingDisks = missing;           
        }
        else if (slot ==1)
        {
            strncpy(dm.msn2, msn.c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
            strncpy(dm.group2, group.c_str(), DIFX_MESSAGE_MARK6_GROUP_LENGTH);
            dm.bank2Disks = modules_m[slot].getNumDiskDevices();
            dm.bank2MissingDisks = missing;
        }
        else if (slot ==2)
        {
            strncpy(dm.msn3, msn.c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
            strncpy(dm.group3, group.c_str(), DIFX_MESSAGE_MARK6_GROUP_LENGTH);
            dm.bank3Disks = modules_m[slot].getNumDiskDevices();
            dm.bank3MissingDisks = missing;
        }
        else if (slot ==3)
        {
            strncpy(dm.msn4, msn.c_str(), DIFX_MESSAGE_MARK6_MSN_LENGTH);
            strncpy(dm.group4, group.c_str(), DIFX_MESSAGE_MARK6_GROUP_LENGTH);
            dm.bank4Disks = modules_m[slot].getNumDiskDevices();
            dm.bank4MissingDisks = missing;
        }       
    }
       
    difxMessageSendMark6Status(&dm);
}



/**
 * Manages devices that were added or removed by turning the Mark6 keys. Newly added devices will be mounted; removed devices will be unmounted. 
 * Prior to mounting validation is done to check if the new device is a Mark6 disk
 */
void Mark6::manageDeviceChange()
{
    vector<Mark6DiskDevice> tempDevices;
    vector<Mark6DiskDevice> tempRemoveDevices;
    Mark6DiskDevice *disk;
    
    cout << "HR new devices: " << newDevices_m.size() << endl;
    // new devices have been found
    if (newDevices_m.size() > 0)
    {
         tempDevices.swap(newDevices_m);
         
        // validate all new devices
        for(std::vector<Mark6DiskDevice>::size_type i = 0; i != tempDevices.size(); i++) {
       
            cout << "Processing device: " << i << endl;
            if (tempDevices[i].isValid() == false)
            {
                cout << "disk " << tempDevices[i].getName() << " is not a valid Mark6 disk device. Discarding." << endl;
                clog << "disk " << tempDevices[i].getName() << " is not a valid Mark6 disk device. Discarding." << endl;
                continue;
            }

            // verify that both partitions have been detected for this device
	    if ((tempDevices[i].getPartitions().size() < 2)  && (!initRawDevice_m)){
		newDevices_m.push_back(tempDevices[i]);
                cout << "HR partition error" << endl;
		continue;
	    }
            else {
                // work around due to partitions not getting detected reliably by udev in case of many simulataneous events
            }
            
            // mount both partitions
            cout << "Mounting device" << endl;
            tempDevices[i].mountDisk(mountRootData_m, mountRootMeta_m, false);

            // read the meta information
            try {
                tempDevices[i].parseMeta();
            } catch (exception& e) {
                // log the exception and go on (needed when dealing with unitialized modules)
                clog << e.what() << endl;
            }

            // determine size
            //  TO DO: possibly shift size determination to Mark6DiskDevice
            string res = execCommand(("cat /sys/class/block/" + tempDevices[i].getName()  + "/size").c_str());
            stringstream ss ;
            ss << res;
            long temp;
            ss >> temp;
            tempDevices[i].capacityMB_m = temp * 512 / 1000000000;
            
                             
            if (tempDevices[i].isMounted())
            {
                //cout << "mounted " << tempDevices[i].getPartitions()[0].deviceName << " " << tempDevices[i].getPartitions()[1].deviceName << endl;           
                //cout << "TEST: " << i << " " << tempDevices[i].getPosition() << " " << tempDevices[i].getSerial() << " " << endl;

                mountedDevices_m.push_back(tempDevices[i]);

                int slot = tempDevices[i].getSlot();
                //cout << "Slot = " << slot << endl;
                
                if(slot == -1)
                {
                    //cout << "Cannot determine bank for disk " << tempDevices[i].getName() << ". Discarding.";
                    continue;
                }
		//cout << " EMSN: " << tempDevices[i].getMeta().getEMSN() << " " << modules_m[slot].getEMSN() << endl;

                //int pos = tempDevices[i].getPosition();
                modules_m[slot].addDiskDevice(tempDevices[i]);
                
                if (modules_m[slot].getEMSN() == "")
                {
                    modules_m[slot].updateMetaFromEMSN(tempDevices[i].getMeta().getEMSN());
                    cout << tempDevices[i].getMeta().getEMSN() << " " << modules_m[slot].getVSN() << endl;

                    if (tempDevices[i].getMeta().getGroup().size() != 0)
                    {
                        modules_m[slot].setGroupMembers(tempDevices[i].getMeta().getGroup());
                    }
                    else
                    {
                        // Bug(?) from VLBA Mark6:
                        //  - only one disk has a 'group' file on its .meta partition, vs. normally all disks
                        //  - grouping unclear, default to single group identical to disk eMSN metadata
                        std::string defaultGroup(tempDevices[i].getMeta().getEMSN());
                        defaultGroup.resize(DIFX_MESSAGE_MARK6_MSN_LENGTH);
                        clog << "Warning: metadata partition of disk " << tempDevices[i].getName() << " lacks 'group', defaulting group to " << defaultGroup << endl;
                        std::vector<std::string> tempGroup;
                        tempGroup.push_back(defaultGroup);
                        modules_m[slot].setGroupMembers(tempGroup);
                    }
                }
            }
            else
            {
                //cout << "mount failed " << tempDevices[i].getPartitions()[0].deviceName << " " << tempDevices[i].getPartitions()[1].deviceName << " will try again" << endl;
                if (!initRawDevice_m)
                    newDevices_m.push_back(tempDevices[i]);
                else
                {
                     int slot = tempDevices[i].getSlot();
                     modules_m[slot].addDiskDevice(tempDevices[i]);
                }
            }
        }  
    }
    
    //removed devices
    if (removedDevices_m.size() > 0)
    {
        tempRemoveDevices.swap(removedDevices_m);
        
        // loop over all removed devices
        for(std::vector<Mark6DiskDevice>::size_type i = 0; i != tempRemoveDevices.size(); i++) {
                       
	    //cout << "Removing device: " << tempRemoveDevices[i].getName() << endl;
            if ((disk = getMountedDevice(tempRemoveDevices[i].getName())) != NULL)
            {
                // get the module slot           
                int slot = disk->getSlot();
                
                // unmount the disk
                disk->unmountDisk();
                                
                if (disk->isMounted() == false)
                {
                    //cout << " now is unmounted " << endl;    
                      
                    // remove disk from the vector of mounted devices
                    removeMountedDevice(tempRemoveDevices[i]);
                    //cout << "Removed mount: " << tempRemoveDevices[i].getName() << " in slot: " << slot << endl;
                            
                    if (slot != -1)
                    {
                        modules_m[slot].removeDiskDevice(tempRemoveDevices[i]);
                        //cout << "Removed device: " << tempRemoveDevices[i].getName() << endl;
                    }
                    else
                        throw  Mark6Exception("Trying to remove a disk device that has no associated slot.");
                }
                else
                {
                    // something didn't work try again on the next go
                    removedDevices_m.push_back( tempRemoveDevices[i]);
                    //cout << " now is still mounted ";
                }
            }    
            
        }        
    }    
}

/**
 * Loops over all mount points required to mount the mark6 data and meta partition. A check is
 * performed whether these directories exist. If not they will be created.
*/
void  Mark6::validateMountPoints()
{
	//clog << "validateMountPoints" << endl;
       
        
        createMountPoint(mountRootData_m);
        createMountPoint(mountRootMeta_m);
      
	for(int i=0; i < numSlots_m; i++)
	{
            //string path = mountRootData_m + slotIds_m[i];
            createMountPoint(mountRootData_m + slotIds_m[i]);
            createMountPoint(mountRootMeta_m + slotIds_m[i]);

            for(int j=0; j < Mark6Module::MAXDISKS; j++)
            {
                createMountPoint(mountRootData_m + slotIds_m[i] + "/" + diskIds_m[j]);
                createMountPoint(mountRootMeta_m + slotIds_m[i] + "/" + diskIds_m[j]);
            }
	}
}

/**
 * Adds a partition with the given name to the corresponding disk device object. 
 * @return 1 in case the corresponding disk device does not exist; 0 otherwise
 */
int Mark6::addPartitionDevice(string partitionName)
{    
    // determine corresponding disk device (assuming less than 9 partitions per disk)
    string diskDevice = partitionName.substr(0, partitionName.size()-1);
    //cout << "Adding partition " << partitionName << " to " << newDevices_m[i].getName() << endl;
    
     // check if disk device object already exists for this partition
    for(std::vector<Mark6DiskDevice>::size_type i = 0; i != newDevices_m.size(); i++) {
        //cout << "Adding partition " << partitionName << " to " << newDevices_m[i].getName() << endl;
        if (newDevices_m[i].getName() == diskDevice)
        {
            //cout << "Adding partition " << partitionName << " to " << newDevices_m[i].getName() << endl;
            newDevices_m[i].addPartition(partitionName);
            return(0);
        }
    }
    
    return(1);
}

/**
 * Checks whether a disk device with the given device name is currently mounted
 * @return true in case the device is mounted; false otherwise
 */
bool Mark6::isMounted(string deviceName)
{
    // loop over all mounted disks
    for (std::vector<Mark6DiskDevice>::size_type i=0; i < mountedDevices_m.size(); i++)
    {
        if (mountedDevices_m[i].getName() == deviceName)
        {
            return(true);
        }
    }
    
    return(false);
}

/**
 * Check whether the disk device with the given device name is currently mounted and returns it
 * @return the Mark6DiskDevice in case is is currently mounted; NULL otherwise 
 */
Mark6DiskDevice *Mark6::getMountedDevice(string deviceName)
{
    // loop over all mounted disks
    for (std::vector<Mark6DiskDevice>::size_type i=0; i < mountedDevices_m.size(); i++)
    {
        if (mountedDevices_m[i].getName() == deviceName)
        {
            return(&mountedDevices_m[i]);
        }
    }
    
    return(NULL);
}

/**
 * Removes the given device from the vector holding the currently mounted devices
 */
void Mark6::removeMountedDevice(Mark6DiskDevice device)
{
    
    // loop over all mounted disks
    for( vector<Mark6DiskDevice>::iterator iter = mountedDevices_m.begin(); iter != mountedDevices_m.end(); ++iter )
    {
        if( (*iter).getName() == device.getName() )
        {
            mountedDevices_m.erase( iter );
            break;
        }
    }
}

/**
 * Validates that the system devices corresponding to the currentlt mounted disk devices exist. This check is
 * neccesary because catching remove events by libudev does not work reliably if many devices get turned off 
 * simultaneously ( several Mark6 keys turned in a short time).
 * If a system device does not exist anymore the device is removed from the vector of currently mounted devices.
 */
int Mark6::verifyDevices()
{
  int count = 0;

  // loop over all mounted disks
  for( vector<Mark6DiskDevice>::iterator iter = mountedDevices_m.begin(); iter != mountedDevices_m.end(); ++iter )
  {
      //cout << "verify " << (*iter).getName()  << endl;
      if (access(("/dev/"+(*iter).getName()).c_str(), F_OK == -1))
      {
          //cout << "device " << (*iter).getName() << "does not exist anymore. Removing" << endl;
          removedDevices_m.push_back(*iter);
          count++;
      }
  }
  return(count);
  
}

/**
 * Cleans up any left-over mounts in the default mount location (default /mnt/disks). Such remaining mounts might
 * result from a crash of the mark5daemon or other unexpected events.
 */
void Mark6::cleanUp()
{
       
    for(int i=0; i < numSlots_m; i++)
    {
        for(int j=0; j < Mark6Module::MAXDISKS; j++)
        {
            string dataPath = mountRootData_m + slotIds_m[i] + "/" + diskIds_m[j]; 
            string metaPath = mountRootMeta_m + slotIds_m[i] + "/" + diskIds_m[j];      
            
            //cout << "unmounting: " << dataPath << endl;
            umount2(dataPath.c_str(), MNT_FORCE);
            umount2(metaPath.c_str(), MNT_FORCE);
        } 
    }
}

/**
 * Look for devices that were added or removed 
 */
void Mark6::pollDevices()
{
    vector<std::string> tempPartitions;

    cout << "Polling for new devices" << endl;
    clog << "Polling for new devices" << endl;

    
    /*for(std::vector<Mark6DiskDevice>::size_type i = 0; i != newDevices_m.size(); i++) {
        cout << " new: " << newDevices_m[i].getName() << endl;
    }
    for(std::vector<Mark6DiskDevice>::size_type i = 0; i != removedDevices_m.size(); i++) {
        cout << " remove : " << removedDevices_m[i].getName() << endl;
    }*/

       // while there are hotplug events to process
       // NOTE: the timeout parameter should be large enough to cope with
       // the large number of events in case of all keys get turned simultaneously.
       // Reducing the timeout too much will lead to dropped hotplugged events
       while(poll(pollItems_m, 1, 3000) > 0)
       {

              // receive the relevant device
               udev_device* dev = udev_monitor_receive_device(mon);
               if(!dev)
               {
                    //cout << "Cannot receive device information" << endl;
                   // error receiving device, skip it
                   continue;
               }
                // parent device
                udev_device  *parent = udev_device_get_parent(dev);
                
               string action(udev_device_get_action(dev));
               string devtype(udev_device_get_devtype(dev));
/*
               cout << "hotplug[" << udev_device_get_action(dev) << "] ";
               cout << udev_device_get_devnode(dev) << ",";
               cout << udev_device_get_subsystem(dev) << ",";
               cout << udev_device_get_devtype(dev) << ",";
                cout << endl;

               cout << " devpath="<< udev_device_get_devpath(dev) << endl;
               cout << " syspath="<< udev_device_get_syspath(dev) << endl;
               cout << " sysname="<< udev_device_get_sysname(dev) << endl;
               cout << " devnode="<< udev_device_get_devnode(dev) << endl;
               cout << " devnum="<< udev_device_get_devnum(dev) << endl;

		udev_device  *parent = udev_device_get_parent(dev);
                cout << "parent ";
                
                
                cout << " subsystem = " << udev_device_get_subsystem(parent) ;
                cout << " devtype = " << udev_device_get_devtype(parent);
                cout << " devpath="<< udev_device_get_devpath(parent) << endl;
                cout << " syspath="<< udev_device_get_syspath(parent) << endl;
                cout << " sysname="<< udev_device_get_sysname(parent) << endl;
                cout << " devnum = " << udev_device_get_devnum(parent) << endl;
                cout << endl;
*/
               if (action ==  "add")
               {
                    if (devtype == "disk")
                    {
                       // cout << " serial_short="<< udev_device_get_property_value(dev,"ID_SERIAL_SHORT") << endl;
                       // cout << " sas_address="<< udev_device_get_sysattr_value(parent,"sas_address") << endl;
                       
                       try {
                            Mark6DiskDevice disk = newDeviceFromUdev(dev);
                            newDevices_m.push_back(disk);
                            
                            // determine size
                            string res = execCommand(("cat /sys/class/block/" + string(udev_device_get_sysname(dev)) + "/size").c_str());
                            disk.capacityMB_m = atoi(res.c_str()) * 512 / 1000000000;
                        } catch (...) {
                            break;
                        }

                        /*     
                        // add new disk device
                        Mark6DiskDevice disk(string(udev_device_get_sysname(dev)));
                        int controllerId = parseControllerId(udev_device_get_devpath(dev));
                        string driver = (getControllerById(controllerId))->getDriver();
                        disk.setControllerId( controllerId );

			if(0 != udev_device_get_sysattr_value(parent,"sas_address"))
			{
                            disk.setSasAddress (udev_device_get_sysattr_value(parent,"sas_address"));
                            //disk.setDiskId(parseDiskId(udev_device_get_sysattr_value(parent,"sas_address"), driver));
                            disk.setDiskId(parsePhyId(udev_device_get_sysattr_value(parent,"sas_address")));
                            disk.setSerial(udev_device_get_property_value(dev,"ID_SERIAL_SHORT") );

                            newDevices_m.push_back(disk);
			}
                        //cout << disk.getControllerId()  << endl;
                        // cout << "Controller ID=" << disk.getControllerId() << endl;
                        //cout << "Disk ID=" << disk.getDiskId() << endl;
                        //cout << "Disk serial=" << disk.getSerial() << endl;


                        */
                        
                    }
                    else if (devtype == "partition")
                    {   
                        udev_device *grandparent = udev_device_get_parent(parent);
			if(0 != udev_device_get_sysattr_value(grandparent, "sas_address"))
			{
                            string partitionName = string(udev_device_get_sysname(dev));
                            newPartitions_m.push_back(partitionName);
			}
                    }
               }
               else if ((action ==  "remove") && (devtype == "disk"))
               {
                       Mark6DiskDevice disk(string(udev_device_get_sysname(dev)));
                       removedDevices_m.push_back(disk);
               }

               // destroy the relevant device
               udev_device_unref(dev);

               // clear the revents
               pollItems_m[0].revents = 0;
       }

      // devices have appeared or disappeared 
      if ((newDevices_m.size() > 0) || (removedDevices_m.size() > 0))
      {
          manageDeviceChange();                
          verifyDevices_m = true;
      }
      else
      {
          if (verifyDevices_m)
          {
              
              verifyDevices();
              verifyDevices_m = false;
          }
      }

      if (newPartitions_m.size() > 0)
      {
          tempPartitions.swap(newPartitions_m);

          /*for(std::vector<std::string>::size_type i = 0; i != tempPartitions.size(); i++) {
              cout << " new partition: " << tempPartitions[i] << endl;
          }*/
          for( vector<std::string>::iterator iter = tempPartitions.begin(); iter != tempPartitions.end(); ++iter ) {
              // add partition to disk device object
              if (addPartitionDevice( *iter ) != 0) {
                  newPartitions_m.push_back(*iter);
              }
          }
      }
                
/*
                cout << "Currently mounted devices:" << endl;
                for (std::vector<Mark6DiskDevice>::size_type i=0; i < mountedDevices_m.size(); i++)
                {
                    cout << mountedDevices_m[i].getName() << " " << mountedDevices_m[i].getMeta().getEMSN() << " " << endl;
                }
                cout << endl;
*/
                
//                cout << "Currently mounted modules:" << endl;
                clog << "Currently mounted modules:" << endl;
                for (int iSlot=0; iSlot < controllers_m.size()*2; iSlot++)
                {
                    //modules_m[iSlot].isComplete();
                    //cout << "Slot " << iSlot << " = " << modules_m[iSlot].getEMSN() << " (" << modules_m[iSlot].getNumDiskDevices() << " disks) " << modules_m[iSlot].isComplete() << endl;
 //                   cout << "Slot " << iSlot+1 << " = " << modules_m[iSlot].getEMSN() << " (" << modules_m[iSlot].getNumDiskDevices() << " disks) " << endl;
                    clog << "Slot " << iSlot+1 << " = " << modules_m[iSlot].getEMSN() << " (" << modules_m[iSlot].getNumDiskDevices() << " disks) " << endl;
		    /*if (modules_m[iSlot].getNumDiskDevices() > 0)
		    {
			//if (modules_m[iSlot].diskDevices_m.empty()) { continue; }
			map<int, string> serials = modules_m[iSlot].getDiskDevice(0)->getMeta().getSerials();

			// loop over all serials found in the meta data
			map<int, string>::iterator it;
			for ( it = serials.begin(); it != serials.end(); it++ )
			{
			   cout << "matching " << it->first << " " << it->second << endl;
			}
                        cout << endl;
		    }*/
                }

    }


Mark6DiskDevice Mark6::newDeviceFromUdev(udev_device *dev)
{

    udev_device  *parent = udev_device_get_parent(dev);
    const char *sysname = udev_device_get_sysname(dev);
    const char *devpath = udev_device_get_devpath(dev);
    const char *sasaddress = udev_device_get_sysattr_value(parent,"sas_address");
    const char *serial = udev_device_get_property_value(dev,"ID_SERIAL_SHORT");
    const char *idSasPath = udev_device_get_property_value(dev, "ID_SAS_PATH");

    string driver = "";

    if (sysname != NULL)
    {
        string devName = string(sysname);
        Mark6DiskDevice disk(devName);

        if (devpath != NULL)
        {
            int controllerId = parseControllerId(string(devpath));
            if (controllerId == -1)
                throw Mark6Exception("");
            disk.setControllerId( controllerId );
            //Mark6Controller *controller = getControllerById(controllerId);
            //driver = controller->getDriver();

        }
  
        //determine disk id
        if (idSasPath != NULL)
        {
            disk.setDiskId(parsePhyId(string(idSasPath)));
        }

        //determine sas address
        if (sasaddress != NULL)
        {
            disk.setSasAddress(sasaddress);
        }
        //determine disk serial id
        if (serial != NULL)
            disk.setSerial(string(serial));

        return(disk);

    }
    else
    {
        throw Mark6Exception("");
    }
}

void Mark6::writeControllerConfig()
{
    struct stat st;
    bool host0 = false;
    
    // If /etc/default does not exist create it
    if (stat("/etc/default", &st) == -1) {
        mkdir("/etc/default", 0700);
    }

    // sort controller name alphabetically
    std::set<std::string> sorted;
    for(std::size_t i = 0; i < controllers_m.size(); ++i) {
        // for compatibility always place host0 on index 1 so that it will mount under slots 3 and 4
        if (controllers_m[i].getName() == "host0")
        {
            host0 = true; 
            continue;
        }
        sorted.insert(controllers_m[i].getName());
    }

    ofstream conf ("/etc/default/mark6_slots");

    conf << "# mark6 SAS controller configuration." << endl;
    conf << "# each line contains the name of a SAS controller present in the system" << endl;
    conf << "# the order determines the mount location (first line will mount slots 1&2, second line slots 3&4 etc.)" << endl;
    conf << "# adapt the order to match the local cabling" << endl;

    for( std::set<std::string>::iterator it = sorted.begin(); it != sorted.end(); ++it )
    {
        if ((distance(sorted.begin(), it) == 1) && host0)
        {
          conf << "host0" << endl;
          host0 = false;
        }
        conf << *it << endl;
    }
    if (host0)
      conf << "host0" << endl;

    conf.close();

    clog << "Wrote new sas controller configuration: /etc/default/mark6_slots. Check if automatic slot assignments matches local cabling" << endl;
    
}


/**
 * Reads the controller configuration file /etc/default/marks_slots
 * in order to determine the mapping between controller and slots.
 * The first controller in the file will mount its disks in slots 1&2
 * The seconds one in slots 3&$ and so forth
 *
 * @returns the number of controllers found in the config file
 * @returns -1 if the copnfig file cannot be read
 * **/
int Mark6::readControllerConfig()
{
    std::string line;
    int count = 0;

    // check for controller config
    ifstream conf ("/etc/default/mark6_slots");
    if (conf.is_open())
    {
        clog << "opened /etc/default/mark6_slots" << endl;
        while ( getline (conf,line) )
        {
            if (line.rfind("#", 0) == 0 || line.size() < 1) // todo: " lrtrim(line).size() "
                continue;
            for(std::size_t i = 0; i < controllers_m.size(); ++i) {
                if (line.compare (controllers_m[i].getName()) == 0)
                {
                    controllers_m[i].setOrder(count);
                    break;
                }
            }
            count++;
        }
        conf.close();
    }
    else
    {
        return(-1);
    }

    /*for(std::size_t i = 0; i < controllers_m.size(); ++i) {
        clog << controllers_m[i].getName() << " " << controllers_m[i].getOrder() << endl;
    }*/

    return(count);
}

/**
 * Uses udev to determine the number and names of the SAS controllers installed in the system
 *
 * @returns the number of SAS controllers identified in the system
 * */

int Mark6::enumerateControllers()
{
struct udev_enumerate *enumerate;
    struct udev_list_entry *devices, *dev_list_entry;
    struct udev_device *dev;

    // obtain the list of currently present block devices
    
    //clog << "enumerateControllers" << endl;
    
    enumerate = udev_enumerate_new(udev_m);
    udev_enumerate_add_match_subsystem(enumerate, "sas_host");
    udev_enumerate_scan_devices(enumerate);
    devices = udev_enumerate_get_list_entry(enumerate);

    // loop over list of currently present block devices
    udev_list_entry_foreach(dev_list_entry, devices)
    {
        const char *path;

        path = udev_list_entry_get_name(dev_list_entry);
        dev = udev_device_new_from_syspath(udev_m, path);
        /*cout << udev_device_get_sysname(dev) <<  " " << path << " " << udev_device_get_devpath(dev) << endl;
        cout << udev_device_get_subsystem(dev) << endl;
        cout << udev_device_get_sysnum(dev) << endl;
        cout << udev_device_get_devnum(dev) << endl;
        cout << udev_device_get_seqnum(dev) << endl; */

        Mark6Controller controller;
        controller.setName(udev_device_get_sysname(dev));
        controller.setPath(udev_device_get_devpath(dev));
        controller.setSysNum(udev_device_get_sysnum(dev));

        // get driver in order to distinguish between sas2 and sas3 controllers
        udev_device  *parent = udev_device_get_parent(dev);
        udev_device  *grand = udev_device_get_parent(parent);
        udev_device  *port = udev_device_get_parent(grand);
        
        controller.setDriver(udev_device_get_driver(grand));
        //cout << udev_device_get_driver(grand) << endl;

        const char *phy_count;
        //udev_device_get_property_value
        phy_count = udev_device_get_property_value(port, "id_sas_path");

        /*if (phy_count != NULL)
                cout << "phy count: " << phy_count << endl;
        else
                cout << "not found" << endl;
        */

        clog << "Detected SAS controller: " << controller.getName() << " " << controller.getPath() << " " << controller.getDriver() << " " << phy_count<< endl;
        //cout << "Detected SAS controller: " << controller.getName() << " " << controller.getPath() << " " << controller.getDriver() << endl;

        controllers_m.push_back(controller);
    }

    udev_enumerate_unref(enumerate);

    return(controllers_m.size());
}

int Mark6::enumerateDevices()
{
    int devCount = 0;
    struct udev_enumerate *enumerate;
    struct udev_list_entry *devices, *dev_list_entry;
    struct udev_device *dev;
        
    // obtain the list of currently present block devices

    //clog << "enumerateDevices" << endl;

    enumerate = udev_enumerate_new(udev_m);
    udev_enumerate_add_match_subsystem(enumerate, "block");
    udev_enumerate_scan_devices(enumerate);
    devices = udev_enumerate_get_list_entry(enumerate);
    
    // loop over list of currently present block devices
    udev_list_entry_foreach(dev_list_entry, devices) 
    {
        const char *path;
        
        path = udev_list_entry_get_name(dev_list_entry);
        dev = udev_device_new_from_syspath(udev_m, path);
        
        if(!dev)
        {
            // error receiving device, skip it
            continue;
        }

        string devtype(udev_device_get_devtype(dev));
	//cout << devtype << path <<endl;
        
	udev_device  *parent = udev_device_get_parent(dev);

	if (devtype =="disk")
	{
           /*             //cout << devtype << path <<endl;
                        // add new disk device
                        const char *sysname = udev_device_get_sysname(dev);
                        const char *devpath = udev_device_get_devpath(dev);
                        const char *sasaddress = udev_device_get_sysattr_value(parent,"sas_address");
			const char *serial = udev_device_get_property_value(dev,"ID_SERIAL_SHORT");
                        const char *idSasPath = udev_device_get_property_value(dev, "ID_SAS_PATH");
                        string driver = "";

			if (sysname != NULL)
			{ 
				string devName = string(sysname);                               
				Mark6DiskDevice disk(devName);

				if (devpath != NULL)
                                {
                                        //cout << "sysname : " <<  sysname << endl;
                                        //cout << "Dev path: " <<  devpath << endl;
                                        int controllerId = parseControllerId(string(devpath));
                                        if (controllerId == -1)
                                            break;
                                        Mark6Controller *controller = getControllerById(controllerId);
                                        
                                        
                                        driver = controller->getDriver();
					disk.setControllerId( controllerId );
                                }

                                if (idSasPath != NULL)
                                {
				    disk.setDiskId(parsePhyId(string(idSasPath)));
                                }
				if (sasaddress != NULL)
                                {
                                        disk.setSasAddress(sasaddress);
                                }
				if (serial != NULL)
					disk.setSerial(string(serial));
							       
				newDevices_m.push_back(disk);			
				devCount++;
				//
				//cout << "device " << devName << " serial " << serial <<endl;
                                //cout << disk.getSasAddress() << endl;
				//cout << "Controller ID=" << disk.getControllerId() << endl;
				//cout << "Disk ID=" << disk.getDiskId() << endl;
				//cout << "Disk serial=" << disk.getSerial() << endl;
			}
                  */
                  try {
                      Mark6DiskDevice disk = newDeviceFromUdev(dev);
                      newDevices_m.push_back(disk);                   
                      devCount++;
                  } catch (...) {
                      break;
                  }

	    }
	    else if (devtype == "partition")
	    {   
		if (udev_device_get_sysname(dev) != NULL)
		{
			string partitionName = string(udev_device_get_sysname(dev));
			// add partition to disk device object
			if (addPartitionDevice(partitionName) != 0)
			{
			    throw Mark6Exception (string("Partition found (" + partitionName + ") without corresponding disk device") );              
			}
		}
	    }
	}

    udev_enumerate_unref(enumerate);

    return(devCount);
    
}

long Mark6::parsePhyId(std::string sasPath)
{
    long diskId = -1;
    string phyId = "";

    //cout << "ID_SAS_PATH: " << sasPath << endl;

    std::size_t found = sasPath.find("phy");
    if (found!=std::string::npos)
    {

       if (isdigit(sasPath.at(found+4)) ){
          phyId = sasPath.substr(found+3, 2);
       }
       else {

          phyId = sasPath.substr(found+3, 1);
       }
       diskId = atol(phyId.c_str());
    }

    //cout << "Disk Id: " << diskId << endl;
    return (diskId);
}

/**
 * Deprecated. The sasaddress turned out to encode the phy id differently for different sas controller manufacturers. Use
 * parsePhyId instead.
 *
 * Obtains the sequence number of the disk device on the SAS controller by parsing the contents of the sas_device attribute provided by UDEV.
 * If cabeling of the Mark6 SAS controller 
 * is done correctly this sequence number should correspond to the module LEDs on the frontpack of the diskpack.
 * Note that the disk id corresponds to the SAS phy* parameter not the port number which can be arbitrary
 * @returns the disk id; -1 if the id could not be parsed from the given sas address
 */
long Mark6::parseDiskId(std::string sasAddress, std::string driver)
{
    long diskId = -1;

    //cout << "DiskID = " << strtoul(sasAddress.substr(11,1).c_str(), NULL, 16) << " " << sasAddress << " " << sasAddress.substr(11,1) << sasAddress.substr(1,1) << endl;
    if (sasAddress.size() < 11)
        return(-1);

    // SAS Address does appear to encode the actual phy* iface number to which the disk is attached,
    // but it is not clear if the encoding is consistent accross different systems.
    // With mpt2sas-driven controllers the phy iface is apparently at addr_str[11],
    // while for mpt3sas it is at the end of addr_str[].

    // MPIfR mark6-06
    //   slot 3, mpt2sas
    //     sasAddress 0x4433221100000000
    //     sasAddress 0x4433221101000000
    //     sasAddress 0x4433221102000000
    //     sasAddress 0x4433221103000000
    //     sasAddress 0x4433221104000000
    //     sasAddress 0x4433221105000000
    //     sasAddress 0x4433221106000000
    //     sasAddress 0x4433221107000000
    //   slot 4, mpt2sas
    //     sasAddress 0x4433221108000000
    //     sasAddress 0x4433221109000000
    //     sasAddress 0x443322110a000000
    //     sasAddress 0x443322110b000000
    //     sasAddress 0x443322110c000000
    //     sasAddress 0x443322110d000000
    //     sasAddress 0x443322110e000000
    //     sasAddress 0x443322110f000000
    //   slot 5, mpt3sas
    //     sasAddress 0x300062b207676380
    //     sasAddress 0x300062b207676381
    //     sasAddress 0x300062b207676382
    //     sasAddress 0x300062b207676383
    //     sasAddress 0x300062b207676384
    //     sasAddress 0x300062b207676385
    //     sasAddress 0x300062b207676386
    //     sasAddress 0x300062b207676387

    size_t strlen = sasAddress.size();

    //if(sasAddress[strlen-2] == '0' && sasAddress[strlen-1] == '0')
  
    //cout << "parseDiskId " << sasAddress << " -> disk id " << diskId << " driver: " << driver << endl;
    if (driver == "mpt2sas")
    {
        diskId = strtol(sasAddress.substr(11,1).c_str(), NULL, 16);
    }
    else if (driver == "mpt3sas")
    {
        diskId = strtol(sasAddress.substr(strlen-1,1).c_str(), NULL, 16);
    }
    else
    {
        return (-1);
    }

    //cout << "parseDiskId " << sasAddress << " -> disk id " << diskId << " driver: " << driver << endl;

    return diskId;
}

/**
 * Obtains the controller object having the given id
 * @returns the Mark6Controller instance; NULL if no controller with matching id was found
 **/
Mark6Controller *Mark6::getControllerById(int id)
{
    for(std::size_t i = 0; i < controllers_m.size(); ++i) {
	if (controllers_m[i].getOrder() == id)
        {
	    return(&controllers_m[i]);
        }

    }
  return(NULL);
}

/**
 * Obtains the controller id by parsing the output 
 * of the devpath information provided by UDEV. If the internal cabling
 * of the Mark6 machine is done correctly module slots 1 and 2 should be
 * located on controller id=1 whereas slots 3 and 4 should have a controller id=0
 * @returns the controller id; -1 if no controller id was found in the devpath
 * @todo controllers are distinguished by the PCI identifier. APPARENTLY one of the controllers always shows as "host0". Don't know if this is always true. Should be discussed with Chet.
 *
 * 
 */
int Mark6::parseControllerId(string devpath)
{
    string name = "";

    
    size_t end;
    size_t found = devpath.find("host");

    // determine name of SAS host adapter
    if (found != string::npos)
    {
        end = devpath.find("/", found);
        if (end != string::npos)
        {
            name = devpath.substr(found, end-found);
        }
    }

    // lookup controller id
    for(std::size_t i = 0; i < controllers_m.size(); ++i) {
        //cout << controllers_m[i].getName()<< " " << name << " " << controllers_m[i].getOrder() << endl;
	if (controllers_m[i].getName() == name)
        {
	    return(controllers_m[i].getOrder());
        }
		
    }
    return(-1);

}
