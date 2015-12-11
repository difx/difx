/* 
 * File:   Mark6Module.cpp
 * Author: Helge Rottmann (MPIfR)
 * 
 * Created on 20. Oktober 2015, 15:08
 */

#include "Mark6.h"
#include "Mark6Module.h"
#include "Mark6DiskDevice.h"
#include <algorithm>
#include <iostream>
#include <sstream>

using namespace std;

Mark6Module::Mark6Module() {
    eMSN_m = "";   
}

Mark6Module::Mark6Module(const Mark6Module& orig) {
}

Mark6Module::~Mark6Module() {
}

/**
 * Adds th given disk device to the list of devices associated with this module. 
 * @param[in] device the disk device to add
 */
void Mark6Module::addDiskDevice(Mark6DiskDevice &device)
{
    
    
    int pos = device.getPosition();
    
    if (pos == -1)
    {
        throw  Mark6Exception("diskId for device " + device.getName() + " is not set. Cannot add device to module");
    }
    
    if ((pos < 0 ) || (pos > MAXDISKS))
    {
        throw  Mark6Exception("Illegal position for device " + device.getName() );
    }
    
    Mark6DiskDevice newDevice = Mark6DiskDevice(device);

    diskDevices_m.insert (std::pair<int, Mark6DiskDevice>(pos , newDevice));
    
    //cout << " added device " << device.getName() << " at position " << pos << endl;;
}

/**
 * Removes the given device from the list of devices associated with the module.
 * If symbolic links were associated with partitions on this device they will be removed.
 * In case the removed device was the last one on the module the module is reset
 * to the initial state.
 * @param device the disk device to add to the module
 */
void Mark6Module::removeDiskDevice(Mark6DiskDevice &device)
{    
    // loop over all devices
    for(int i = 0; i < MAXDISKS; i++)
    { 
        // device hasn"t been set at this position
        if ( diskDevices_m.find(i) == diskDevices_m.end() ) 
            continue;
                
        //find the device to be removed
        if( diskDevices_m[i].getName() == device.getName() )
        {
            // remove symbolic links maintained to this device
            diskDevices_m[i].unlinkDisk();
           
            diskDevices_m.erase(i);
            break;
        }
    }
   
    // if this was the last disk of the module clear the eMSN
    if (getNumDiskDevices() == 0)
        eMSN_m = "";
}

/**
 * Gets the disk device with at given index position
 * @param[in] index
 * @return the disk device at the given index position; NULL if no device exists at the index position
 */
Mark6DiskDevice *Mark6Module::getDiskDevice(int index)
{

    
    if ((index < 0 ) || (index > MAXDISKS))
    {
        stringstream message;
        message << "Illegal disk index requested (" << index << ") Must be between 0 and " << MAXDISKS;
        throw  Mark6Exception( message.str());
    }
    
    if ( diskDevices_m.find(index) == diskDevices_m.end() ) 
        return(NULL);
        
    return(&diskDevices_m[index]);
    
}

/**
 * Returns the eMSN of the module
 * @return the eMSN of the module
 */
string Mark6Module::getEMSN()
{
    return(eMSN_m);
}

/**
 * Sets the module eMSN
 * @param eMSN of the module
 */
void Mark6Module::setEMSN(std::string eMSN) {
    eMSN_m = eMSN;
}

/**
 * 
 * @return the number of disk devices associated with this module
 */
int Mark6Module::getNumDiskDevices()
{
    int count = 0;
    
    for (int i=0; i < MAXDISKS; i++)
    {
        if ( diskDevices_m.find(i) != diskDevices_m.end() )
            count++;
    }
    
    return(count);
}
/**
 * Returns the number of expected disks within this disk module, as obtained from the meta data. 
 * @return the number of expected disks; 0 if no information could be obtained from the meta data (e.g. the module is not switched on)
 */
int Mark6Module::getNumTargetDisks()
{
    int target = 0;
    
    // find first associated disk device
    for (int i=0; i < MAXDISKS; i++)
    {
        if ( diskDevices_m.find(i) != diskDevices_m.end() )
        {

            // get expected number of disks from metadata            
            target = diskDevices_m[i].getMeta().getSerials().size();
            //cout << diskDevices_m[i].getMeta().getSerials().size() << endl;
            break;
        }
    }
    
    return(target);
}

/**
 * Compares the expected serial numbers found in the meta data against the
 * disk serial numbers obtained via udev during the mounting process.
 * Only if all expected disks are presently mounted completeness of the module
 * is indicated.
 * @return true if the module is complete; false otherwise
 */
bool Mark6Module::isComplete(){
    
    int diskIndex = -1;
    unsigned int match = 0;
    
    // obtain expected disk serials for this module from the meta data
    // since meta data on all th disks of this module should be identical 
    // let's use the meta from the first not empty disk device
    for (int i=0; i < MAXDISKS; i++)
    {
       if ( diskDevices_m.find(i) != diskDevices_m.end() )
        {  
            diskIndex = i;
            break;
        }
    }
    
   
    map<int, string> serials = diskDevices_m[diskIndex].getMeta().getSerials();
     cout << diskIndex << " " << serials.size() << endl;
    // loop over all serials found in the meta data
    map<int, string>::iterator it;
    for ( it = serials.begin(); it != serials.end(); it++ )
    {
        cout << "matching " << it->second << endl;
        for (int i=0; i < MAXDISKS; i++)
        {
            cout << "trying " << diskDevices_m[i].getSerial() << endl;
            if (it->second == diskDevices_m[i].getSerial())
            {
                cout << "match" << endl;
                match++;
            }
        }
    }
    
    if (match != serials.size())
    {
        return(false);
        
    }
    else
    {
        return(true);
    }

}

void Mark6Module::setGroupMembers(std::vector<std::string> groupMembers_m) {
    this->groupMembers_m = groupMembers_m;
}

std::vector<std::string> Mark6Module::getGroupMembers() const {
    return groupMembers_m;
}
