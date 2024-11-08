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
#include "Mark6.h"
#include "Mark6Module.h"
#include "Mark6DiskDevice.h"
#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>
#include <sstream>
#include <string>

using namespace std;

Mark6Module::Mark6Module() {
    clearMeta();
}

Mark6Module::~Mark6Module() {
}

/**
 * Adds the given disk device to the list of devices associated with this module. 
 *
 * In addition the module meta information (VSN, capacity, datarate. eMSN) is updated based on the 
 * meta information found on the individual disk device
 *
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

    string eMSN = newDevice.getMeta().getEMSN();

    updateMetaFromEMSN(eMSN);
    
    //cout << " added device " << device.getName() << " at position " << pos << " with eMSN " << eMSN << endl;;
}

/**
 * Remove a disk device from the module
 *
 * Removes the given device from the list of devices associated with the module.
 * In case the removed device was the last one on the module the module is reset
 * to the initial state.
 * 
 * @param device the disk device to remove from the module
 */
void Mark6Module::removeDiskDevice(Mark6DiskDevice &device)
{    
    // loop over all devices
    for(int i = 0; i < MAXDISKS; i++)
    { 
        // device hasn't been set at this position
        if ( diskDevices_m.find(i) == diskDevices_m.end() ) 
            continue;
                
        //find the device to be removed
        if( diskDevices_m[i].getName() == device.getName() )
        {           
            diskDevices_m.erase(i);
            break;
        }
    }
   
    // if this was the last disk of the module clear the meta information
    if (getNumDiskDevices() == 0)
        clearMeta();
}

/**
 * Gets the disk device at the given index position
 *
 * @param[in] index
 *
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
 *
 * @return the eMSN of the module
 */
string Mark6Module::getEMSN()
{
    return(eMSN_m);
}

/**
 * Returns the VSN of the module
 *
 * @return the VSN of the module
 */
string Mark6Module::getVSN()
{
    return(vsn_m);
}


/**
 * Returns the capacity of the module (in units of MB)
 *
 * @return the capacity of the module
 */
unsigned long  Mark6Module::getCapacity()
{
    return(capacityMB_m);
}

/**
 * Returns the datarate of the module 
 *
 * @return the datarate of the module
 */
unsigned long  Mark6Module::getDatarate()
{
    return(datarate_m);
}

/**
 * Sets the module VSN
 *
 * The VSN is the 8-character module identifier 
 *
 * @param VSN of the module
 */
void Mark6Module::setVSN(std::string vsn) {
    vsn_m = vsn;
    resetEMSN();
}

/**
 * Sets the module capacity (in MB)
 *
 * @param the capacity of the module (in units of MB)
 */
void Mark6Module::setCapacity(unsigned long capacity) {
    capacityMB_m = capacity;
    resetEMSN();
}

/**
 * Sets the module datarate 
 *
 * @param the datarate of the module 
 */
void Mark6Module::setDatarate(unsigned long datarate) {
    datarate_m = datarate;
    resetEMSN();
}

/**
 * Resets the module EMSN based on the VSN, capacity, datarate and number of installed disks
 *
 * Note that this does not update/rewrite the meta information stored on the individual disk devices
 */
void Mark6Module::resetEMSN() {

    stringstream ss;

    ss << vsn_m << "/" << capacityMB_m << "/" << datarate_m << "/" << getNumDiskDevices();

    ss >> eMSN_m;

    //cout << "Update EMSN to: " << eMSN_m;
}


/**
 * Obtains the number of disks mounted in the module

 * @return the number of disk devices 
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
 *
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
 * Checks that all expected disk devices of the module are found
 *
 * Compares the expected serial numbers found in the meta data against the
 * disk serial numbers obtained via udev during the mounting process.
 * Only if all expected disks are presently mounted completeness of the module
 * is indicated.
 *
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
    // cout << diskIndex << " " << serials.size() << endl;
    // loop over all serials found in the meta data
    map<int, string>::iterator it;
    for ( it = serials.begin(); it != serials.end(); it++ )
    {
        //cout << "matching " << it->second << endl;
        for (int i=0; i < MAXDISKS; i++)
        {
            //cout << "trying " << diskDevices_m[i].getSerial() << endl;
            if (it->second == diskDevices_m[i].getSerial())
            {
                //cout << "match" << endl;
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

/**
 * Parse the given eMSN and determines the relevant meta fields (vsn, capacity, datarate)
 **/
void Mark6Module::updateMetaFromEMSN(string eMSN) {

    std::vector<std::string> field;
    std::string item;
    std::stringstream ss (eMSN);

    // split eMSN
    while (getline (ss, item, '/')) {
        field.push_back (item);
    }
    
    if (field.size() != 4)
        return;

    vsn_m = field[0];
    istringstream (field[1]) >> capacityMB_m;
    istringstream (field[2]) >> datarate_m;

    //capacityMB_m *= getNumDiskDevices();
    

   // cout << "updated vsn: " << vsn_m << " capacity: " <<  capacityMB_m << " datarate: " << datarate_m << endl;
    /*for(vector<string>::size_type i = 0; i != field.size(); i++) {
        cout << field[i] << endl;
    }*/
}


/**
* Clears the meta data information for the module
**/
void Mark6Module::clearMeta() {
    eMSN_m = "";   
    vsn_m = "";
    capacityMB_m = 0;
    datarate_m = 0;
}

void Mark6Module::setGroupMembers(std::vector<std::string> groupMembers_m) {
    this->groupMembers_m = groupMembers_m;
}

std::vector<std::string> Mark6Module::getGroupMembers() const {
    return groupMembers_m;
}
