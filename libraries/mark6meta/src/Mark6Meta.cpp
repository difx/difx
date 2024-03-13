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
// $Id: Mark6Meta.cpp 10521 2022-06-28 12:46:07Z HelgeRottmann $
// $HeadURL: $
// $LastChangedRevision: 10521 $
// $Author: HelgeRottmann $
// $LastChangedDate: 2022-06-28 20:46:07 +0800 (二, 2022-06-28) $
//
//============================================================================
#include "Mark6Meta.h"
#include "Mark6.h"
#include "Mark6Module.h"
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <fstream>
#include <iostream>
#include <sstream>
#include <map>

using namespace std;

Mark6Meta::Mark6Meta() {
    reset();
}

Mark6Meta::Mark6Meta(const Mark6Meta& orig) {
}

Mark6Meta::~Mark6Meta() {
}

void Mark6Meta::reset()
{
    eMSN_m = "empty";
    for (int i=0; i< Mark6Module::MAXDISKS; i++)
        serials_m[i] = "";
    
}

/*
 * Returns a map with the serial numbers of the disks installed in the module. 
 * Note: that the map index respresents the indices aas found in the meta data.
 * This index does not repsent the physical position on the module bus. If you need 
 * the correct physical disk position use Mark6DiskDevice::getPosition().
 * @returns the map holding the disk serial numbers.
 */
const map<int, std::string> &Mark6Meta::getSerials() {
    const map<int, string > &ptr = serials_m;
    return ptr;
}

std::vector<std::string> Mark6Meta::getGroup() const {
    return group_m;
}
   
void Mark6Meta::setSerials( map<int, string > serials) {
    serials_m = serials;
}

string Mark6Meta::getEMSN() const {
    return eMSN_m;
}

void Mark6Meta::setEMSN(string eMSN) {
    eMSN_m = eMSN;
}

void Mark6Meta::setEMSN(std::string vsn, unsigned int capacity, unsigned int datarate, unsigned int numDisks)
{
    std::stringstream ss;

    ss << vsn << "/" << capacity << "/" << datarate << "/" << numDisks;
    ss >> eMSN_m;
}

/**
 * Parses the meta data of a Mark6 disk device. 
 * Since the meta data is located on a partition the disk device needs 
 * to be mounted prior to calling this method.
 */
void Mark6Meta::parse(string rootPath)
{
    
    struct stat info;
    string path = "";
    string line;
    string group;

    // check if directory exists
    if (( stat( rootPath.c_str(), &info ) != 0 ) || (!info.st_mode & S_IFDIR))
    {
        throw  Mark6MountException("The meta directory: " + rootPath + " does not exist");
        
    }
    
    // read contents of eMSN file
    path = rootPath + "/eMSN";
    if (stat( path.c_str(), &info ) != 0 )
    {
        throw  Mark6InvalidMetadata ("The meta file: eMSN does not exist at:" + rootPath);
    }

    ifstream infile(path.c_str());
    infile >> eMSN_m;
    infile.close();

    // parse disk serials
    path = rootPath + "/disk_sn";
    if (stat( path.c_str(), &info ) != 0 )
    {
        throw  Mark6InvalidMetadata ("The meta file: disk_sn does not exist at:" + rootPath);
    }
    
    infile.open(path.c_str());
    while (getline(infile, line))
    {
        string pos = line.substr(0, line.find(":"));
        string serial = line.substr(line.find(":")+1, string::npos);
        
        int index = -1;
        stringstream(pos) >> index;
        
        //serials_m[index] = serial;
        
       // cout << "Meta add serial " << index << " " << serial << endl;
        serials_m.insert (std::pair<int, string>(index , serial));                  
    }
    infile.close();
    
    // parse group in case a group file exist on the meta data partition
    path = rootPath + "/group";
    if (stat( path.c_str(), &info ) == 0 )
    {
        ifstream groupfile(path.c_str());

        int count = 0;
        int groupCount = -1;
        while(getline(groupfile, line, ':')) 
        {
            count++;
            if (count == 1)
            {
                stringstream ss(line);
                ss >> groupCount;
                continue;
            }
            //cout << "token " << line << endl;
            group_m.push_back(line);
        }
        groupfile.close();
        
        if (group_m.size() != count-1)
        {
            stringstream message;
            message << "Meta data indicates a group size of " << groupCount << " but  " << count-1 << " members have been listed.";
            throw Mark6InvalidMetadata (message.str());
        }
    }

        
}
