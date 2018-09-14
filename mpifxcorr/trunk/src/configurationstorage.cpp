/***************************************************************************
 *   Copyright (C) 2006-2017 by Adam Deller                                *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <mpi.h>
#include <mpifxcorr.h> // for fxcorr::MANAGERID

#include <fstream>
#include <string>
#include <map>
#include <sstream>

#include "configurationstorage.h"

using namespace std;

ConfigurationStorage::ConfigurationStorage()
{
  init_keyname2internal();
  rawconfigfiles.clear();
}

ConfigurationStorage::ConfigurationStorage(const char * inputfile)
{
  init_keyname2internal();
  rawconfigfiles.clear();
  readInputfileAndAncillaries(inputfile);
}

void ConfigurationStorage::init_keyname2internal()
{
  keyname2internal.clear();
  keyname2internal["INPUT FILENAME"] = "input";
  keyname2internal["CALC FILENAME"] = "calc";
  keyname2internal["CORE CONF FILENAME"] = "threads";
  keyname2internal["PULSAR CONFIG FILE"] = "binconfig";
  keyname2internal["PHASED ARRAY CONFIG FILE"] = "phasedarray";
  keyname2internal["VEX FILENAME"] = "vex";
  keyname2internal["IM FILENAME"] = "im";
  keyname2internal["FLAG FILENAME"] = "flag";
}

bool ConfigurationStorage::readInputfileAndAncillaries(const char * inputfile)
{
  rawconfigfiles.clear();

  // load contents of .input as well as the files referenced from .input
  string inputfilecontents;
  bool fileok = readFile(inputfile, inputfilecontents);
  if (!fileok)
  {
    std::cout << "Error: failed to load file " << inputfile << "!\n";
    return false;
  }
  rawconfigfiles.insert(pair<string,string>("input", string(inputfilecontents)));
  if (!loadAllReferencedFrom(inputfilecontents))
    return false;

  // load content of delaymodel files that .calc refers to
  multimap<string,string>::iterator it_calc = rawconfigfiles.find("calc");
  if (it_calc == rawconfigfiles.end())
  {
    std::cout << "Error: unexpectedly there was no .calc file referenced from .input!\n";
    rawconfigfiles.clear();
    return false;
  }
  if (!loadAllReferencedFrom(it_calc->second))
    return false;

  // TODO: load content of pulsar polyco files that .binconfig refers to
  return true;
}

int ConfigurationStorage::exchangeOverMPI(void* channel, int my_mpiid)
{
  const int ROOT_MPI_ID = fxcorr::MANAGERID;
  const int debug_msg = 0;
  MPI_Comm* comm = (MPI_Comm*)channel;
  int size;

  if (channel==NULL)
    return -1;
  if (MPI_Comm_size(*comm, &size) != MPI_SUCCESS)
    return -1;

  if (my_mpiid != ROOT_MPI_ID)
    rawconfigfiles.clear();

  int numfiles = rawconfigfiles.size();
  if (debug_msg)
    cout << "node#" << my_mpiid << " is starting " << (my_mpiid==ROOT_MPI_ID ? "broadcast" : "reception") << " of " << numfiles << " files over MPI\n";

  MPI_Barrier(*comm);
  MPI_Bcast((void*)&numfiles, 1, MPI_INT, ROOT_MPI_ID, *comm);
  if (my_mpiid == ROOT_MPI_ID)
  {
    multimap<string,string>::const_iterator it;
    for (it=rawconfigfiles.begin(); it!=rawconfigfiles.end(); ++it)
    {
      const string& fieldname(it->first), fieldcontent(it->second);
      int fieldnamelen = fieldname.size()+1, fieldcontentlen = fieldcontent.size()+1;
      MPI_Bcast((void*)&fieldnamelen, 1, MPI_INT, ROOT_MPI_ID, *comm);
      MPI_Bcast(const_cast<char*>(fieldname.data()), fieldnamelen, MPI_CHAR, ROOT_MPI_ID, *comm);
      MPI_Bcast((void*)&fieldcontentlen, 1, MPI_INT, ROOT_MPI_ID, *comm);
      MPI_Bcast(const_cast<char*>(fieldcontent.data()), fieldcontentlen, MPI_CHAR, ROOT_MPI_ID, *comm);
    }
  }
  else
  {
    for (int n=0; n<numfiles; n++)
    {
      string fieldname, fieldcontent;
      int fieldnamelen, fieldcontentlen;
      MPI_Bcast((void*)&fieldnamelen, 1, MPI_INT, ROOT_MPI_ID, *comm);
      fieldname.resize(fieldnamelen+4);
      MPI_Bcast(const_cast<char*>(fieldname.data()), fieldnamelen, MPI_CHAR, ROOT_MPI_ID, *comm);
      MPI_Bcast((void*)&fieldcontentlen, 1, MPI_INT, ROOT_MPI_ID, *comm);
      fieldcontent.resize(fieldcontentlen+4);
      MPI_Bcast(const_cast<char*>(fieldcontent.data()), fieldcontentlen, MPI_CHAR, ROOT_MPI_ID, *comm);
      rawconfigfiles.insert(pair<string,string>(fieldname,fieldcontent));
      if (debug_msg)
        cout << "node#" << my_mpiid << " got '" << fieldname << "' with " << fieldcontentlen << " bytes\n";
    }
  }
  if (debug_msg)
    cout << "node#" << my_mpiid << " finished " << (my_mpiid==ROOT_MPI_ID ? "broadcast" : "reception") << " of " << numfiles << " files over MPI\n";

  if (debug_msg)
  {
    multimap<string,string>::const_iterator it;
    for (it=rawconfigfiles.begin(); it!=rawconfigfiles.end(); ++it)
    {
        string shortened(it->second);
        shortened.resize(16);
        std::cout << "node#" << my_mpiid << " rawfile[" << it->first << "] content '" << shortened << " ... '\n";
    }
  }

  MPI_Barrier(*comm);
  return numfiles;
}

bool ConfigurationStorage::readFile(const char* filename, string& out) const
{
  out.clear();
  ifstream in(filename);
  if (in.is_open() && !in.fail())
    out = string((std::istreambuf_iterator<char>(in)), (std::istreambuf_iterator<char>()));
  return (out.length() > 0);
}

bool ConfigurationStorage::tokenizeIStreamNext(istream& is, string& key, string& value) const
{
  const string whitespace("\t\n\v\f\r ");
  string line;
  key.clear();
  value.clear();
  while (true)
  {
    if (is.eof() || is.fail())
      return false;
    std::getline(is, line, '\n');

    // trim all whitespace, skip comments and blank lines
    line.erase(0, line.find_first_not_of(whitespace));
    line.erase(line.find_last_not_of(whitespace) + 1);
    if (line.length() <= 0) continue;
    if (line.at(0) == '#') continue;

    // parse the line, assuming it is in mpifxcorr's usual "<keyname>:<value>" format
    size_t sepidx = line.find_first_of(':');
    if (sepidx == string::npos) continue;
    key = line.substr(0, sepidx);
    value = line.substr(sepidx+1);

    // trim all whitespace before returning the key & value
    key.erase(0, key.find_first_not_of(whitespace));
    key.erase(key.find_last_not_of(whitespace) + 1);
    value.erase(0, value.find_first_not_of(whitespace));
    value.erase(value.find_last_not_of(whitespace) + 1);
    return true;
  }

  return false;
}

bool ConfigurationStorage::loadAllReferencedFrom(const string& filecontents)
{
  string keyname, keyvalue;
  istringstream iss(filecontents);
  while (tokenizeIStreamNext(iss, keyname, keyvalue))
  {
    map<string,string>::iterator entry = keyname2internal.find(keyname);
    if (entry != keyname2internal.end())
    {
      string auxfilecontents, internalname(entry->second);
      bool fileok = readFile(keyvalue.c_str(), auxfilecontents);
      if (!fileok)
      {
        cout << "Warning: failed to load file " << keyvalue << "!\n";
        return false;
      }
      rawconfigfiles.insert(make_pair<string,string>(internalname, string(auxfilecontents)));
      //cout << "loaded file '" << keyvalue << "' into map under '" << internalname << "'\n";
    }
  }
  return true;
}

// vim: shiftwidth=2:softtabstop=2:expandtab

