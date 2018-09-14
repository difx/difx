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
// $Id: $
// $HeadURL: $
// $LastChangedRevision: $
// $Author: $
// $LastChangedDate: $
//
//============================================================================
#ifndef CONFIGURATIONSTORAGE_H
#define CONFIGURATIONSTORAGE_H

#include <string>
#include <map>

using namespace std;

/**
@class Configuration file storage
@brief Stores the text-based contents of the correlator control input file and all associated files.

This class stores the content of all correlation setup related files (.input, .calc, .im, .thread,
.binconfig, ...). The class is intended as a helper to exchange control files between MPI nodes
via e.g. MPI Broadcast without having to access these files at every node of the cluster. This
allows to avoid occasional file system overload conditions that prevent opening a file at some
node, leading to an mpifxcorr node abort and a crash.

@author Jan Wagner
*/
class ConfigurationStorage {
public:
  /**
   * C'stor
   */
  ConfigurationStorage();

 /**
  * C'stor to read content of an input file and all ancillary files that it refers to.
  * @param inputfile The filename of the input file containing configuration information to be read
  */
  ConfigurationStorage(const char * inputfile);

  /**
   * Read content of an input file and all ancillary files that it refers to.
   * @param inputfile The filename of the input file containing configuration information to be read
   * @return True on success
   */
  bool readInputfileAndAncillaries(const char * inputfile);

  /**
   * Exchange data over MPI. The origin is always the FXMANAGER node and other nodes are recipients.
   * @param channel Pointer to MPI_Comm communication channel (MPI structure) to use for the exchange
   * @param my_mpiid The MPI ID of the caller
   * @return Number of files exchanged over MPI, 0 on error
   */
  int exchangeOverMPI(void* channel, int my_mpiid);

public:
  /**
   * Accessor to the string contents of any of the previously loaded configuration file.
   * The file may have been loaded locally from disk (c'stor, readInputfileAndAncillaries()),
   * or may have been received from the cluster head node over MPI (exchangeOverMPI()).
   * @param filesuffix The type of config file to return, can be "input", "calc", "im", "binconfig", ...
   * @param index The index of the respective config file if there are multiple
   * @return Reference to string that contains the contents of the requested configuration file
   */
  const string& getFileContent(const char * filesuffix, int index = 0) const;

private:
  void init_keyname2internal();
  bool readFile(const char* filename, string& out) const;
  bool tokenizeIStreamNext(istream& is, string& key, string& value) const;
  bool loadAllReferencedFrom(const string& filecontents);

private:
  map<string,string> keyname2internal;
  std::multimap<string,string> rawconfigfiles;
};

#endif
