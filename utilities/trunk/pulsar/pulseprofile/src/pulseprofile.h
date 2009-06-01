/*   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <iostream>
#include <cstdlib>
#include <string>
#include "mpi.h"
#include "configuration.h"
#include "datastream.h"
#include "mode.h"

//constants
#define COREBUFLEN 2

//variables
MPI_Comm world, return_comm;
Configuration * config;
char ** rawbuffer;
int ** offsets;
double ** delaydata;
DataStream * datastream;
Polyco ** polycos;
int ** bins;
Mode * mode;
f64 * resultbuffer;
f64 * profile;
int * coreids;
int * controldata;
//int dsindices[1000];
int numprocs, numcores, numdatastreams, myid, skipseconds, numbins, nsincrement;
int currentconfigindex, executetimeseconds, corecounter, databytes, numchannels;
int currentdsindex;
double profilemin, totalprofile;
bool quit;
ifstream input;
ofstream output;
string * datafilenames;

//functions
void processManager();
void processCore();
void managerTerminate();
void receiveVisData(bool sendagain);
void sendManagerData(int * senddata, int destinationcore);
void coreReceiveData();
void coreProcessData();
