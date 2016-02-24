/***************************************************************************
 *   Copyright (C) 2007-2016 by Adam Deller and Walter Brisken             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
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

#ifndef __VDIF_SERVER_H__
#define __VDIF_SERVER_H__

#include <string>
#include <fstream>
#include <cstdlib>
#include <iostream>
#include "difxio.h"
#include "vdifio.h"
#include "configuration.h"

using namespace std;

// main function
int main(int argc, char *argv[]);

//vdif_server class
class VDIF_Server{
  public:
    //constructor, destructor
    VDIF_Server(string configfile);
    ~VDIF_Server();

    //methods
    bool serve_data();
    inline bool initialisedOK() { return init_ok; }

  protected:
    //methods
    static void * launchNewReadThread(void * thisserver);
    static void * launchNewCopyThread(void * thisserver);

  private:
    //classes
    class VDIF_Copier{
      public:
        //constructor, destructor
        VDIF_Copier(VDIF_Server * p);
	~VDIF_Copier();

	//methods
	void loopcopy();
	inline void stopCopying() { stillcopying = false; }

      private:
	//methods
	void drainsegment(int segment);
	int getThreadBufferIndex(int second, int framenum, int framethread);
	bool threadSegmentComplete(int segment);

	//variables
	VDIF_Server * parent;
	int readsegment;
	int oldestthreadsegment, newestthreadsegment;
	int numreadsegments, numthreadsegments;
	bool stillcopying;
    };

    class VDIF_Reader{
      public:
        //constructor, destructor
        VDIF_Reader(VDIF_Server * p);
	virtual ~VDIF_Reader();
	
	//methods
        void initialise();
	void loopread();
	inline bool stillReading() { return stillreading; }
	inline void stopReading() { stillreading = false; }
      
      protected:
	//methods
	virtual void fillsegment(int segment) = 0;

	//variables
	VDIF_Server * parent;
	int atsegment;
	bool stillreading;
    };

    class VDIF_Module_Reader : public VDIF_Reader{
      public:
        //constructor, destructor
        VDIF_Module_Reader(VDIF_Server * p, string modname);
	virtual ~VDIF_Module_Reader();

      protected:
	//methods
	virtual void fillsegment(int segment);

      private:
	//variables
	string modulename;
    };

    class VDIF_File_Reader : public VDIF_Reader{
      public:
        //constructor, destructor
        VDIF_File_Reader(VDIF_Server * p, int nfiles, string * fnames);
	virtual ~VDIF_File_Reader();

      protected:
	//methods
	virtual void fillsegment(int segment);

      private:
	//methods
	void openfile(int filenum);

	//variables
	int numfiles, atfile;
	string * filenames;
	bool fileopen;
	ifstream input;
    };

    class VDIF_Network_Reader : public VDIF_Reader{
      public:
        //constructor, destructor
        VDIF_Network_Reader(VDIF_Server * p, int tcpwinbyes, int prt);
	virtual ~VDIF_Network_Reader();

      protected:
	//methods
	virtual void fillsegment(int segment);
        void openstream(int portnumber, int tcpwindowsizebytes);

      private:
	//variables
	int tcpwindowbytes, port, socketnumber;
    };

    //constants
    enum datasource {MODULE, FILE, NETWORK};
    static const char * DS_NETWORK_STR;
    static const char * DS_MODULE_STR;
    static const char * DS_FILE_STR;

    //structs
    typedef struct {
      int nthreads;
      int framebytes;
      int framespersec;
      int * threadtcpwins;
      int * threadports;
      string * hostnames;
      string srcname;
      char srccalcode;
      int srcqual;
      string obsmode;
      int scan;
    } serversetting;

    typedef struct {
      int framesfilled;
      int framesrequired;
      int jobsecond;
      int firstframenumber;
      pthread_mutex_t lock;
    } threadsegmentinfo;

    typedef struct {
      int framesfilled;
      pthread_mutex_t lock;
    } readsegmentinfo;

    //methods
    void sendData(int threadsegment);
    void initialiseBuffers();
    void joinThreads();
    bool remainingData();
    bool parseCommonTable(DifxParameters * inputparms);
    bool parseServerTable(DifxParameters * inputparms);
    bool parseDatasourceTable(DifxParameters * inputparms);
    void populateTimeSettings();
    bool consistencyCheck();
    bool matchingSetup(int executesec, serversetting s);
    void lockThreadSegment(int segment);
    void unlockThreadSegment(int segment);
    void updateThreadSegment(int segment);
    void lockReadSegment(int segment);
    void unlockReadSegment(int segment);
    bool socketOpen(int thread);
    void openSocket(int thread, serversetting * setting);
    void sendToSocket(char * buffer, int thread, int sendbytes);
    void closeSockets();

    //variables
    int startmjd, startseconds, executeseconds, numserversetups, numfiles;
    int threadbuffersegmentbytes, readbuffersegmentbytes, numreadbuffersegments, numthreadbuffersegments;
    int maxthreads, sendsegment, lastreadsegment;
    string modulename, inputfile;
    string * filenames;
    int * setting_indices;
    int srctcpwin, srcport;
    datasource datasrc;
    bool init_ok, readthreadstarted, copythreadstarted, sentlastdata;
    char ** threadbuffers;
    int ** socket_handles;
    threadsegmentinfo ** threadbufinfo;
    char * readbuffer;
    readsegmentinfo * readbufinfo;
    pthread_t readthread, copythread;
    pthread_cond_t readinitcond, copyinitcond;
    serversetting * serversettings;
    Configuration * config;
    Model * model;
    DifxParameters * inputparms;
    VDIF_Reader * vdr;
    VDIF_Copier * vdc;
};

#endif
