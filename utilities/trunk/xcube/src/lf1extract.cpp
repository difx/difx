/*
 * lf1ReaderSample.cpp
 *
 * W Brisken modified version
 *
 */
#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <string>
#include <vector>

#include "packetReader/CPacketReader.h"
#include "X3cPacket.h"
#include "CTimeFilter.h"
#include "projectManagement/Projects.h"

#define DEVELOPMENT_MACHINE 1


/**
 * on a development computer, we might not have the
 * resources that a production system has.  the DEVELOPMENT_MACHINE
 * define cuts back on the size and number of memory bufs being
 * used by the reader.
 *
 * sizes are in MBytes
 */
#ifdef DEVELOPMENT_MACHINE
#define READ_BUFFER_SIZE 5
#else
#define READ_BUFFER_SIZE 128
#endif

#ifdef DEVELOPMENT_MACHINE
#define READ_BUFFER_CHUNK 2
#else
#define READ_BUFFER_CHUNK 16
#endif



int readLF1file()
{
	char *fileNames[16];
	int nFile = 0;
	FILE *out;

	/***
	 * This section of code shows how to use the Projects class.  From this, we can
	 * eventually get the full stream path/name
	 *
	 * The mount string (mnt) has the path for the project.  the last directory should
	 * be the directory that gets the disk/folder number appended to.  For
	 * example:
	 *    /mnt/disk
	 *       would end up being /mnt/disk0, /mnt/disk1, /mnt/disk2,...
	 *    /mnt/data_sets/vol2/disk
	 *       would end up with .../vol2/disk0, .../vol2/disk1, ...
	 *
	 * The base string gets appended to the mount path.
	 */
	string mnt ("/mnt/disk");
	string base ("/xcube");

	Projects projects(mnt, base, 16);
	vector <PROJECT> prjs = projects.pmGetProjects();
	projects.pmActivateProject("proj_LGR1011_2012_09_20_215011");

	vector<STREAM> strms;
    if (projects.pmGetStreams(strms) != 0)
    {
        fprintf(stdout, "Error retrieving streams for current project\n");
        return -1;
    }

    vector<STREAM>::iterator it1;
    for (it1 = strms.begin(); it1 < strms.end(); it1++)
    {
        STREAM st = *it1;
        if(st.strmName.compare("T51_E0_R0_2012_09_20_215022.lf1") != 0)
            continue;

        vector<string> streamFiles;
        projects.pmGetStreamFiles(&st, streamFiles);

        vector<string>::iterator it2;
        for (it2 = streamFiles.begin(); it2 < streamFiles.end(); it2++)
        {
            string dirName = *it2;
            // at this point, dirName has the full path/name of the stream
            fprintf(stdout, "Stream Names with path: %s\n", dirName.c_str());

	    fileNames[nFile] = strdup(dirName.c_str());
	    ++nFile;
        }
    }

    int nRetVal;


    // Create the packet reader
    CPacketReader *pktReader = new CPacketReader();

    /***
     * Create a time stamp filter.  This will be used for the start/end times of the
     * stream.
     *
     * If the stream has been indexed, it will also seek to that position.  Also included
     * are a couple of time sample tests.
     */
    CTimeFilter startFlt;
    startFlt.setStartTime(0x00000000, 0x14700);
    startFlt.setEndTime(0x99999999, 0x1200000);

    pktReader->setTimeFilter(startFlt);

    /**
     * initialize the readers.
     */
    pktReader->initialize(nFile, fileNames, (size_t) READ_BUFFER_SIZE, (size_t) READ_BUFFER_CHUNK);

    /**
     * start reading in the packets
     */

    out = fopen("vdif", "w");

    try
    {
        nRetVal = pktReader->start();
        if(0 == nRetVal)
        {
            // skip to the correct frame
            int currentPacket = 0;
            bool isDone = false;
//            while (currentPacket < 30)  // way to limit number of packets
            while (false == isDone)
            {
                X3cPacket *pkt = pktReader->getPacket();
                //currentFrame++;
                if (NULL == pkt)
                {
                    fprintf(stdout, "Finished reading. packet <%d>\n", currentPacket);
                    currentPacket = 10;
                    isDone = true;
                }
                else
                {

                    /**
                     * at this point, we have a packet.  now we need to
                     * do something with it.  In the sample code, we are just
                     * writing out the timestamp for the LF1 header.
                     */
                    UINT32 sec, nsec;
                    sec = pkt->getHeader()->ts.tv_sec;
                    nsec = pkt->getHeader()->ts.tv_nsec;

		    const unsigned char *p = (unsigned char *)(pkt->getData());

		    if(pkt->getLen() != 8274) 
			printf("l = %d\n", pkt->getLen());
		    else
		    	fwrite(p+50, pkt->getLen()-50, 1, out); 

                    /**
                     * MAKE SURE TO DELETE the packet when finished!!!
                     */
                    pktReader->freePacket(pkt);
                    currentPacket++;
                }
            }
	    printf("Stopping pktReader\n");
            pktReader->stop();
	    printf("Stopped pktReader\n");
        }
    }
    catch (std::exception ex)
    {
        return -1;
    }

    fclose(out);

    delete pktReader;

    return 0;
}


int main(int argc, char **argv)
{

    return readLF1file();
}
