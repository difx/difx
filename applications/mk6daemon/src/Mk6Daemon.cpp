#include <cstring>
#include <iostream>
#include <sstream>
#include <sys/stat.h>
#include <unistd.h>

#include <proc.h>
#include <difxmessage.h>
#include "Mk6Daemon.h"
#include <ctime>
using namespace std;

const unsigned int Mk6Daemon::LOAD_MON_INTERVAL = 30;
const unsigned int Mk6Daemon::MK6_MON_INTERVAL = 5;




/**
 * Constructor
 **/
//Mk6Daemon::Mk6Daemon(void)
Mk6Daemon::Mk6Daemon(const std::string& logPath, bool logToConsole) : logPath_m(logPath)
{
    char hostName[32];
    maxUserIdLength_m = 32;

    dieNow = false;
    //isHeadNode = 0;
   //isEmbedded = 0;
    //noSu = 0;
    userID = "difx";
    //providedHostname = 0;
    messageGroup = "224.2.2.1";
    messagePort = 52000;
    gethostname(hostName, 32);
    hostname_m = (string)hostName;


    //initialize multicast 
    difxMessageInitFull(-1, "mk6daemon", hostname_m.c_str());
    startMCListen();


    //construct logger
    logFile_m = makeLogfileName();
    logger_m = new Logger(logFile_m, logToConsole);

    logger_m->log(INFO, "mk6daemon starting");
    difxMessageSendDifxInfo("mk6daemon starting");

    difxMessagePrint();

    
    // mark6meta library writes messages to clog
    // redirect clog to be able to write the messages to the mk6daemon log
    // the original destination of clog is stored and can be restored upon exit
    psbuf = m_mk6out.rdbuf();
    m_clogBackup = clog.rdbuf();
    std::clog.rdbuf(psbuf);
}

Mk6Daemon::~Mk6Daemon()
{
    delete logger_m;

    // revert clog redirection
    clog.rdbuf(m_clogBackup);
}

/**
 * Performs sanity checks on the user supplied options
 **/
void Mk6Daemon::validate()
{
        struct stat sb;

	// validate logPath exist
        if (stat(logPath_m.c_str(), &sb) != 0) {
            throw runtime_error(("log path does not exist: " + logPath_m));
        }

	// validate logPath is directory
        if (!S_ISDIR(sb.st_mode)) {
            throw runtime_error(("log path is not a directory: " + logPath_m));
        }
        
        //validate logPath is writable
        if (access(logPath_m.c_str(), W_OK) != 0) {
            throw runtime_error(("log path is not writable: " + logPath_m));
        }
  
	// check length of userID
	if(userID.length() > maxUserIdLength_m)
	{
		fprintf(stderr, "Error: userID is longer than %d chars.\n", maxUserIdLength_m);
	}
}

/**
* Opens a listening socket for receiving multicast difx messages 
*
* @exception runtime_error in case the socket connection cannot be opened
**/

void Mk6Daemon::startMCListen()
{
        this->stopMCListen();

        mcSock_m = difxMessageReceiveOpen();

        if(mcSock_m < 0)
            throw runtime_error("Cannot open multicast socket connection");
}

/**
* Closes the multicast listening socket
**/
void Mk6Daemon::stopMCListen()
{
        difxMessageReceiveClose(mcSock_m);
        mcSock_m = 0;
}

/**
*
**/
string Mk6Daemon::makeLogfileName()
{

    char filename[256];
    time_t t =  time(0);
    float mjd = 40587.0 + t/86400.0;

    snprintf(filename, 256, "%s/%s.%5.0f.log", logPath_m.c_str(), hostname_m.c_str(), mjd);

    return ((string)filename);
}

void Mk6Daemon::checkLoad()
{
        long long curRX, curTX;
        float l1, l5, l15;
        int memused, memtot;
        char message[DIFX_MESSAGE_LENGTH];
        unsigned int netTXRate;
        unsigned int netRXRate;
        int v;

        // load
        v = procGetCPU(&l1, &l5, &l15);
        if (v < 0)
            logger_m->log(WARNING, "Cannot obtain current system load");
        
        // memory usage
        v = procGetMem(&memused, &memtot);
        if(v < 0)
            logger_m->log(WARNING, "Cannot obtain current memory usage");

        // network load 
        v = procGetNet(&curRX, &curTX);
        if(v < 0)
            logger_m->log(WARNING, "Cannot obtain current network stats");


        //calculate network load based on new and previous number of rx/tx bytes
        if(lastRX_m > 0 || lastTX_m > 0)
        {
                long long d;

                d = curRX - lastRX_m;
                netRXRate = d / LOAD_MON_INTERVAL;

                d = curTX - lastTX_m;
                netTXRate = d / LOAD_MON_INTERVAL;
        }
        else
        {
                netRXRate = 0;
                netTXRate = 0;
        }

        lastRX_m = curRX;
        lastTX_m = curTX;

        snprintf(message, DIFX_MESSAGE_LENGTH,
                "LOAD: %4.2f %d %d %5.3f %5.3f \n", l1, memused ,memtot , netRXRate*8.0e-6, netTXRate*8.0e-6);

        logger_m->log(INFO, (string)message);
}

/**
* Determines the currently mounted modules and broadcasts their VSNs
**/
void Mk6Daemon::checkMk6Status()
{
    try
    {
          mark6->pollDevices();

          //mark6->sendStatusMessage();

          mark6->sendSlotStatusMessage();

          std::vector<Mark6Module> modules = mark6->getMountedModules();

          std::stringstream stream;
          stream.str("");
          //if (mark6->isModuleChange()) {
              for (unsigned int i=0; i< modules.size(); i++) {
            
                  //if (modules[i].getVSN() == "")
                  //    continue;

                  stream << "slot " << i+1 << ": " <<  modules[i].getVSN() << " ";
              }
              logger_m->log( INFO, stream.str());
          //}
    }
    catch(Mark6InvalidMetadata& ex) {
        cerr << "No valid Mark6 metadata found, retrying again later. The error was: " << ex.what() << endl;
    }
}

void Mk6Daemon::receiveDifxMessage()
{
    char message[DIFX_MESSAGE_LENGTH];
    int n;
    int v;
    DifxMessageGeneric G;
    char from[64];
    std::time_t t;
    std::tm* now;
    int count = 0;

    for(;;)
    { 
        t = std::time(0) ;
        now = std::localtime(&t);

        asctime(now);
        n = difxMessageReceive(mcSock_m, message, DIFX_MESSAGE_LENGTH-1, from);
                          //cout << message << endl;
        cout << "n = " << n << endl;
        if (n <= 0)
            break;
        count++;
            //message[n] = 0;
              //v = difxMessageParse(&G, message);
              //  difxMessageGenericPrint(&G);
              //if (G.type == 9)
              //  difxMessageGenericPrint(&G);
             // cout << G.type << endl;
    }

    cout <<  "done parsing " << count << " messages " << asctime(now) << endl;

}
