#include <getopt.h>
#include <iostream>
#include <stdlib.h>
#include <signal.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <mark6meta/Mark6.h>
#include <mark6meta/Mark6Meta.h>
#include <Mk6Daemon.h>
#include <semaphore.h> 
#include <fcntl.h>  
#include "../config.h"

#include <sys/file.h>

using namespace std;

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

// defaults (can be overriden by the environment or command line options
string logPath = "/tmp";
string userID = "difx";
string messageGroup = "224.2.2.1";
unsigned int messagePort = 50200;


//--------
std::streambuf* backup;
int num = -1;
bool is_beep = false;
float sigma = 2.034;
std::string write_file = "default_file.txt";

struct {
    string  logPath ="/tmp";
    bool  console = false;
} options;

/**
*
**/
void cleanExit (int exitCode)
{
    
    cout << " Exiting" << endl;
    // remove named semaphore
    sem_unlink ("/mk6daemon");

    // revert clog redirection
    clog.rdbuf(backup);

    exit(exitCode);
}

void printUsage()
{
    std::cout <<
            "--log/-l <path>:     Specify the path under which the logfile will be created.\n"
            "--console/-c:        Print log output also to sonsole.\n"
            "--num <n>:           Set number of program\n"
            "--beep:              Beep the user\n"
            "--sigma <val>:       Set sigma of program\n"
            "--writeFile <fname>: File to write to\n"
            "--help:              Show help\n";
    cleanExit(1);
}

/**
 * Parse the command line options
 **/
void parseArgs(int argc, char** argv)
{
    const char* const short_opts = "l:cn:bs:w:h";
    const option long_opts[] = {
            {"log", required_argument, nullptr, 'l'},
            {"console", no_argument, nullptr, 'c'},
            {"num", required_argument, nullptr, 'n'},
            {"beep", no_argument, nullptr, 'b'},
            {"sigma", required_argument, nullptr, 's'},
            {"writeFile", required_argument, nullptr, 'w'},
            {"help", no_argument, nullptr, 'h'},
            {nullptr, no_argument, nullptr, 0}
    };

    while (true)
    {
        const auto opt = getopt_long(argc, argv, short_opts, long_opts, nullptr);

        if (-1 == opt)
            break;

        switch (opt)
        {
        case 'l':
            options.logPath = (string) optarg;
            break;

        case 'c':
            options.console = true;
            break;

        case 'n':
            num = std::stoi(optarg);
            std::cout << "Num set to: " << num << std::endl;
            break;

        case 'b':
            is_beep = true;
            std::cout << "Beep is set to true\n";
            break;

        case 's':
            sigma = std::stof(optarg);
            std::cout << "Sigma set to: " << sigma << std::endl;
            break;

        case 'w':
            write_file = std::string(optarg);
            std::cout << "Write file set to: " << write_file << std::endl;
            break;

        case 'h': // -h or --help
        case '?': // Unrecognized option
        default:
            printUsage();
            break;
        }
    }
}

/**
** Overrides default settings from environment variables
**/

void initFromEnv()
{
   const char *e;

    // determine log path from environment
    e = getenv("DIFX_LOG_PATH");
    if(e)
        options.logPath = (string)e;

    // determine default DIFX user id
    e = getenv("DIFX_USER_ID");
    if(e)
        userID = {e};

    // determine multicast group and port from the environent
    e = getenv("DIFX_MESSAGE_PORT");
    if (e) {
        stringstream strValue;
        strValue << e;
        strValue >> messagePort;
    }

    e = getenv("DIFX_MESSAGE_GROUP");
    if (e)
        messageGroup = e;
}


/**
* Sinal handler to be called on SIGINT and SIGTERM events
**/
void intHandler (int s){
    cout << "Terminating program" << endl;
    cleanExit(-1);
}


int main(int argc, char **argv)
{
    std:: ostringstream mk6out;
    time_t t, lastTime;
    double mjd;
    Mk6Daemon *mk6d;
    sem_t *sem; 
    fd_set socks;
    int highSock;
    int readSocks;
    struct timeval timeout;


    // install signal handlers for user/software termination
    signal (SIGINT, intHandler);
    signal (SIGTERM, intHandler);

    try {
        // create named semaphore 
        sem = sem_open ("/mk6daemon", O_CREAT | O_EXCL, 0644, 1); 
 //     sem = sem_open ("/mk6daemon", O_CREAT , 0644, value); 

        // validate that no other instance of mk6daemon is running
        if (sem == SEM_FAILED) {
            cerr << "mk6daemon is already running. Terminate it and retry." << endl;
            cerr << "In case no other instance of mark6daemon is running remove /dev/shm/sem.mk6daemon and retry." << endl;
            cleanExit(-1);
        }

        // initialize defaults from the environment
        initFromEnv();

        // parse command line arguments
        parseArgs(argc, argv);

        mk6d = new Mk6Daemon(options.logPath, options.console);


        //TODO send start message 


        cout << "Using log path: " << mk6d->logPath() << endl;

        
        // mark6meta library writes messages to clog
        // redirect clog to be able to write the messages to the mk6daemon log
        backup = clog.rdbuf();
        clog.rdbuf(mk6out.rdbuf());
        //Logger_logData(mk6daemon.log, mk6out.str().c_str());
        mk6out.str("");

        //attach Mark6
        mk6d->mark6 = new Mark6();


        lastTime = time(0);

        // main loop
        while(!mk6d->dieNow)
        {
            t = time(0);

            // continue until next 1 second boundary
            if(t == lastTime) 
                continue;
            lastTime = t;

            cout << "tick" << endl;

            // monitors system/memory/network load
            if( (t % Mk6Daemon::LOAD_MON_INTERVAL) == 0) {
                    mk6d->checkLoad();
            }

            // check every 5 seconds
            if( (t % Mk6Daemon::MK6_MON_INTERVAL) == 0)
            {
                mk6d->checkMk6Status();
                // check for new modules on a mark6
                /*
                try
                {
                    mk6d->mark6->pollDevices();
                    mk6d->mark6->sendStatusMessage();
                    mk6d->mark6->sendSlotStatusMessage();

                    //Logger_logData(D->log, mk6out.str().c_str());
                    //mk6out.str("");
                }
                catch(Mark6InvalidMetadata& ex)

                {
                    cerr << "No valid Mark6 metadata found, retrying again later. The error was: " << ex.what() << endl;
                }
                */
            }

            highSock = 0;
            FD_ZERO(&socks);
            if(mk6d->mcSock())
                {
                        FD_SET(mk6d->mcSock(), &socks);
                        if(mk6d->mcSock() > highSock)
                        {
                                highSock = mk6d->mcSock();
                        }
                }

            readSocks = select(highSock+1, &socks, 0, 0, &timeout);
        
            cout << "readsocks: " << readSocks << endl;

            if(mk6d->mcSock() && FD_ISSET(mk6d->mcSock(), &socks))
            {
                  cout << "reading" << endl;
                  mk6d->receiveDifxMessage();
            }

        }

    }
    catch (const std::exception& e) {
        cerr << e.what() << endl;
	cout << "error: " << e.what() << endl;
    }

    cleanExit(0); 
}
