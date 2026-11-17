/***************************************************************************
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

#ifndef __MK6DAEMON_H__
#define __MK6DAEMON_H__

#include <mark6meta/Mark6.h>
#include <Logger.h>

class Mk6Daemon
{
public:
        //constants declarations
        static const unsigned int LOAD_MON_INTERVAL;
        static const unsigned int MK6_MON_INTERVAL;

        //int isHeadNode;
        //int isEmbedded;
        //int noSu;
        //char *providedHostname;
        std::string userID;
        Mark6 *mark6;
//        std::string logPath;
        int  messagePort;
        std::string messageGroup;
        bool dieNow;

        Mk6Daemon(const std::string& logPath, bool logToConsole);
        ~Mk6Daemon();

        void initFromEnv();
        void checkLoad();
        void checkMk6Status();
        void startMCListen();
        void stopMCListen();
        void receiveDifxMessage();

        std::string logPath() { return logPath_m; } 
        int mcSock() { return mcSock_m; }

private:
        unsigned int maxUserIdLength_m;
        int mcSock_m;
        std::string logPath_m;
        std::string logFile_m;
        std::string hostname_m;
        Logger *logger_m;
        void validate();
        std::string makeLogfileName();
        long long lastRX_m = 0;
        long long lastTX_m = 0;
        std::streambuf* m_clogBackup;
        std::streambuf *psbuf;
        std::ostringstream m_mk6out;
};

#endif
