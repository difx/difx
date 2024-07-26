/***************************************************************************
 *   Copyright (C) 2006-2021 by Adam Deller                                *
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

#include <mpi.h>
#include <stdlib.h>
#include <difxmessage.h>
#include <difxmessage/difxmessageinternal.h>
#include "configuration.h"
#include "alert.h"

void usage(const char *pgm)
{
  cerr << "Usage: " << pgm << " [options] <inputfilename> ..." << endl;
  cerr << endl;
  cerr << "Options can be:" << endl;
  cerr << "  -h : print help info" << endl;
  cerr << "  -f : print messages with level FATAL and worse" << endl;
  cerr << "  -s : print messages with level SEVERE and worse" << endl;
  cerr << "  -e : print messages with level ERROR and worse" << endl;
  cerr << "  -w : print messages with level WARNING and worse [default]" << endl;
  cerr << "  -i : print messages with level INFO and worse" << endl;
  cerr << "  -v : print messages with level VERBOSE and worse" << endl;
  cerr << "  -d : print messages with level DEBUG and worse" << endl;
  cerr << "  -q : produce no output if no problems are found" << endl;
  cerr << endl;
}
 
void setMessageLevel(int msglevel)
{
  if(msglevel < DIFX_ALERT_LEVEL_SEVERE)
  {
    csevere.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
  if(msglevel < DIFX_ALERT_LEVEL_ERROR)
  {
    cerror.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
  if(msglevel < DIFX_ALERT_LEVEL_WARNING)
  {
    cwarn.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
  if(msglevel < DIFX_ALERT_LEVEL_INFO)
  {
    cinfo.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
  if(msglevel < DIFX_ALERT_LEVEL_VERBOSE)
  {
    cverbose.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
  if(msglevel < DIFX_ALERT_LEVEL_DEBUG)
  {
    cdebug.setAlertLevel(DIFX_ALERT_LEVEL_DO_NOT_SEND);
  }
}

int main(int argc, char *argv[])
{
  int msglevel = DIFX_ALERT_LEVEL_WARNING;
  int nFile = 0;
  int nBad = 0;
  int verbose = 1;
  Configuration * config;

  if(argc < 2)
  {
    usage(argv[0]);

    return EXIT_FAILURE;
  }

  for(int a = 1; a < argc; ++a)
  {
    if(strcmp(argv[a], "-h") == 0)
    {
      usage(argv[0]);

      return EXIT_SUCCESS;
    }
    else if(strcmp(argv[a], "-f") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_FATAL;
    }
    else if(strcmp(argv[a], "-s") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_SEVERE;
    }
    else if(strcmp(argv[a], "-e") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_ERROR;
    }
    else if(strcmp(argv[a], "-w") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_WARNING;
    }
    else if(strcmp(argv[a], "-i") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_INFO;
    }
    else if(strcmp(argv[a], "-v") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_VERBOSE;
    }
    else if(strcmp(argv[a], "-d") == 0)
    {
      msglevel = DIFX_ALERT_LEVEL_DEBUG;
    }
    else if(strcmp(argv[a], "-q") == 0)
    {
      verbose -= 1;
    }
    else 
    {
      if(nFile == 0)
      {
        setMessageLevel(msglevel);
        difxMessagePort = -1;
      }

      ++nFile;

      config = new Configuration(argv[a], 0);
      if(!config->consistencyOK())
      {
	//There was a problem with the input file, so shut down gracefully
	cerr << "Config encountered inconsistent setup in config file - please check setup" << endl;
	
        ++nBad;
      }
      else
      {
        if(verbose > 0)
	{
          cout << "No errors with input file " << argv[a] << endl;
	}
      }
      cout << endl;
    }
  }

  if(nBad == 0)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    if(nFile > 1)
    {
      cerr << "Warning: " << nBad << " of " << nFile << " files had major problems." << endl;
    }

    return EXIT_FAILURE;
  }
}
