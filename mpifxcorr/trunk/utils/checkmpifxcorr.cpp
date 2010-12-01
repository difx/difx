/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: mpifxcorr.cpp 2818 2010-11-19 23:36:12Z AdamDeller $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mpifxcorr.cpp $
// $LastChangedRevision: 2818 $
// $Author: AdamDeller $
// $LastChangedDate: 2010-11-19 16:36:12 -0700 (Fri, 19 Nov 2010) $
//
//============================================================================

#include "configuration.h"

int main(int argc, char *argv[])
{
  if(argc != 2)
  {
    cerr << "Error - invoke with checkmpifxcorr <inputfilename>" << endl;
    return EXIT_FAILURE;
  }

  Configuration * config = new Configuration(argv[1], 0);
  if(!config->consistencyOK())
  {
    //There was a problem with the input file, so shut down gracefully
    cerr << "Config encountered inconsistent setup in config file - please check setup" << endl;
    return EXIT_FAILURE;
  }
  cout << "No errors with input file " << argv[1] << endl;
}
