/***************************************************************************
 *   Copyright (C) 2006-2015 by Adam Deller and Walter Brisken             *
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
// $Id: vdiffile.cpp 6684 2015-06-02 11:40:23Z HelgeRottmann $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.cpp $
// $LastChangedRevision: 6684 $
// $Author: HelgeRottmann $
// $LastChangedDate: 2015-06-02 06:40:23 -0500 (Tue, 02 Jun 2015) $
//
//============================================================================
#include "vdifmark6.h"
#include "alert.h"


/// VDIFMark6DataStream -------------------------------------------------------

VDIFMark6DataStream::VDIFMark6DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
 : VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
        cfatal << startl << "Mark6 data format is not yet supported.  Check back soon!" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
}

VDIFMark6DataStream::~VDIFMark6DataStream()
{
}

void VDIFMark6DataStream::closeMark6()
{
}

void VDIFMark6DataStream::openfile(int configindex, int fileindex)
{
}

void VDIFMark6DataStream::initialiseFile(int configindex, int fileindex)
{
}

int VDIFMark6DataStream::dataRead(int buffersegment)
{
	return -1;
}

void VDIFMark6DataStream::mark6ToMemory(int buffersegment)
{
}

void VDIFMark6DataStream::loopfileread()
{
}
