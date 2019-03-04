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
#include <mpi.h>
#include "nativemk5.h"
#include "alert.h"

NativeMk5DataStream::NativeMk5DataStream(const Configuration * conf, int snum,
        int id, int ncores, int * cids, int bufferfactor, int numsegments) :
                Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor,
        numsegments)
{
	cfatal << startl << "NativeMk5DataStream::NativeMk5DataStream stub called, meaning mpifxcorr was not compiled for nativemk5 support, but it was requested (with MODULE in .input file).  Aborting." << endl;
	MPI_Abort(MPI_COMM_WORLD, 1);
}

NativeMk5DataStream::~NativeMk5DataStream()
{
}

void NativeMk5DataStream::initialiseFile(int configindex, int fileindex)
{
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
}

void NativeMk5DataStream::loopfileread()
{
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
}

int NativeMk5DataStream::readonedemux(bool isfirst, int buffersegment)
{
	return -1;
}

int NativeMk5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	return -1;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
