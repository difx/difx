//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <mpi.h>
#include "vdifmark5.h"
#include "alert.h"

VDIFMark5DataStream::VDIFMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) : VDIFDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cfatal << startl << "VDIFMark5DataStream::VDIFMark5DataStream stub called, meaning mpifxcorr was not compiled for native Mark5 support, but it was requested (with MODULE in .input file).  Aborting." << endl;
	MPI_Abort(MPI_COMM_WORLD, 1);
}

VDIFMark5DataStream::~VDIFMark5DataStream()
{
}

void VDIFMark5DataStream::startReaderThread()
{
}

void VDIFMark5DataStream::initialiseFile(int configindex, int fileindex)
{
}

void VDIFMark5DataStream::openfile(int configindex, int fileindex)
{
}

int VDIFMark5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	return -1;
}

int VDIFMark5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
{
	return -1;
}

int VDIFMark5DataStream::resetDriveStats()
{
	return -1;
}

int VDIFMark5DataStream::reportDriveStats()
{
	return -1;
}
