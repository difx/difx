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
#include "mark5bmark5.h"
#include "alert.h"

Mark5BMark5DataStream::Mark5BMark5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments) : Mark5BDataStream(conf, snum, id, ncores, cids, bufferfactor, numsegments)
{
	cfatal << startl << "Mark5BMark5DataStream::Mark5BMark5DataStream stub called, meaning mpifxcorr was not compiled for native Mark5 support, but it was requested (with MODULE in .input file).  Aborting." << endl;
	MPI_Abort(MPI_COMM_WORLD, 1);
}

Mark5BMark5DataStream::~Mark5BMark5DataStream()
{
}

void Mark5BMark5DataStream::initialiseFile(int configindex, int fileindex)
{
}

void Mark5BMark5DataStream::openfile(int configindex, int fileindex)
{
}

int Mark5BMark5DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
	return -1;
}

int Mark5BMark5DataStream::sendMark5Status(enum Mk5State state, long long position, double dataMJD, float rate)
{
	return -1;
}

int Mark5BMark5DataStream::resetDriveStats()
{
	return -1;
}

int Mark5BMark5DataStream::reportDriveStats()
{
	return -1;
}
