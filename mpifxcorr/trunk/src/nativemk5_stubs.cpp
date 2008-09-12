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
#include <string.h>
#include <stdlib.h>
#include "nativemk5.h"
#include "config.h"
#include <difxmessage.h>

NativeMk5DataStream::NativeMk5DataStream(Configuration * conf, int snum,
        int id, int ncores, int * cids, int bufferfactor, int numsegments) :
                Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor,
        numsegments)
{
	difxMessageSendDifxAlert("NativeMk5DataStream::NativeMk5DataStream stub called, meaning mpifxcorr was not compiled for nativemk5 support, but it was requested (with MODULE in .input file).  Aborting.", DIFX_ALERT_LEVEL_FATAL);
	MPI_Abort(MPI_COMM_WORLD, 1);
}

NativeMk5DataStream::~NativeMk5DataStream()
{
	MPI_Abort(MPI_COMM_WORLD, 1);
}

void NativeMk5DataStream::initialiseFile(int configindex, int fileindex)
{
	MPI_Abort(MPI_COMM_WORLD, 1);
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
	MPI_Abort(MPI_COMM_WORLD, 1);
}

void NativeMk5DataStream::loopfileread()
{
	MPI_Abort(MPI_COMM_WORLD, 1);
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	MPI_Abort(MPI_COMM_WORLD, 1);
}
