#include <mpi.h>
#include <string.h>
#include <stdlib.h>
#include "nativemk5.h"
#include "config.h"

NativeMk5DataStream::NativeMk5DataStream(Configuration * conf, int snum,
        int id, int ncores, int * cids, int bufferfactor, int numsegments) :
                Mk5DataStream(conf, snum, id, ncores, cids, bufferfactor,
        numsegments)
{
	cout << "NativeMk5DataStream::NativeMk5DataStream stub called" << endl;
}

NativeMk5DataStream::~NativeMk5DataStream()
{
	cout << "NativeMk5DataStream::~NativeMk5DataStream stub called" << endl;
}

void NativeMk5DataStream::initialiseFile(int configindex, int fileindex)
{
	cout << "NativeMk5DataStream::initialiseFile stub called" << endl;
}

void NativeMk5DataStream::openfile(int configindex, int fileindex)
{
	cout << "NativeMk5DataStream::openfile stub called" << endl;
}

void NativeMk5DataStream::loopfileread()
{
	cout << "NativeMk5DataStream::loopfileread stub called" << endl;
}

void NativeMk5DataStream::moduleToMemory(int buffersegment)
{
	cout << "Never to be called" << endl;
}
