/***************************************************************************
 *   Copyright (C) 2026 by Walter Brisken                                  *
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

#ifndef VDIFONETHREADFAKE_H
#define VDIFONETHREADFAKE_H

#include "mk5.h"

class VDIFOneThreadFakeDataStream : public Mk5DataStream
{
public:
	VDIFOneThreadFakeDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
	virtual ~VDIFOneThreadFakeDataStream();
	virtual void initialiseFile(int configindex, int fileindex);
	virtual void openfile(int configindex, int fileindex);
protected:
	virtual int dataRead(int buffersegment);
	virtual void loopfakeread();
private:
	int framespersecond, inputframebytes;
	vdif_header header;
};

#endif
