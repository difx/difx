/***************************************************************************
 *   Copyright (C) 2008-2024 by Walter Brisken                             *
 *                                                                         *
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
#ifndef __SNIFFER_H__
#define __SNIFFER_H__

#include <stdio.h>
#include <complex.h>
#include <fftw3.h>
#include "fitsUV.h"
#include "snifferoptions.h"

struct _Sniffer;

typedef struct _Sniffer Sniffer;

Sniffer *newSniffer(const DifxInput *D, int nComplex, const char *filebase, const SnifferOptions *sOpts);

void deleteSniffer(Sniffer *S);

void flushSniffer(Sniffer *S);

int feedSnifferFITS(Sniffer *S, const DifxVis *dv);

long long getSnifferMemoryUsage(const Sniffer *S);

#endif
