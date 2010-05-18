/***************************************************************************
 *   Copyright (C) 2010 by Roger Cappallo                                  *
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
#ifndef __DIFX2MARK4_H__
#define __DIFX2MARK4_H__

#include <difxio/difx_input.h>

#define array_MAX_BANDS 32
#define array_MAX_TONES 64
#define MAX_INPUT_FILES 4096
#define MAX_VIS 512
#define MAX_STN 50

enum booleans {FALSE, TRUE};

struct CommandLineOptions
    {
    char *fitsFile;
    char *baseFile[MAX_INPUT_FILES];
    char *scan;
    int nBaseFile;
    int writemodel;
    int pretend;
    double scale;
    int verbose;
    /* some overrides */
    int specAvg;
    int doalldifx;
    float nOutChan;
    float startChan;
    int keepOrder;
    int dontCombine;
    int overrideVersion;
    double sniffTime;
    int pulsarBin;
    int phaseCentre;
    double jobMatrixDeltaT; /* seconds */
    };

struct vis_record
    {
    int sync;
    int version;
    int baseline;
    int mjd;
    double iat;
    int config_index;
    int source_index;
    int freq_index;
    char pols[2];
    int pulsar_bin;
    double weight;
    double uvw[3];
    struct components
        {
        float real;
        float imag;
        } comp[MAX_VIS];
    };

struct stations
    {
    int dind;                       // index from difx number to this array
    char mk4_id;                    // single char mk4 station code
    char intl_name[2];              // two letter international stn name
    char difx_name[2];              // two letter code used by difx
    };

#include "type_000.h"
#include "type_100.h"
#include "type_101.h"
#include "type_120.h"
#include "type_300.h"
#include "type_301.h"
#include "type_302.h"

                                    // byte swap prototypes
short short_reverse (short);
unsigned short unsig_reverse (unsigned short);
int int_reverse (int);
long long_reverse (long);
float float_reverse (float);
double double_reverse (double);


#endif
