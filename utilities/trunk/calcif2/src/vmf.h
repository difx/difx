/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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


#ifndef __VMF_H__
#define __VMF_H__

#include <gsl/gsl_spline.h>
#include "wxdata.h"

#define VMF_ANTENNA_NAME_LENGTH		12

/* these map directly to columns of data from http://vmf.geo.tuwien.ac.at/trop_products/VLBI/VMF3/VMF3_OP/daily/2019/2019074.vmf3_r */
typedef struct
{
	char antennaName[VMF_ANTENNA_NAME_LENGTH];
	double mjd;
	double a_hydrostatic;
	double a_wet;
	double zd_hydrostatic;	/* [m] "dry" zenith delay */
	double zd_wet;		/* [m] wet zenith delay */
	double pressure;	/* [hPa] */
	double temperature;	/* [C] */
	double pressure_wv;	/* [hPa] water vapor pressure */
} VMFData;

typedef struct
{
	char antennaName[VMF_ANTENNA_NAME_LENGTH];
	
	gsl_interp_accel *acc_ah;
	gsl_spline *spline_ah;
	
	gsl_interp_accel *acc_aw;
	gsl_spline *spline_aw;
	
	gsl_interp_accel *acc_zh;
	gsl_spline *spline_zh;
	
	gsl_interp_accel *acc_zw;
	gsl_spline *spline_zw;
	
	gsl_interp_accel *acc_p;
	gsl_spline *spline_p;
	
	gsl_interp_accel *acc_t;
	gsl_spline *spline_t;
	
	gsl_interp_accel *acc_pw;
	gsl_spline *spline_pw;
} VMFInterpolator;

int loadVMFData(VMFData *data, int maxRows, int mjdStart, int nDay, int verbose);

int selectVMFData(const char *antennaName, VMFData **antennaData, int maxOut, VMFData *vmfData, int nData);

int calculateVMFDifxInput(DifxInput *D, VMFData *vmfData, int vmfRows, WXData *wxData, int verbose);

VMFInterpolator *newVMFInterpolator(VMFData **antennaData, int nRow);

void deleteVMFInterpolator(VMFInterpolator *vi);

void interpolateVMFData(VMFData *vmf, VMFInterpolator *vi, double mjd);

void printVMFData(const VMFData *vmf);

#endif
