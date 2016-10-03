/*****************************************************************************
*    <DataSim: VLBI data simulator>                                          * 
*    Copyright (C) <2015> <Zheng Meyer-Zhao>                                 *
*                                                                            *
*    This file is part of DataSim.                                           *
                                                                             *
*    DataSim is free software: you can redistribute it and/or modify         *
*    it under the terms of the GNU General Public License as published by    *
*    the Free Software Foundation, either version 3 of the License, or       *
*    (at your option) any later version.                                     *
*                                                                            *
*    DataSim is distributed in the hope that it will be useful,              *
*    but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*    GNU General Public License for more details.                            *
*                                                                            *
*    You should have received a copy of the GNU General Public License       *
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.   *
*****************************************************************************/

#ifndef __UTIL_H__
#define __UTIL_H__

#include <cstdlib>
#include <gsl/gsl_randist.h>
#include "configuration.h"
#include "subband.h"

/* parameters used for random number generation */
#define MEAN 1.0
#define STDEV 1.0
#define SEED 48573


typedef struct setup {
  int verbose;
  int test;                     // test mode
  unsigned int seed;            // random number generator seed
  float sfluxdensity;           // source flux density in Jansky
  vector<unsigned int> antSEFDs;// antenna SEFD
  std::string inputfilename;    // .input file name
} setup;

/*
 * Initialize subbands of all antennas
 */
int initSubbands(Configuration* config, int configindex, float specRes, 
                  float minStartFreq, vector<Subband*> &subbands, Model* model,
                  float tdur, setup setupinfo);

/*
 * free subbands of all antennas
 */
void freeSubbands(vector<Subband*> &subbands);

/*
 * Check whether the fractional part of a floating point number is 0
 */
bool is_integer(float num);
/*
 * Calculate the maximum spectrum resolution that can be used by all antennas
 */
int getSpecRes(Configuration* config, int configindex, float& specRes, size_t verbose);
/*
 * Calculate the maximum subband width among all antennas
 */
float getMaxChanFreq(Configuration* config, int configindex, size_t verbose);
/*
 * Calculate the number of samples to be generated per time block for the common signal
 */
int getNumSamps(Configuration* config, int configindex, float specRes, size_t verbose);
/*
 * Get the lowest start frequency among all antennas
 */
float getMinStartFreq(Configuration* config, int configindex, size_t verbose);
/*
 * Generate complex numbers using IPP Library
 * @cpDst Destination array where data will be store
 * @len   number of complex samples to generate
 * @stdev standard deviation
 * @rng_inst pointer to random number generator spec
 */
void gencplx(cf32* cpDst, size_t len, f32 stdev, gsl_rng *rng_inst, size_t verbose);
/*
 * Generate TDUR time signal for all subband of all antennas
 */
void genSignal(size_t stdur, cf32* commFreqSig, vector<Subband*>& sbVec, int numSamps, gsl_rng *rng_inst, float tdur, float sfluxdensity, size_t verbose);
/*
 * loop through each subband of each antenna
 * move data from the second half of the array to the first half
 * set the process pointer to the proper location
 */
void movedata(vector<Subband*>& sbVec, size_t verbose);
/*
 * loop through each subband of each antenna
 * select vdif packet size of data to process
 * process the data
 * quantization
 * pack to vdif
 */
int processAndPacketize(size_t framespersec, vector<Subband*>& sbVec, Model* model, size_t verbose);
/*
 * calculate the lowest process pointer among all subband arrays
 */
double getMinProcPtrTime(vector<Subband*>& sbVec, size_t verbose);

#endif /* __UTIL_H__ */

/*
 * eof
 */
