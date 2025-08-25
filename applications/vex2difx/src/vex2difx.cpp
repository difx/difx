/***************************************************************************
 *   Copyright (C) 2009-2022 by Walter Brisken & Adam Deller               *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: vex2difx.cpp 10918 2023-03-15 23:11:14Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/vex2difx/src/vex2difx.cpp $
 * $LastChangedRevision: 10918 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2023-03-16 07:11:14 +0800 (四, 2023-03-16) $
 *
 *==========================================================================*/

#include <algorithm>
#include <cassert>
#include <vector>
#include <set>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iterator>
#include <sys/time.h>
#include <sys/stat.h>
#include <difxio/difx_input.h>
#include <difxmessage.h>
#include <vexdatamodel.h>

#include "autobands.h"
#include "corrparams.h"
#include "freq.h"
#include "job.h"
#include "makejobs.h"
#include "timeutils.h"
#include "sanitycheck.h"
#include "applycorrparams.h"
#include "shelves.h"
#include "../config.h"

using namespace std;


const string version(VERSION);
const string program("vex2difx");
const string verdate("20221115");
const string author("Walter Brisken/Adam Deller");

const int defaultMaxNSBetweenACAvg = 2000000;	// 2ms, good default for use with transient detection

static int calculateWorstcaseGuardNS(double sampleRate, int subintNS, int nBit, int nSubband)
{
	double sampleTimeNS = 1.0e9/sampleRate;
	double nsAccumulate = sampleTimeNS;
	const double MaxEarthGeomSlipRate = 1600.0;	// ns/sec

	while(fabs(nsAccumulate - round(nsAccumulate)) > 2.0e-11)
	{
		nsAccumulate += sampleTimeNS;
	}

	if(nBit*nSubband < 8)
	{
		nsAccumulate = nsAccumulate*8.0/(nBit*nSubband);
	}

	return static_cast<int>(round(nsAccumulate + MaxEarthGeomSlipRate*subintNS*1.0e-9 + 1.0));
}

static DifxJob *makeDifxJob(string directory, const Job& J, int nAntenna, const string& obsCode, int *n, int nDigit, char ext, const CorrParams *P)
{
	DifxJob *job;
	const char *difxVersion;
	const char *difxLabel;
	char jobName[DIFXIO_FILENAME_LENGTH];
	char fileBase[DIFXIO_FILENAME_LENGTH];
	int v;

	*n = 1;
	job = newDifxJobArray(*n);
	difxVersion = getenv("DIFX_VERSION");
	if(difxVersion)
	{
		snprintf(job->difxVersion, DIFXIO_VERSION_LENGTH, "%s", difxVersion);
	}
	else
	{
		snprintf(job->difxVersion, DIFXIO_VERSION_LENGTH, "%s", "Unknown");
	}
	difxLabel = getenv("DIFX_LABEL");
	if(difxLabel)
	{
		snprintf(job->difxLabel, DIFXIO_VERSION_LENGTH, "%s", difxLabel);
	}
	else
	{
		job->difxLabel[0] = 0;
	}
	snprintf(job->vexFile, DIFXIO_FILENAME_LENGTH, "%s", P->vexFile.c_str());
	job->jobStart = J.mjdStart;
	job->jobStop  = J.mjdStop;
	job->mjdStart = J.mjdStart;
	job->duration = trunc((J.mjdStop - J.mjdStart) * 86400.0 + 0.001);
	job->jobId    = J.jobId;
	job->subarrayId = 0;
	snprintf(job->obsCode, DIFXIO_OBSCODE_LENGTH, "%s", obsCode.c_str());
	job->taperFunction = TaperFunctionUniform;
	job->polyOrder = 5;
	job->polyInterval = 120;
	job->aberCorr = AberCorrExact;
	job->activeDatastreams = nAntenna;
	job->activeBaselines = nAntenna*(nAntenna-1)/2;
	job->dutyCycle = J.dutyCycle;
	snprintf(job->delayModel, DIFXIO_FILENAME_LENGTH, "%s", P->delayModel.c_str());

	// The following line defines the format of the job filenames

	if(J.jobId > 0)
	{
		const int FormatLength = 20;
		char format[FormatLength];

		v = snprintf(format, FormatLength, "%%s_%%0%dd%%c", nDigit);
		if(v >= FormatLength)
		{
			cerr << "Developer error: makeDifxJob: format being truncated.  Needed " << v << " bytes." << endl;

			exit(EXIT_FAILURE);
		}
		v = snprintf(jobName, DIFXIO_FILENAME_LENGTH, format, J.jobSeries.c_str(), J.jobId, ext);
	}
	else
	{
		if(ext != 0)
		{
			cerr << "Warning: makeDifxJob: ext!=0 and making job names without extensions!" << endl;

			exit(EXIT_FAILURE);
		}
		v = snprintf(jobName, DIFXIO_FILENAME_LENGTH, "%s", J.jobSeries.c_str());
	}
	v = snprintf(fileBase, DIFXIO_FILENAME_LENGTH, "%s/%s", directory.c_str(), jobName);
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		cerr << "Developer error: makeDifxJob: fileBase needed " << v << " bytes." << endl;

		exit(EXIT_FAILURE);
	}

	snprintf(job->inputFile,   DIFXIO_FILENAME_LENGTH, "%s.input", fileBase);
	snprintf(job->calcFile,    DIFXIO_FILENAME_LENGTH, "%s.calc",  fileBase);
	snprintf(job->flagFile,    DIFXIO_FILENAME_LENGTH, "%s.flag",  fileBase);
	snprintf(job->imFile,      DIFXIO_FILENAME_LENGTH, "%s.im",    fileBase);
	if(P->outPath.empty())
	{
		snprintf(job->outputFile, DIFXIO_FILENAME_LENGTH, "%s.difx", fileBase);
	}
	else
	{
		snprintf(job->outputFile, DIFXIO_FILENAME_LENGTH, "%s/%s.difx", P->outPath.c_str(), jobName);
	}
	if(P->threadsFile.empty())
	{
		v = snprintf(job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s.threads", fileBase);
	}
	else
	{
		v = snprintf(job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s", P->threadsFile.c_str());
	}
	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		cerr << "Developer error: makeDifxJob: threadsFile wanted " << v << " bytes, not " << DIFXIO_FILENAME_LENGTH << " .  Truncating."  << endl;
	}

	return job;
}

static unsigned int getSpacecraftSet(const VexData *V, std::set<std::string> const &sourceSet, std::set<std::string> &spacecraftSet)
{
	spacecraftSet.clear();

	for(std::set<std::string>::const_iterator it = sourceSet.begin(); it != sourceSet.end(); ++it)
	{
		const VexSource *S;

		S = V->getSourceByDefName(*it);
		if(!S)
		{
			cerr << "Developer error: getSpacecraftSet() : source " << *it << " not found in vex data model." << endl;

			exit(EXIT_FAILURE);
		}
		if(S->type == VexSource::EarthSatellite || S->type == VexSource::BSP || S->type == VexSource::TLE || S->type == VexSource::Ephemeris)
		{
			spacecraftSet.insert(*it);
		}
	}

	return spacecraftSet.size();
}

static DifxAntenna *makeDifxAntennas(const Job &J, const VexData *V, int *n)
{
	DifxAntenna *A;
	double mjd;
	vector<string>::const_iterator a;
	int i;

	mjd = 0.5*(V->obsStart() + V->obsStop());

	*n = J.jobAntennas.size();

	A = newDifxAntennaArray(*n);

	// Note: the vsns vector here is used even for non-module corrlation.  It will map an antenna to a non-allowed VSN name which won't be used in case of non-module correlation.
	for(i = 0, a = J.jobAntennas.begin(); a != J.jobAntennas.end(); ++i, ++a)
	{
		const VexAntenna *ant = V->getAntenna(*a);
		snprintf(A[i].name, DIFXIO_NAME_LENGTH, "%s", ant->difxName.c_str());
		A[i].X = ant->x + ant->dx*(mjd-ant->posEpoch)*86400.0;
		A[i].Y = ant->y + ant->dy*(mjd-ant->posEpoch)*86400.0;
		A[i].Z = ant->z + ant->dz*(mjd-ant->posEpoch)*86400.0;
		A[i].mount = stringToMountType(ant->axisType.c_str());
		if(!ant->nasmyth.empty())
		{
			VexAntenna::NasmythType t;

			t = J.getJobNasmythType(V, *a);

			if(t == VexAntenna::NasmythRight)
			{
				A[i].mount = AntennaMountNasmythR;
			}
			else if(t == VexAntenna::NasmythLeft)
			{
				A[i].mount = AntennaMountNasmythL;
			}
			else if(t == VexAntenna::NasmythError)
			{
				cerr << "An inconsistency was found in the Nasmyth mount configurations for antenna " << ant->name << "." << endl;
				cerr << "Currently a single job cannot handle multiple Nasmyth types for one antenna -- it is possible that condition has been encountered." << endl;

				exit(EXIT_FAILURE);
			}
		}
		A[i].clockrefmjd = ant->getVexClocks(J.mjdStart, A[i].clockcoeff, &A[i].clockorder, MAX_MODEL_ORDER);
		for(int j = 0; j <= A[i].clockorder; ++j)
		{
			A[i].clockcoeff[j] *= 1.0e6;	// convert to us/sec^j from sec/sec^j
		}
		A[i].offset[0] = ant->axisOffset;
		A[i].offset[1] = 0.0;
		A[i].offset[2] = 0.0;
	}

	return A;
}

// NOTE: FIXME: before this gets called, all datasource=NONE datastreams should be stripped.

static DifxDatastream *makeDifxDatastreams(const Job& J, const VexData *V, int nSet, DifxAntenna *difxAntennas, const Shelves &shelves)
{
	DifxDatastream *datastreams;
	int nDatastream;
	int di;
	const VexMode *M;

	// Determine worst case (but typical) number of datastreams for this job
	M = V->getModeByDefName(J.modeName);
	if(!M)
	{
		std::cerr << "Developer error: makeDifxDatastreams: getModeByDefName() returns null for modeName=" << J.modeName << std::endl;

		exit(EXIT_FAILURE);
	}
	nDatastream = M->nStream();

	// for each setup these are duplicated
	nDatastream *= nSet;
	
	datastreams = newDifxDatastreamArray(nDatastream);

	di = 0;	// datastream array index
	for(int s = 0; s < nSet; ++s)
	{
		int antennaId = 0;
		for(std::vector<std::string>::const_iterator a = J.jobAntennas.begin(); a != J.jobAntennas.end(); ++a)
		{
			int nd;
			const VexAntenna *ant = V->getAntenna(*a);
			std::string shelf;

			std::map<std::string,VexSetup>::const_iterator sit = M->setups.find(*a);
			if(sit == M->setups.end())
			{
				std::cerr << "Developer error: makeDifxDatastreams: setup for antenna " << *a << " not found in mode " << M->defName << std::endl;

				exit(EXIT_FAILURE);
			}
			const VexSetup &setup = sit->second;

			nd = setup.nStream();

			shelf.clear();
			for(int d = 0; d < nd; ++d)
			{
				DifxDatastream *dd = datastreams + di;
				const VexStream &stream = setup.streams[d];

				dd->antennaId = antennaId;
				dd->dataSource = stream.dataSource;
				dd->tSys = stream.difxTsys;
				dd->dataSampling = stream.dataSampling;
				switch(dd->dataSource)
				{
				case DataSourceNetwork:
					dd->windowSize = ant->ports[d].windowSize;
					snprintf(dd->networkPort, DIFXIO_ETH_DEV_SIZE, "%s", ant->ports[d].networkPort.c_str());
					break;
				case DataSourceFile:
					{
						int nFile = ant->files.size();
						int count = 0;

						for(int j = 0; j < nFile; ++j)
						{
							if(ant->files[j].streamId == d && J.overlap(ant->files[j]) > 0.0)
							{
								++count;
							}
						}

						DifxDatastreamAllocFiles(dd, count);

						count = 0;

						for(int j = 0; j < nFile; ++j)
						{
							if(ant->files[j].streamId == d && J.overlap(ant->files[j]) > 0.0)
							{
								dd->file[count] = strdup(ant->files[j].filename.c_str());
								++count;
							}
						}
					}
					break;
				case DataSourceMark6:
					{
						// mark6 has both files and vsns
						int nFile = ant->files.size();
						int count = 0;

						for(int j = 0; j < nFile; ++j)
						{
							if(ant->files[j].streamId == d && J.overlap(ant->files[j]) > 0.0)
							{
								++count;
							}
						}

						DifxDatastreamAllocFiles(dd, count);

						count = 0;

						for(int j = 0; j < nFile; ++j)
						{
							if(ant->files[j].streamId == d && J.overlap(ant->files[j]) > 0.0)
							{
								dd->file[count] = strdup(ant->files[j].filename.c_str());
								++count;
							}
						}
						
						string sVSN;
						int nVSN = ant->vsns.size();
						count = 0;

						for(int j = 0; j < nVSN; ++j)
						{
							if(ant->vsns[j].streamId == d && J.overlap(ant->vsns[j]) > 0.0)
							{
								sVSN = ant->vsns[j].filename;
								++count;
							}
						}

						if(!shelf.empty())
						{
							shelf += ",";
						}
						shelf += shelves.getShelf(sVSN);
					}
					break;
				case DataSourceModule:
					{
						int nVSN = ant->vsns.size();
						int count = 0;
						
						DifxDatastreamAllocFiles(dd, 1);
						for(int j = 0; j < nVSN; ++j)
						{
							if(ant->vsns[j].streamId == d && J.overlap(ant->vsns[j]) > 0.0)
							{
								dd->file[0] = strdup(ant->vsns[j].filename.c_str());
								++count;
							}
						}
						if(count > 1)
						{
							std::cerr << "Developer error: got into job creation and antenna " << *a << " datastream " << d << " had " << count << " > 1 module valid in time range of job: " << J << std::endl;

							exit(EXIT_FAILURE);
						}
						if(count == 0)
						{
							std::cerr << "Developer error: got into job creation and antenna " << *a << " datastream " << d << " had no module valid in time range of job: " << J << std::endl;

							exit(EXIT_FAILURE);
						}
						if(!shelf.empty())
						{
							shelf += ",";
						}
						shelf += shelves.getShelf(dd->file[0]);
					}
					break;
				case DataSourceFake:
					break;
				default:
					std::cerr << "Developer error: got into job creation with antenna " << *a << " datastream num " << d << " having unsupported data source " << dataSourceNames[dd->dataSource] << std::endl;

					exit(EXIT_FAILURE);
				}

				++di;
			}

			if(shelf.empty())
			{
				snprintf(difxAntennas[antennaId].shelf, DIFXIO_SHELF_LENGTH, "NONE");
			}
			else
			{
				snprintf(difxAntennas[antennaId].shelf, DIFXIO_SHELF_LENGTH, "%s", shelf.c_str());
			}

			++antennaId;
		}
	}

	return datastreams;
}

static int getBand(vector<pair<int,int> >& bandMap, int fqId)
{
	for(vector<pair<int,int> >::iterator it = bandMap.begin(); it != bandMap.end(); ++it)
	{
		if(it->first == fqId)
		{
			++it->second;

			return it - bandMap.begin();
		}
	}

	// not in list yet, so add
	bandMap.push_back(pair<int,int>(fqId, 1));

	return bandMap.size() - 1;
}

static int getToneSetId(vector<vector<unsigned int> > &toneSets, const vector<unsigned int> &tones)
{
	for(vector<vector<unsigned int> >::const_iterator it = toneSets.begin(); it != toneSets.end(); ++it)
	{
		if(*it == tones)
		{
			return it - toneSets.begin();
		}
	}

	// not in list yet, so add
	toneSets.push_back(tones);

	return toneSets.size() - 1;
}

// The information feeding this function (.vex and .v2d) considers a recorded channel to be one that was intended to be recorded.
// Due to possible thread filtering in VDIF, the number of actually present channels could be less.
// It is this number of present channels that is reported in the .input files as "recorded channels", so some room for confusion here.

// FIXME: the name "startBand" is confusing.

/**
 * Derive multiple parameters from Datastream Format string.
 *
 * Allocate DS freq/band structures, convert VexStream into DS format,
 * convert VexSetup subbands into DS rec bands&pols, update DS rec freqs.
 *
 * @param[in] dsId Datastream ID to update/create if positive number
 * @param[in,out] &freqs A global list of freqs to update via addFreqId()
 * @param[in,out] &toneSets A global vector of tone nr vectors to update via getToneSetId()
 * @param[in] mode The VEX mode with subband details
 * @param[in] antName The VEX antenna name
 * @param[in] startBand (unclear)
 * @param[in] setup The VEX Setup with recorded-channel details
 * @param[in] stream The VEX Stream with data format and recorded-channel details
 * @param[in] corrSetup The Corr Setup with spectral resolution details
 * @param[in] corrParams The Corr Params with global correlation parameters
 */

static unsigned int setFormat(DifxInput *D, int dsId, vector<freq>& freqs, vector<vector<unsigned int> >& toneSets, const VexMode *mode, const string &antName, unsigned int startBand, const VexSetup &setup, const VexStream &stream, const CorrSetup *corrSetup, const CorrParams *corrParams)
{
	vector<pair<int,int> > bandMap;
	unsigned int streamPresentChan = 0;	// "present channel" index

	if(mode == 0)
	{
		cerr << "Developer error: setFormat: mode is NULL" << endl;

		exit(EXIT_FAILURE);
	}

	// just check to make sure antId is legal
	int antId = D->datastream[dsId].antennaId;
	if(antId < 0 || antId >= D->nAntenna)
	{
		cerr << "Developer error: setFormat: antId=" << antId << " while nAntenna=" << D->nAntenna << endl;
		
		exit(EXIT_FAILURE);
	}

	stream.snprintDifxFormatName(D->datastream[dsId].dataFormat, DIFXIO_FORMAT_LENGTH);
	D->datastream[dsId].dataFrameSize = stream.dataFrameSize();
	D->datastream[dsId].quantBits = stream.nBit;
	DifxDatastreamAllocBands(D->datastream + dsId, stream.nPresentChan());

	for(unsigned int i = 0; i < stream.nRecordChan; ++i)
	{
		std::string rxName;

		if(i + startBand >= setup.channels.size())
		{
			cerr << "Error: in setting format parameters, the number of provided channels was less than that expected.  This is for antenna " << antName << ".  In this particular case, " << setup.channels.size() << " were found but " << stream.nRecordChan << " were expected." << endl;
			break;
		}

		const VexChannel *ch = &setup.channels[i + startBand];
		if(ch->subbandId < 0 || ch->subbandId >= static_cast<int>(mode->subbands.size()))
		{
			cerr << "Error: setFormat: index to subband=" << ch->subbandId << " is out of range.  antName=" << antName << " mode=" << mode->defName << endl;

			exit(EXIT_FAILURE);
		}

		const VexIF *vif = setup.getVexIFByLink(ch->ifLink);
		if(vif)
		{
			rxName = vif->rxName;
		}
		else
		{
			rxName = "";
		}

		int setupRecChan = ch->recordChan;
		if(setupRecChan >= 0)
		{
			unsigned int toneSetId, fqId;
			const VexSubband& subband = mode->subbands[ch->subbandId];
			int streamRecChan;

			streamRecChan = setupRecChan - startBand;
			if(streamRecChan < 0 || streamRecChan >= (int)stream.nRecordChan)
			{
				cerr << "Error: setFormat: index to stream record channel=" << streamRecChan << " is out of range.  antName=" << antName << " mode=" << mode->defName << endl;
				cerr << "nRecBand = " << D->datastream[dsId].nRecBand << endl;
				cerr << "startBand = " << startBand << endl;
				cerr << "subband = " << mode->subbands[ch->subbandId] << endl;
				cerr << "nRecordChan = " << stream.nRecordChan << "  i = " << i << "  streamRecChan = " << streamRecChan << "  streamPresentChan = " << streamPresentChan << endl;

				exit(EXIT_FAILURE);
			}

			if(stream.recordChanAbsent(streamRecChan))
			{
				continue;
			}
			
			if(corrParams->v2dMode == V2D_MODE_PROFILE || setup.phaseCalIntervalMHz() == 0)
			{
				// In profile mode don't extract any tones
				toneSetId = 0;
			}
			else
			{
				toneSetId = getToneSetId(toneSets, ch->tones);
			}
			
			fqId = addFreqId(freqs, subband.freq, subband.bandwidth, subband.sideBand, corrSetup->FFTSpecRes, corrSetup->outputSpecRes, /*decimation*/1, /*isZoom*/0, toneSetId, rxName);

			// index into the difxio datastream object arrays is by "present band"
			D->datastream[dsId].recBandFreqId[streamPresentChan] = getBand(bandMap, fqId);
			D->datastream[dsId].recBandPolName[streamPresentChan] = subband.pol;

			// Mark threads to be ignored by changing polarization to lower case
			if(stream.recordChanIgnore(streamRecChan))
			{
				D->datastream[dsId].recBandPolName[streamPresentChan] += ('a'-'A');
			}

			++streamPresentChan;
		}
	}
	DifxDatastreamAllocFreqs(D->datastream + dsId, bandMap.size());
	for(size_t j = 0; j < bandMap.size(); ++j)
	{
		D->datastream[dsId].recFreqId[j]     = bandMap[j].first;
		D->datastream[dsId].recFreqDestId[j] = bandMap[j].first;
		D->datastream[dsId].nRecPol[j]       = bandMap[j].second;

		// With Outputbands not only zooms but also recorded freqs as a whole may map (in)to a different output freq
		if(corrParams->globalOutputbands.size() >= 1 && corrParams->globalOutputbands.back().outputBandwidthMode != OutputBandwidthOff)
		{
			int destFqId = corrParams->globalOutputbands.back().autobands.lookupDestinationFreq(freqs[bandMap[j].first], freqs);
			if (destFqId >= 0)
			{
				D->datastream[dsId].recFreqDestId[j] = destFqId;
			}
		}
	}

	if(streamPresentChan != stream.nPresentChan())
	{
		cerr << "Developer error: setFormat: inconsistent number of present channels.  antName=" << antName << " mode=" << mode->defName << endl;

		exit(EXIT_FAILURE);
	}

	return streamPresentChan;
}

static void populateRuleTable(DifxInput *D, const CorrParams *P)
{
	D->nRule = P->rules.size();
	D->rule = newDifxRuleArray(D->nRule);
	for(int i = 0; i < D->nRule; ++i)
	{
		if(!P->rules[i].scanName.empty())
		{
			for(list<string>::const_iterator s = (P->rules[i].scanName).begin(); s != (P->rules[i].scanName).end(); ++s)
			{
				DifxStringArrayadd(&D->rule[i].scanId, s->c_str(), 0);
			}
		}
		if(!P->rules[i].sourceName.empty())
		{
			for(list<string>::const_iterator s = (P->rules[i].sourceName).begin(); s != (P->rules[i].sourceName).end(); ++s)
			{
				DifxStringArrayadd(&D->rule[i].sourceName, P->getNewSourceName(*s).c_str(), 0);
			}
		}
		if(!P->rules[i].modeName.empty())
		{
			// Note: this is not a problem as the mode selection is being done by vex2difx
			//cerr << "Cannot rule on modeName at this time; ignoring." << endl;
		}
		if(!P->rules[i].calCode.empty())
		{
			if(P->rules[i].calCode.size() > 1)
			{
				cerr << "Cannot handle rules for more than one calCode simultaneously." << endl;

				exit(EXIT_FAILURE);
			}
			D->rule[i].calCode[0] = P->rules[i].calCode.front();
			D->rule[i].calCode[1] = 0;
		}
		if(!P->rules[i].qualifier.empty())
		{
			if(P->rules[i].qualifier.size() > 1)
			{
				cerr << "Cannot handle rules for more than one qualifier simultaneously." << endl;
				
				exit(EXIT_FAILURE);
			}
			D->rule[i].qual = P->rules[i].qualifier.front();
		}
		snprintf(D->rule[i].configName, DIFXIO_NAME_LENGTH, "%s", P->rules[i].corrSetupName.c_str());
	}
}

static void populateFreqTable(DifxInput *D, const vector<freq>& freqs, const vector<vector<unsigned int> > &toneSets)
{
	D->nFreq = freqs.size();
	D->freq = newDifxFreqArray(D->nFreq);
	D->nFreqUnsimplified = D->nFreq;
	D->freqIdRemap = (int *)calloc(D->nFreqUnsimplified+1, sizeof(int));

	for(int f = 0; f < D->nFreq; ++f)
	{
		static int firstChanBWWarning=1;
		DifxFreq *df = D->freq + f;
		double chanBW;

		df->freq = freqs[f].fq/1.0e6;
		df->bw   = freqs[f].bw/1.0e6;
		df->sideband = freqs[f].sideBand;
		df->nChan = static_cast<int>(freqs[f].bw/freqs[f].inputSpecRes + 0.5);	// df->nChan is the number of pre-averaged channels
		df->specAvg = freqs[f].specAvg();
		df->decimation = freqs[f].decimation;
		df->overSamp = 1;	// FIXME: eventually provide this again.
		D->freqIdRemap[f] = f; // 1:1 mapping which simplifyDifxFreqs() will alter later
		snprintf(df->rxName, DIFXIO_RX_NAME_LENGTH, "%s", freqs[f].rxName.c_str());

		chanBW = freqs[f].outputSpecRes*1e-6;
		if(chanBW > 0.51 && firstChanBWWarning)
		{
			firstChanBWWarning = 0;
			cout << "Warning: channel bandwidth is " << chanBW << " MHz, which is larger than the minimum recommended 0.5 MHz.  Consider decreasing the output spectral resolution." << endl;
		}

		if(freqs[f].toneSetId >= toneSets.size())
		{
			cerr << "Developer error: populateFreqTable: toneSetId=" << freqs[f].toneSetId << " nToneSet=" << toneSets.size() << endl;
		}
		const vector<unsigned int> &tones = toneSets[freqs[f].toneSetId];

		if(!tones.empty())
		{
			DifxFreqAllocTones(df, tones.size());
		
			for(unsigned int t = 0; t < tones.size(); ++t)
			{
				df->tone[t] = tones[t];
			}
		}
	}
}

static double populateBaselineTable(DifxInput *D, const CorrParams *P, const CorrSetup *corrSetup, vector<set<int> > blockedfreqids)
{	
	int n1, n2;
	int nPol;
	int a1c[2], a2c[2];
	char a1p[2], a2p[2];
	int nFreq;
	DifxBaseline *bl;
	DifxConfig *config;
	int freqId, destFreqId, altFreqId, blId, configId;
	double lowedgefreq, altlowedgefreq;
	double globalBandwidth = 0;

	// Calculate maximum number of possible baselines based on list of configs
	D->nBaseline = 0;

	for(configId = 0; configId < D->nConfig; ++configId)
	{
		D->nBaseline += D->config[configId].nBaseline;
	}

	D->baseline = newDifxBaselineArray(D->nBaseline);

	bl = D->baseline;
	blId = 0;	// baseline table index

	for(configId = 0; configId < D->nConfig; ++configId)
	{
		config = D->config + configId;

		config->nBaseline = 0;

		// Note: these should loop over antennaIds
		if(P->v2dMode == V2D_MODE_PROFILE)
		{
			// Disable writing of standard autocorrelations
			config->doAutoCorr = 0;

			// Instead, make autocorrlations from scratch
			for(int a1 = 0; a1 < D->nAntenna; ++a1)
			{
				for(int configds1 = 0; configds1 < config->nDatastream; ++configds1)
				{
					int ds1;

					ds1 = config->datastreamId[configds1];

					if(ds1 >= D->nDatastream)
					{
						std::cerr << "Developer error: populateBaselineTable pos 1: ds1=" << ds1 << " , nDatastream=" << D->nDatastream << std::endl;
						std::cerr << "configds1=" << configds1 << " config->nDatastream=" << config->nDatastream << " a1=" << a1 << std::endl;
						std::cerr << "All values of config->datastreamId[] array are:";
						for(int y = 0; y < config->nDatastream; ++y)
						{
							std::cerr << " " << config->datastreamId[y];
						}
						std::cerr << std::endl;

						exit(EXIT_FAILURE);
					}

					if(a1 != D->datastream[ds1].antennaId)
					{
						continue;
					}

					bl->dsA = ds1;
					bl->dsB = ds1;

					// Allocate enough space for worst case possibility
					DifxBaselineAllocFreqs(bl, D->datastream[ds1].nRecFreq + D->datastream[ds1].nZoomFreq);

					nFreq = 0; // this counts the actual number of freqs

					for(int f = 0; f < D->datastream[ds1].nRecFreq; ++f)
					{
						freqId = D->datastream[ds1].recFreqId[f];
						destFreqId = D->datastream[ds1].recFreqDestId[f];

						if(strcmp(D->freq[freqId].rxName, "null") == 0)
						{
							continue;
						}
						if(!corrSetup->correlateFreqId(freqId))
						{
							continue;
						}
						if(!blockedfreqids[a1].empty() && blockedfreqids[a1].find(freqId) != blockedfreqids[a1].end())
						{
							continue;
						}

						if(islower(D->datastream[ds1].recBandPolName[f]))
						{
							continue;
						}

						DifxBaselineAllocPolProds(bl, nFreq, 4);

						n1 = DifxDatastreamGetRecBands(D->datastream+ds1, freqId, a1p, a1c);

						nPol = 0;
						for(int u = 0; u < n1; ++u)
						{
							int v;

							for(v = 0; v < n1; ++v)
							{
								if(corrSetup->doPolar || (a1p[u] == a1p[v] && (corrSetup->onlyPol == ' ' || corrSetup->onlyPol == a1p[u])))
								{
									bl->bandA[nFreq][nPol] = a1c[u];
									bl->bandB[nFreq][nPol] = a1c[v];
									++nPol;
								}
							}
						}
						bl->destFq[nFreq] = destFreqId;
						bl->nPolProd[nFreq] = nPol;

						if(nPol == 0)
						{
							// This deallocates
							DifxBaselineAllocPolProds(bl, nFreq, 0);

							continue;
						}

						++nFreq;
					}
					for(int f = 0; f < D->datastream[ds1].nZoomFreq; ++f)
					{
						freqId = D->datastream[ds1].zoomFreqId[f];
						destFreqId = D->datastream[ds1].zoomFreqDestId[f];

						DifxBaselineAllocPolProds(bl, nFreq, 4);

						n1 = DifxDatastreamGetZoomBands(D->datastream+ds1, freqId, a1p, a1c);

						if(n1 < 0 || n1 > 2)
						{
							std::cerr << "Developer error: populateBaselineTable: n1=" << n1 << " for ds1=" << ds1 << " a1=" << a1 <<" freqId=" << freqId << std::endl;

							exit(EXIT_FAILURE);
						}

						nPol = 0;
						for(int u = 0; u < n1; ++u)
						{
							for(int v = 0; v < n1; ++v)
							{
								if(corrSetup->doPolar || (a1p[u] == a1p[v] && (corrSetup->onlyPol == ' ' || corrSetup->onlyPol == a1p[u])))
								{
									bl->bandA[nFreq][nPol] = D->datastream[ds1].nRecBand + a1c[u];
									bl->bandB[nFreq][nPol] = D->datastream[ds1].nRecBand + a1c[v];
									++nPol;
								}
							}
						}
						bl->destFq[nFreq] = destFreqId;
						bl->nPolProd[nFreq] = nPol;

						if(nPol == 0)
						{
							// This deallocates
							DifxBaselineAllocPolProds(bl, nFreq, 0);

							continue;
						}

						++nFreq;
					}

					bl->nFreq = nFreq;

					if(bl->nFreq > 0)
					{
						config->baselineId[config->nBaseline] = blId;
						++config->nBaseline;
						++bl;
						++blId;
					}
				} // config datastream loop
			} // antenna loop
		} // if profile mode
		else // Not profile mode
		{
			// Beware those who try to follow the logic below!
			// Its actually not to tricky.  There are 8 cases of matching between
			// sub-bands to consider.
			// First all antenna 1 recorded bands to be correlated are considered.  Pairing
			// with sub-bands from antenna 2 is done with the following priority:
			// 1. a recorded band with same sideband
			// 2. a recorded band with opposite sideband
			// 3. a zoom band with same sideband
			// 4. a zoom band with opposite sideband
			//
			// Further down, the antenna 1 zoom bands are considered with priority
			// 5. a recorded band with same sideband
			// 6. a recorded band with opposite sideband
			// 7. a zoom band with same sideband
			// 8. a zoom band with opposite sideband

			// Needless to say, this logic can probably be simplified some, but it seems to work!
			//
			// Finally, we have the case of exhaustiveAutocorrs
			// Here we disable "normal" autocorrelations and instead construct autocorrs as baselines
			// This allows us to grab cross-hand autocorrs when the polarisations are in different baselines

			int enda1 = D->nAntenna-1;
			if(P->exhaustiveAutocorrs)
			{
				enda1 = D->nAntenna;
				config->doAutoCorr = 0;
			}
			for(int a1 = 0; a1 < enda1; ++a1)
			{
				int starta2 = a1 + 1;
				if(P->exhaustiveAutocorrs)
				{
					starta2 = a1;
				}
				for(int a2 = starta2; a2 < D->nAntenna; ++a2)
				{
					for(int configds1 = 0; configds1 < config->nDatastream; ++configds1)
					{
						int ds1;

						ds1 = config->datastreamId[configds1];

						if(ds1 >= D->nDatastream)
						{
							std::cerr << "Developer error: populateBaselineTable pos 2: ds1=" << ds1 << " , nDatastream=" << D->nDatastream << std::endl;
							std::cerr << "configds1=" << configds1 << " config->nDatastream=" << config->nDatastream << " a1=" << a1 << std::endl;

							std::cerr << "All values of config->datastreamId[] array are:";
							for(int y = 0; y < config->nDatastream; ++y)
							{
								std::cerr << " " << config->datastreamId[y];
							}
							std::cerr << std::endl;

							exit(EXIT_FAILURE);
						}

						if(a1 != D->datastream[ds1].antennaId)
						{
							continue;
						}

						for(int configds2 = 0; configds2 < config->nDatastream; ++configds2)
						{
							int ds2;

							ds2 = config->datastreamId[configds2];
							if(a2 != D->datastream[ds2].antennaId)
							{
								continue;
							}
							
							// Excape if this baseline is not requested
							if(!P->useBaseline(D->antenna[a1].name, D->antenna[a2].name))
							{
								continue;
							}

							if(config->nBaseline >= D->nBaseline)
							{
								std::cerr << "Developer error: populateBaselineTable: trying to add " << config->nBaseline+1 << "th baseline for DS " << ds1 << " x " << ds2 << ", but pre-allocated only D->nBaseline=" << D->nBaseline << ": skipping!" << std::endl;
								continue;
							}

							bl->dsA = ds1;
							bl->dsB = ds2;

							// Allocate enough space for worst case possibility
							DifxBaselineAllocFreqs(bl, D->datastream[ds1].nRecFreq + D->datastream[ds1].nZoomFreq);

							nFreq = 0; // this counts the actual number of freqs

							for(int f = 0; f < D->datastream[ds1].nRecFreq; ++f)
							{
								bool zoom2 = false;	// did antenna 2 zoom band make match?

								freqId = D->datastream[ds1].recFreqId[f];
								destFreqId = D->datastream[ds1].recFreqDestId[f];

								if(strcmp(D->freq[freqId].rxName, "null") == 0)
								{
									continue;
								}
								if(!corrSetup->correlateFreqId(freqId))
								{
									continue;
								}
								if(!blockedfreqids[a1].empty() && blockedfreqids[a1].find(freqId) != blockedfreqids[a1].end())
								{
									continue;
								}

								if(islower(D->datastream[ds1].recBandPolName[f]))
								{
									continue;
								}

								DifxBaselineAllocPolProds(bl, nFreq, 4);

								n1 = DifxDatastreamGetRecBands(D->datastream+ds1, freqId, a1p, a1c);
								n2 = DifxDatastreamGetRecBands(D->datastream+ds2, freqId, a2p, a2c);

								lowedgefreq = D->freq[freqId].freq;
								if(D->freq[freqId].sideband == 'L')
								{
									lowedgefreq -= D->freq[freqId].bw;
								}

								if(n2 == 0)
								{
									//look for another freqId which matches band but is opposite sideband
									for(int f2 = 0; f2 < D->datastream[ds2].nRecFreq; ++f2)
									{
										altFreqId = D->datastream[ds2].recFreqId[f2];
										altlowedgefreq = D->freq[altFreqId].freq;
										if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
										{
											continue;
										}
										if(islower(D->datastream[ds2].recBandPolName[f2]))
										{
											continue;
										}

										if(D->freq[altFreqId].sideband == 'L')
										{
											altlowedgefreq -= D->freq[altFreqId].bw;
										}
										if(altlowedgefreq     == lowedgefreq &&
										   D->freq[freqId].bw == D->freq[altFreqId].bw)
										{
											n2 = DifxDatastreamGetRecBands(D->datastream+ds2, altFreqId, a2p, a2c);
										}
									}
								}
								if(n2 == 0)
								{
									//still no dice? Try the zoom bands of datastream 2 with the same sideband
									for(int f2 = 0; f2 < D->datastream[ds2].nZoomFreq; ++f2)
									{
										altFreqId = D->datastream[ds2].zoomFreqId[f2];
										if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
										{
											continue;
										}
										if(D->freq[freqId].freq == D->freq[altFreqId].freq &&
										   D->freq[freqId].bw   == D->freq[altFreqId].bw &&
										   D->freq[freqId].sideband == D->freq[altFreqId].sideband)
										{
											n2 = DifxDatastreamGetZoomBands(D->datastream+ds2, altFreqId, a2p, a2c);
											zoom2 = true;
										}
									}
								}
								if(n2 == 0)
								{
									//still no dice? Try the opposite sidebands of zoom bands of datastream 2
									for(int f2 = 0; f2 < D->datastream[ds2].nZoomFreq; ++f2)
									{
										altFreqId = D->datastream[ds2].zoomFreqId[f2];
										altlowedgefreq = D->freq[altFreqId].freq;
										if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
										{
											continue;
										}
										if(D->freq[altFreqId].sideband == 'L')
										{
											altlowedgefreq -= D->freq[altFreqId].bw;
										}
										if(altlowedgefreq == lowedgefreq &&
										   D->freq[freqId].bw == D->freq[altFreqId].bw)
										{
											n2 = DifxDatastreamGetZoomBands(D->datastream+ds2, altFreqId, a2p, a2c);
											zoom2 = true;
										}
									}
								}

								nPol = 0;
								for(int u = 0; u < n1; ++u)
								{
									for(int v = 0; v < n2; ++v)
									{
										if(corrSetup->doPolar || (a1p[u] == a2p[v] && (corrSetup->onlyPol == ' ' || corrSetup->onlyPol == a1p[u])))
										{
											bl->bandA[nFreq][nPol] = a1c[u];
											bl->bandB[nFreq][nPol] = a2c[v];
											if(zoom2)
											{
												bl->bandB[nFreq][nPol] += D->datastream[ds2].nRecBand;
											}
											++nPol;
										}
									}
								}
								bl->destFq[nFreq] = destFreqId;
								bl->nPolProd[nFreq] = nPol;

								if(nPol == 0)
								{
									// This deallocates
									DifxBaselineAllocPolProds(bl, nFreq, 0);

									continue;
								}

								if(globalBandwidth == 0)
								{
									globalBandwidth = D->freq[bl->destFq[nFreq]].bw;
								}
								else if(globalBandwidth > 0)
								{
									if(globalBandwidth != D->freq[bl->destFq[nFreq]].bw)
									{
										globalBandwidth = -1;
									}
								}

								++nFreq;
							}

							for(int f = 0; f < D->datastream[ds1].nZoomFreq; ++f)
							{
								bool zoom2 = false;	// did antenna 2 zoom band make match?

								n2 = 0;

								freqId = D->datastream[ds1].zoomFreqId[f];
								destFreqId = D->datastream[ds1].zoomFreqDestId[f];

								// Unlike for recbands, don't query corrSetup->correlateFreqId as all defined zoom bands should be correlated

								DifxBaselineAllocPolProds(bl, nFreq, 4);

								n1 = DifxDatastreamGetZoomBands(D->datastream+ds1, freqId, a1p, a1c);

								lowedgefreq = D->freq[freqId].freq;
								if(D->freq[freqId].sideband == 'L')
								{
									lowedgefreq -= D->freq[freqId].bw;
								}

								for(int f2 = 0; f2 < D->datastream[ds2].nRecFreq; ++f2)
								{
									altFreqId = D->datastream[ds2].recFreqId[f2];
									if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
									{
										continue;
									}
									if(D->freq[freqId].freq == D->freq[altFreqId].freq &&
									   D->freq[freqId].bw   == D->freq[altFreqId].bw &&
									   D->freq[altFreqId].sideband == 'U')
									{
										n2 = DifxDatastreamGetRecBands(D->datastream+ds2, altFreqId, a2p, a2c);
									}
								}

								if(n2 == 0)
								{
									//look for another freqId which matches band but is opposite sideband
									for(int f2 = 0; f2 < D->datastream[ds2].nRecFreq; ++f2)
									{
										altFreqId = D->datastream[ds2].recFreqId[f2];
										altlowedgefreq = D->freq[altFreqId].freq;
										if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
										{
											continue;
										}
										if(D->freq[altFreqId].sideband == 'L')
										{
											altlowedgefreq -= D->freq[altFreqId].bw;
										}
										if(altlowedgefreq     == lowedgefreq &&
										   D->freq[freqId].bw == D->freq[altFreqId].bw)
										{
											n2 = DifxDatastreamGetRecBands(D->datastream+ds2, altFreqId, a2p, a2c);
										}
									}
								}
								if(n2 == 0)
								{
									n2 = DifxDatastreamGetZoomBands(D->datastream+ds2, freqId, a2p, a2c);
									if(n2 > 0)
									{
										zoom2 = true;
									}
								}
								if(n2 == 0)
								{
									//still no dice? Try the opposite sidebands of zoom bands of datastream 2
									for(int f2 = 0; f2 < D->datastream[ds2].nZoomFreq; ++f2)
									{
										altFreqId = D->datastream[ds2].zoomFreqId[f2];
										altlowedgefreq = D->freq[altFreqId].freq;
										if(!blockedfreqids[a2].empty() && blockedfreqids[a2].find(altFreqId) != blockedfreqids[a2].end())
										{
											continue;
										}
										if(D->freq[altFreqId].sideband == 'L')
										{
											altlowedgefreq -= D->freq[altFreqId].bw;
										}
										if(altlowedgefreq == lowedgefreq &&
										   D->freq[freqId].bw == D->freq[altFreqId].bw)
										{
											n2 = DifxDatastreamGetZoomBands(D->datastream+ds2, altFreqId, a2p, a2c);
											zoom2 = true;
										}
									}
								}

								nPol = 0;
								for(int u = 0; u < n1; ++u)
								{
									for(int v = 0; v < n2; ++v)
									{
										if(corrSetup->doPolar || (a1p[u] == a2p[v] && (corrSetup->onlyPol == ' ' || corrSetup->onlyPol == a1p[u])))
										{
											bl->bandA[nFreq][nPol] = D->datastream[ds1].nRecBand + a1c[u];
											bl->bandB[nFreq][nPol] = a2c[v];
											if(zoom2)
											{
												bl->bandB[nFreq][nPol] += D->datastream[ds2].nRecBand;
											}
											++nPol;
										}
									}
								}
								bl->destFq[nFreq] = destFreqId;
								bl->nPolProd[nFreq] = nPol;

								if(nPol == 0)
								{
									// This deallocates
									DifxBaselineAllocPolProds(bl, nFreq, 0);

									continue;
								}

								if(globalBandwidth == 0)
								{
									globalBandwidth = D->freq[bl->destFq[nFreq]].bw;
								}
								else if(globalBandwidth > 0)
								{
									if(globalBandwidth != D->freq[bl->destFq[nFreq]].bw)
									{
										globalBandwidth = -1;
									}
								}

								++nFreq;
							}
			
							bl->nFreq = nFreq;
			
							if(bl->nFreq > 0)
							{
								config->baselineId[config->nBaseline] = blId;
								++config->nBaseline;
								++bl;
								++blId;
							}
						} // config datastream 2 loop
					} // config datastream 1 loop
				} // ant 2 loop
			} // ant 1 loop
		}
		config->baselineId[config->nBaseline] = -1;
	}

	// set actual number of baselines
	D->nBaseline = blId;

	return globalBandwidth;
}

static void populateEOPTable(DifxInput *D, const vector<VexEOP>& E)
{
	unsigned int nEOP;

	nEOP = E.size();
	D->nEOP = nEOP;
	D->eop = newDifxEOPArray(D->nEOP);

	for(unsigned int e = 0; e < nEOP; ++e)
	{
		D->eop[e].mjd = static_cast<int>(E[e].mjd);
		D->eop[e].tai_utc = static_cast<int>(E[e].tai_utc);
		D->eop[e].ut1_utc = E[e].ut1_utc;
		D->eop[e].xPole = E[e].xPole*180.0*3600.0/M_PI;
		D->eop[e].yPole = E[e].yPole*180.0*3600.0/M_PI;
	}
}

static int getConfigIndex(vector<pair<string,string> >& configs, DifxInput *D, const VexData *V, const CorrParams *P, const VexScan *S)
{
	int nConfig, nFFTsPerIntegration, nSubintsPerIntegration;
	int max5div, max2div;
	int fftDurNS;
	DifxConfig *config;
	const CorrSetup *corrSetup;
	const VexMode *mode;
	string configName;
	double floatReadTimeNS, floatFFTDurNS, floatSubintDurNS;
	double msgSize, dataRate, readSize;
	int64_t tintNS;
	int nDatastream;

	const std::string &corrSetupName = P->findSetup(S->defName, S->sourceDefName, S->modeDefName);
	corrSetup = P->getCorrSetup(corrSetupName);
	if(corrSetup == 0)
	{
		cerr << "Error: correlator setup[" << corrSetupName << "] == 0" << endl;
		
		exit(EXIT_FAILURE);
	}

	mode = V->getModeByDefName(S->modeDefName);
	if(mode == 0)
	{
		cerr << "Error: mode[" << S->modeDefName << "] == 0" << endl;
		
		exit(EXIT_FAILURE);
	}

	nConfig = configs.size();
	for(int i = 0; i < nConfig; ++i)
	{
		if(configs[i].first  == S->modeDefName &&
		   configs[i].second == corrSetupName)
		{
			return i;
		}
	}

	// get worst case datastream count
	nDatastream = mode->nStream();
	configName = S->modeDefName + string("_") + corrSetupName;

	configs.push_back(pair<string,string>(S->modeDefName, corrSetupName));
	config = D->config + nConfig;
	snprintf(config->name, DIFXIO_NAME_LENGTH, "%s", configName.c_str());
	for(int i = 0; i < D->nRule; ++i)
	{
		if(corrSetupName == D->rule[i].configName)
		{
			snprintf(D->rule[i].configName, DIFXIO_NAME_LENGTH, "%s", configName.c_str());
		}
	}
	config->tInt = corrSetup->tInt;
	tintNS = static_cast<int64_t>(1e9*corrSetup->tInt + 0.5);
	floatFFTDurNS = 1000000000.0/corrSetup->FFTSpecRes;
	fftDurNS = static_cast<int>(floatFFTDurNS);
	dataRate = mode->getHighestSampleRate()*mode->getBits()*mode->subbands.size();
	nFFTsPerIntegration = static_cast<int>(1e9*corrSetup->tInt/floatFFTDurNS + 0.5);

	//first test how big a single FFT is - if it is too big, fail with a warning and a suggestion
	if(floatFFTDurNS > (1LL<<31) - 1)
	{
		cerr << "A single FFT is too long (" << floatFFTDurNS << " ns)!" << endl;
		cerr << "The maximum duration of an FFT is 2^31 - 1 nanoseconds" << endl;
		cerr << "Please reduce nFFTChan accordingly" << endl;

		exit(EXIT_FAILURE);
	}

	if(corrSetup->subintNS > 0) //This is relatively easy - just see if the provided values are reasonable
	{
		config->subintNS = corrSetup->subintNS;
		if(config->subintNS % fftDurNS != 0)
		{
			cerr << "Error: The provided subintNS (" << config->subintNS << ") is not an integer multiple of the FFT duration (" << fftDurNS << ")" << endl;
			cerr << "You should adjust your subint time, or leave subint unset and vex2difx will set it for you" << endl;

			exit(EXIT_FAILURE);
		}
		if(tintNS % config->subintNS != 0)
		{
			if(P->tweakIntTime)
			{
				//leave it - we will adjust the int time later
			}
			else
			{
				cerr << "Error: The provided tInt (" << config->tInt << ") is not an integer multiple of the provided subint (" << corrSetup->subintNS/1.0e9 << ")" << endl;
				cerr << "You should adjust your subint and/or int time, or leave subint unset and vex2difx will set it for you" << endl;

				exit(EXIT_FAILURE);
			}
		}
	}
	else //first try to set a reasonable subintNS
	{
		int64_t nscounter;

		nFFTsPerIntegration = static_cast<int>(1e9*corrSetup->tInt/floatFFTDurNS + 0.5);

		// check that integration time is an integer number of FFTs
		if(fabs(1e9*corrSetup->tInt/floatFFTDurNS - nFFTsPerIntegration) > 1e-9)
		{
			//it is not - no chance of a reasonable subintNS unless we tweak intTime
			if(P->tweakIntTime)
			{
				cout << "Requested integration time (" << corrSetup->tInt << ") is not an integer multiple of the FFT time (" << floatFFTDurNS << " ns)" << endl;
				cout << "Will attempt to nudge the int time to a more appropriate value" << endl;
			}
			else
			{
				cerr << "Integration time is not an integer number of FFTs!" << endl;
				cerr << "Please change tInt to a multiple of " << floatFFTDurNS << " nanoseconds, or set tweakIntTime = true" << endl;
				cerr << "Try to use a number of FFTs which is factorable entirely by 2 and/or 5" << endl;
			
				exit(EXIT_FAILURE);
			}
		}

		config->subintNS = fftDurNS;
		msgSize = (config->subintNS*1.0e-9)*dataRate/8.0;
		readSize = msgSize*D->dataBufferFactor/D->nDataSegments;
		if(readSize > P->maxReadSize)
		{
			cerr << "Warning: a single FFT gives a read size of " << readSize << " bytes" << endl;
			cerr << "The maximum read size has been set (or defaulted) to " << P->maxReadSize << endl;
			cerr << "There are known problems with Mark5 module playback at large read sizes" << endl;
			cerr << "If you want to try with the large read size, set maxReadSize in the global area of the .v2d file" << endl;

			exit(EXIT_FAILURE);
		}

		max5div = 0;
		max2div = 0;
		nscounter = tintNS/5;
	 	while(nscounter > 0 && fabs(nscounter/floatFFTDurNS - static_cast<int>(nscounter/floatFFTDurNS + 0.5)) < 1e-9)
		{
			nscounter /= 5;
			++max5div;
		}

		nscounter = tintNS/2;
		while(nscounter > 0 && fabs(nscounter/floatFFTDurNS - static_cast<int>(nscounter/floatFFTDurNS + 0.5)) < 1e-9)
		{
			nscounter /= 2;
			++max2div;
		}

		for(int i = max2div; i >= 0; --i)
		{
			for(int j = max5div; j >= 0; --j)
			{
				int divisor = 1;
				for(int k = 0; k < i; ++k)
				{
					divisor *= 2;
				}
				for(int l = 0; l < j; ++l)
				{
					divisor *= 5;
				}

				int64_t testsubintNS = tintNS / divisor;
				msgSize = (testsubintNS*1.0e-9)*dataRate/8.0;
				readSize = msgSize*D->dataBufferFactor/D->nDataSegments;
				if(readSize > P->minReadSize && readSize < P->maxReadSize &&
					testsubintNS <= 1020000000 && testsubintNS > config->subintNS &&
					fabs(testsubintNS/floatFFTDurNS - static_cast<int>(testsubintNS/floatFFTDurNS + 0.5)) < 1e-9)
				{
					config->subintNS = testsubintNS;
				}
			}
		}
		//refuse to run if the generated read size is too small
		msgSize = (config->subintNS*1.0e-9)*dataRate/8.0;
		readSize = msgSize*D->dataBufferFactor/D->nDataSegments;
		if(readSize < P->minReadSize)
		{
			if(P->tweakIntTime)
			{
				while(((config->subintNS*1.0e-9)*dataRate/8.0)*(D->dataBufferFactor/D->nDataSegments) < P->minReadSize && config->subintNS <= 510000000)
				{
					config->subintNS *= 2;
				}
			}
			else
			{
				cerr << "Automatic subint duration selection generated " << config->subintNS << " nanoseconds" << endl;
				cerr << "This leads to a read size of " << readSize << " B" << endl;
				cerr << "The minimum read size was set or defaulted to " << P->minReadSize << " B" << endl;
				cerr << "Either decrease minReadSize (which may lead to slow correlation) or explicitly set subintNS" << endl;
				cerr << "You may find it advantageous to tweak the tInt to a more power-of-2 friendly value" << endl;

				exit(EXIT_FAILURE);
			}
		}
	}

	// change nDataSegments if needed to get send sizes under 2^31 nanoseconds
	floatReadTimeNS = static_cast<double>(config->subintNS)*D->dataBufferFactor/D->nDataSegments;
	if(floatReadTimeNS > 2140000000.0)
	{
		int f = static_cast<int>(2140000000.0/config->subintNS);
		if(f < 1)
		{
			cerr << "Error: There is no way to change dataBufferFactor to keep send sizes below 2^31 seconds" << endl;

			exit(EXIT_FAILURE);
		}
		cout << "Note: changing nDataSegments from " << D->nDataSegments << " to " << (D->dataBufferFactor/f) << " in order to keep data send sizes below 2.14 seconds" << endl;
		D->nDataSegments = D->dataBufferFactor/f;
		msgSize = (config->subintNS*1.0e-9)*dataRate/8.0;
		readSize = msgSize*D->dataBufferFactor/D->nDataSegments;
		if(readSize < P->minReadSize)
		{
			cout << "This has lead to a read size " << readSize << " smaller than the provided guideline " << P->minReadSize << ": correlation may run more slowly" << endl;
			cout << "But probably this is a low data rate experiment, so it won't matter" << endl;
		}
	}

	//now check that the int time is an integer number of subints, and tweak if necessary
	floatSubintDurNS = (double)config->subintNS;
	nSubintsPerIntegration = static_cast<int>(1e9*corrSetup->tInt/floatSubintDurNS + 0.5);
	if(fabs(1e9*corrSetup->tInt/floatSubintDurNS - nSubintsPerIntegration) > 1e-9)
	{
		if(P->tweakIntTime)
		{
			int best5div = 0;
			int best2div = 0;
			double bestfractionaldiff;

			cout << "The provided tInt (" << config->tInt << ") is not an integer multiple of the subint (" << floatSubintDurNS/1.0e9 << ")" << endl;
			max5div = static_cast<int>(config->tInt/(5*floatSubintDurNS/1.0e9)) + 1;
			max2div = static_cast<int>(config->tInt/(2*floatSubintDurNS/1.0e9)) + 1;
			bestfractionaldiff = fabs(1.0 - (floatSubintDurNS/1.0e9)/config->tInt);
			for(int i = 0; i <= max5div; ++i)
			{
				for(int j = 0; j <= max5div; ++j)
				{
					double test_tint = (floatSubintDurNS/1.0e9)*pow(5.0,i)*pow(2.0,j);
					if(fabs(1.0 - (test_tint)/config->tInt) < bestfractionaldiff)
					{
						bestfractionaldiff = fabs(1.0 - (test_tint)/config->tInt);
						best5div = i;
						best2div = j;
					}
				}
			}
			cout << "tInt has been updated from " << config->tInt << " to " << (floatSubintDurNS/1.0e9)*pow(5.0,best5div)*pow(2.0,best2div) << endl;
			cout << "(You could also try modifying the subintNS if you want to try harder to get your desired integration time)" << endl;
			config->tInt = (floatSubintDurNS/1.0e9)*pow(5.0,best5div)*pow(2.0,best2div);
		}
		else
		{
			cerr << "Error: The provided tInt (" << config->tInt << ") is not an integer multiple of the subint (" << floatSubintDurNS/1.0e9 << ")" << endl;
			cerr << "Either change your integration time to a more friendly value, or tweak number of channels or subint time, or set tweakIntTime = true" << endl;

			exit(EXIT_FAILURE);
		}
	}

	config->guardNS = corrSetup->guardNS;
	config->fringeRotOrder = corrSetup->fringeRotOrder;
	config->strideLength = corrSetup->strideLength;
	config->xmacLength = corrSetup->xmacLength;
	config->numBufferedFFTs = corrSetup->numBufferedFFTs;

#warning "FIXME: is setting pulsarId = -1 correct"
	config->pulsarId = -1;
	config->doPolar = corrSetup->doPolar;
	config->doAutoCorr = 1;
	config->nAntenna = D->nAntenna;
	config->nDatastream = nDatastream;
	if(P->v2dMode == V2D_MODE_PROFILE)
	{
		config->nBaseline = config->nDatastream;
	}
	else
	{
		config->nBaseline = nDatastream*(nDatastream-1)/2;	// this is a worst case (but typical) scenario; may shrink later.
									// FIXME: it seems the shrinking causes seg faults.
		if(P->exhaustiveAutocorrs)
		{
			config->nBaseline += 2*nDatastream;		// worst case if every datastream has a corresponding partner
		}

		config->nBaseline *= 2; // in case some freq-pols appear redundantly in two datastreams
	}

	//if guardNS was set to negative value, change it to the right amount to allow for
	//adjustment to get to an integer NS + geometric rate slippage (assumes Earth-based antenna)
	//Note: if not set explicitly, zero will be passed to mpifxcorr where it will do the calculation
	if(config->guardNS < 0)
	{
		config->guardNS = calculateWorstcaseGuardNS(mode->getLowestSampleRate(), config->subintNS, mode->getMinBits(), mode->getMinSubbands());
	}
	
	DifxConfigAllocDatastreamIds(config, config->nDatastream, D->nConfig*config->nDatastream);
	DifxConfigAllocBaselineIds(config, config->nBaseline, nConfig*config->nBaseline);

	config->nPol = mode->getPols(config->pol);
	config->quantBits = mode->getBits();

	return nConfig;
}

// For a zoom freq provideable by multiple recfreqs, return recfreq id for which the zoom is farthest from the edes, or return -1 on error/not-found
// TODO: the choice is still suboptimal
static int bestMatchingFreq(const ZoomFreq &zoomfreq, const std::vector<int> matchingFreqs, const DifxDatastream *dd, const vector<freq> &freqs)
{
	const double upper_zoombandedge = zoomfreq.frequency + zoomfreq.bandwidth;
	const double lower_zoombandedge = zoomfreq.frequency;

	if(0)
	{
		std::cout << "lower_zoombandedge = " << lower_zoombandedge << std::endl;
		std::cout << "upper_zoombandedge = " << upper_zoombandedge << std::endl;
		std::cout << "bw_zoom  MHz       = " << (upper_zoombandedge-lower_zoombandedge)*1e-6 << std::endl;
		std::cout << "matchingFreqs      = [";
		std::copy(matchingFreqs.begin(), matchingFreqs.end(), std::ostream_iterator<int>(std::cout, " "));
		std::cout << "], length " << matchingFreqs.size() << endl;
	}

	if(matchingFreqs.size() <= 0)
	{
		return -1;
	}

	if(matchingFreqs.size() == 1)
	{
		return matchingFreqs[0];
	}

// TODO: can simplify into a single-pass search
	std::vector<freq> f_vec;
	std::vector<double> input_bands_upper_edge;
	std::vector<double> input_bands_lower_edge;
	for(size_t ii = 0; ii < matchingFreqs.size(); ++ii)
	{
		f_vec.push_back(freqs[dd->recFreqId[matchingFreqs[ii]]]);
		if(f_vec[ii].sideBand == 'U')
		{
			input_bands_upper_edge.push_back(f_vec[ii].fq + f_vec[ii].bw);
			input_bands_lower_edge.push_back(f_vec[ii].fq);
		}
		else if(f_vec[ii].sideBand == 'L')
		{
			input_bands_upper_edge.push_back(f_vec[ii].fq);
			input_bands_lower_edge.push_back(f_vec[ii].fq - f_vec[ii].bw);
		}
	}

	// Find the best matching input band by maximizing the distance from a band edge
	double edge_dist_l = 0.0;
	double edge_dist_u = 0.0;
	std::vector<int>::size_type best_input_band_index = 0;
	double ed_min = 0.0;
	std::vector<double> ed1_vec;
	std::vector<double> ed2_vec;
	std::vector<double> input_center;
	std::vector<double> input_centers;
	for(size_t jj = 0; jj < matchingFreqs.size(); ++jj)
	{
		edge_dist_l = abs(lower_zoombandedge - input_bands_lower_edge[jj]);
		edge_dist_u = abs(upper_zoombandedge - input_bands_upper_edge[jj]);
		if(0)
		{
			std::cout << "input_bands_lower_edge[" << jj << "] = " << input_bands_lower_edge[jj] << std::endl;
			std::cout << "input_bands_upper_edge[" << jj  << "] = " << input_bands_upper_edge[jj] << std::endl;
			std::cout << "edge_dist_l = " << edge_dist_l << std::endl;
			std::cout << "edge_dist_u = " << edge_dist_u << std::endl;
		}
		if(0 == jj)
		{
			ed_min = min(edge_dist_l,edge_dist_u);
			best_input_band_index = jj;
		}
		if (edge_dist_l > ed_min or edge_dist_u > ed_min)
		{
			ed_min = min(edge_dist_l,edge_dist_u);
			best_input_band_index = jj;
		}
	}

	if(0)
	{
		std::cout << "ed_min = " << ed_min << std::endl;
		std::cout << "best_input_band_index = " << best_input_band_index << " corresponding to freq = " << matchingFreqs[best_input_band_index] << std::endl;
	}

	return matchingFreqs[best_input_band_index];
}

static int fixDatastreamTable(DifxInput *D)
{
	int nFix = 0;
	int addedFreq = -1;

	if(D && D->nDatastream > 0)
	{
		int dd;
		int power2;		/* power of 2 that is >= nRecBand */

		for(dd = 0; dd < D->nDatastream; ++dd)
		{
			int r;
			DifxDatastream *ds;

			ds = D->datastream + dd;

			for(r = 0; r < ds->nRecFreq; ++r)
			{
				if(islower(ds->recBandPolName[r]))
				{
					ds->recBandPolName[r] += ('A' - 'a');
					++nFix;
				}
			}

			/* In case < 2^n channels were recorded, pad the arrays with info that can easily identified and safely ignored */
			for(power2 = 1; power2 < ds->nRecBand; power2 *= 2) ;
			if(ds->nRecBand < power2)
			{
				int delta;	/* additional fake refFreq and recBand entries to make */
				int N;

				if(addedFreq < 0)
				{
					DifxFreq *df;

					D->freq = (DifxFreq *)realloc(D->freq, (D->nFreq + 1)*sizeof(DifxFreq));
					
					df = D->freq + D->nFreq;
					memcpy(df, D->freq, sizeof(DifxFreq));		// copy much of this channel from [0]
					df->freq = 0.999;
					df->sideband = 'U';
					df->nTone = 0;
					df->tone = 0;
					strcpy(df->rxName, "null");

					addedFreq = D->nFreq;
					++D->nFreq;
				}

				delta = power2 - ds->nRecBand;

				// FIXME: the below would be tidier if relocated into difxio; padDifxDatastreamFreq(DifxDatastream* srcdst, int deltaNumRecbands)
				// NB: manually keep the below realloc()'s in sync with any changes made to difx_input.h struct DifxDatastream
				N = ds->nRecFreq + 1;
				ds->clockOffset = (double *)realloc(ds->clockOffset, N*sizeof(double));
				ds->clockOffsetDelta = (double *)realloc(ds->clockOffsetDelta, N*sizeof(double));
				ds->phaseOffset = (double *)realloc(ds->phaseOffset, N*sizeof(double));
				ds->freqOffset = (double *)realloc(ds->freqOffset, N*sizeof(double));
				ds->nRecPol = (int *)realloc(ds->nRecPol, N*sizeof(int));
				ds->recFreqId = (int *)realloc(ds->recFreqId, N*sizeof(int));
				ds->recFreqDestId = (int *)realloc(ds->recFreqDestId, N*sizeof(int));
				ds->clockOffset[N-1] = 0.0;
				ds->clockOffsetDelta[N-1] = 0.0;
				ds->phaseOffset[N-1] = 0.0;
				ds->nRecPol[N-1] = delta;
				ds->recFreqId[N-1] = addedFreq;
				ds->recFreqDestId[N-1] = addedFreq;
				ds->nRecFreq = N;

				N = ds->nRecBand + delta;
				ds->recBandFreqId = (int *)realloc(ds->recBandFreqId, N*sizeof(int));
				ds->recBandPolName = (char *)realloc(ds->recBandPolName, N*sizeof(char));
				for(r = ds->nRecBand; r < N; ++r)
				{
					ds->recBandFreqId[r] = ds->nRecFreq - 1;
					ds->recBandPolName[r] = 'R';
				}
				ds->nRecBand = N;

				for(r = 0; r < delta; ++r)
				{
					char *p;

					p = ds->dataFormat + strlen(ds->dataFormat);
					sprintf(p, ":%d", 999-r);
				}
			}
		}
	}

	return nFix;
}

static void populateSourceTable(DifxInput *D, const Job& J, const VexData *V, set<string> &sourceSet)
{
	J.getCorrelationSourceSet(V, sourceSet);

	allocateSourceTable(D, sourceSet.size());	// returns empty allocated array (e.g., D->nSource is set to 0)

	for(set<string>::const_iterator it = sourceSet.begin(); it != sourceSet.end(); ++it)
	{
		const VexSource *S = V->getSourceBySourceName(*it);
		if(!S)
		{
			std::cerr << "Developer error: populateSourceTable(): soruce " << *it << " not found in VexData" << std::endl;

			exit(EXIT_FAILURE);
		}
		
		snprintf(D->source[D->nSource].name, DIFXIO_NAME_LENGTH, "%s", S->defName.c_str());
		D->source[D->nSource].ra = S->ra;
		D->source[D->nSource].dec = S->dec;
		D->source[D->nSource].calCode[0] = S->calCode;
		
		++D->nSource;
	}
}

static void populateScanTable(DifxInput *D, const Job& J, const VexData *V, const CorrParams *P, const CorrSetup *corrSetup, vector<pair<string,string> > &configs, unsigned int &maxScanPhaseCentres)
{
	maxScanPhaseCentres = 0;

	D->nScan = J.scans.size();
	D->scan = newDifxScanArray(D->nScan);
	DifxScan *difxScan = D->scan;
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); ++si, ++difxScan)
	{
		int fftDurNS;

		const VexScan *vexScan = V->getScanByDefName(*si);
		if(!vexScan)
		{
			cerr << "Developer error: scan[" << *si << "] not found!  This cannot be!" << endl;
			
			exit(EXIT_FAILURE);
		}

		// Determine interval where scan and job overlap
		Interval scanInterval(*vexScan);
		scanInterval.logicalAnd(J);

		if(vexScan->phaseCenters.size() > maxScanPhaseCentres)
		{
			maxScanPhaseCentres = vexScan->phaseCenters.size();
		}

		difxScan->pointingCentreSrc = DifxInputGetSourceId(D, vexScan->sourceDefName.c_str());
		if(difxScan->pointingCentreSrc < 0)
		{
			std::cerr << "Developer error: DifxInputGetSourceId(D, " << vexScan->sourceDefName << ") returned " << difxScan->pointingCentreSrc << " for pointing center." << std::endl;

			exit(EXIT_FAILURE);
		}

		for(unsigned int pc = 0; pc < vexScan->phaseCenters.size(); ++pc)
		{
			difxScan->phsCentreSrcs[pc] = DifxInputGetSourceId(D, vexScan->phaseCenters[pc].c_str());
			if(difxScan->phsCentreSrcs[pc] < 0)
			{
				std::cerr << "Developer error: DifxInputGetSourceId(D, " << vexScan->phaseCenters[pc] << ") returned " << difxScan->phsCentreSrcs[pc] << " for phase center source index " << pc << "." << std::endl;

				exit(EXIT_FAILURE);
			}
		}
		difxScan->nPhaseCentres = vexScan->phaseCenters.size();

		difxScan->mjdStart = scanInterval.mjdStart;
		difxScan->mjdEnd = scanInterval.mjdStop;
		difxScan->startSeconds = static_cast<int>((scanInterval.mjdStart - J.mjdStart)*86400.0 + 0.01);
		difxScan->durSeconds = static_cast<int>(scanInterval.duration_seconds() + 0.01);
		if(difxScan->durSeconds == 0)
		{
			difxScan->durSeconds = 1;
		}
		difxScan->configId = getConfigIndex(configs, D, V, P, vexScan);
		difxScan->maxNSBetweenUVShifts = corrSetup->maxNSBetweenUVShifts;
		fftDurNS = static_cast<int>(1000000000.0/corrSetup->FFTSpecRes);
		if(corrSetup->maxNSBetweenACAvg > 0)
		{
			difxScan->maxNSBetweenACAvg = corrSetup->maxNSBetweenACAvg;
		}
		else
		{
			difxScan->maxNSBetweenACAvg = defaultMaxNSBetweenACAvg;
		}
		if(corrSetup->numBufferedFFTs*fftDurNS > difxScan->maxNSBetweenACAvg)
		{
			if(corrSetup->maxNSBetweenACAvg != 0)	// Only print warning if explicitly overriding user value
			{
				cout << "Adjusting maxNSBetweenACAvg since the number of buffered FFTs (";
				cout << corrSetup->numBufferedFFTs << ") gives a duration of ";
				cout << corrSetup->numBufferedFFTs*fftDurNS << ", longer that that specified (";
				cout << corrSetup->maxNSBetweenACAvg << ")" << endl;
			}
			difxScan->maxNSBetweenACAvg = corrSetup->numBufferedFFTs*fftDurNS;
		}
		if(corrSetup->numBufferedFFTs*fftDurNS > corrSetup->maxNSBetweenUVShifts)
		{
			cout << "The number of buffered FFTs (" << corrSetup->numBufferedFFTs;
			cout << ") gives a duration of " << corrSetup->numBufferedFFTs*fftDurNS;
			cout << ", longer that that specified for the UV shift interval (";
			cout << corrSetup->maxNSBetweenUVShifts;
			cout << "). Reduce FFT buffering or increase allowed interval!" << endl;

			exit(EXIT_FAILURE);
		}

		snprintf(difxScan->identifier, DIFXIO_NAME_LENGTH, "%s", vexScan->defName.c_str());
		snprintf(difxScan->obsModeName, DIFXIO_NAME_LENGTH, "%s", vexScan->modeDefName.c_str());
	}
}

static void populateSpacecraftTable(DifxInput *D, const VexData *V, const std::set<std::string> &spacecraftSet, int verbose)
{
	DifxSpacecraft *ds;
	int v;

	D->spacecraft = newDifxSpacecraftArray(spacecraftSet.size());
	D->nSpacecraft = spacecraftSet.size();
	
	ds = D->spacecraft;

	for(set<string>::const_iterator s = spacecraftSet.begin(); s != spacecraftSet.end(); ++s, ++ds)
	{
		const VexSource *vexSource = V->getSourceByDefName(*s);
		if(!vexSource)
		{
			cerr << "Developer error: couldn't find spacecraft " << *s << " in the source table, aborting!)" << endl;
			
			exit(EXIT_FAILURE);
		}

		const long int deltaT = vexSource->ephemDeltaT;	// seconds -- interval between ephemeris calculations.  24 sec is normal
		const int intMJD = static_cast<int>(D->mjdStart);
		// start time in seconds rounded down to nearest 2 minute boundary, and then one more
		const int secStart = (static_cast<int>((D->mjdStart - intMJD)*720.0) - 1)*120;
		// end time in seconds rounded up to nearest 2 minute boundary, and then one more
		const int secEnd = static_cast<int>((D->mjdStop - intMJD)*720.0 + 1.999)*120;
		const int nPoint = (secEnd - secStart)/deltaT + 1;
		const double mjd0 = intMJD + secStart/86400.0;

		/* initialize state vector structure with evaluation times */
		ds->nPoint = nPoint;
		ds->pos = (sixVector *)calloc(ds->nPoint, sizeof(sixVector));
		for(int p = 0; p < nPoint; ++p)
		{
			sixVectorSetTime(ds->pos + p, intMJD, secStart + p*deltaT);
		}

		if(vexSource->type == VexSource::BSP)		// process a .bsp file through spice
		{
			if(verbose > 0)
			{
				cout << "Computing ephemeris for source: " << *vexSource << endl;
				cout << "  start mjd = " << intMJD << "  sec = " << secStart << endl;
				cout << "  nPoint = " << nPoint << endl;
			}

			v = populateSpiceLeapSecondsFromEOP(D->eop, D->nEOP);
			if(v != 0)
			{
				cerr << "Error: populateSpiceLeapSecondsFromEOP returned " << v << endl;

				exit(EXIT_FAILURE);
			}

			v = computeDifxSpacecraftEphemeris(ds, mjd0, deltaT/86400.0, nPoint,
				vexSource->bspObject.c_str(),
				0,
				vexSource->bspFile.c_str(),
				vexSource->ephemStellarAber,
				vexSource->ephemClockError);
			if(v != 0)
			{
				cerr << "Error: ephemeris calculation failed.  Must stop." << endl;
				
				exit(EXIT_FAILURE);
			}
		}
		else if(vexSource->type == VexSource::TLE)
		{
			if(verbose > 0)
			{
				cout << "Computing ephemeris for source: " << *vexSource << endl;
				cout << "  start mjd = " << intMJD << "  sec = " << secStart << endl;
				cout << "  nPoint = " << nPoint << endl;
			}

			v = populateSpiceLeapSecondsFromEOP(D->eop, D->nEOP);
			if(v != 0)
			{
				cerr << "Error: populateSpiceLeapSecondsFromEOP returned " << v << endl;

				exit(EXIT_FAILURE);
			}

			v = computeDifxSpacecraftTwoLineElement(ds, mjd0, deltaT/86400.0, nPoint,
				vexSource->defName.c_str(),
				0,
				vexSource->tle[1].c_str(),
				vexSource->tle[2].c_str(),
				vexSource->ephemStellarAber,
				vexSource->ephemClockError);
			if(v != 0)
			{
				cerr << "Error: TLE ephemeris calculation failed.  Must stop." << endl;
				
				exit(EXIT_FAILURE);
			}
		}
		else if(vexSource->type == VexSource::Fixed)
		{
			v = computeDifxSpacecraftEphemerisFromXYZ(ds, mjd0, deltaT/86400.0, nPoint,
				vexSource->X, vexSource->Y, vexSource->Z,
				0,
				vexSource->ephemClockError);
			if(v != 0)
			{
				cerr << "Error: XYZ ephemeris calculation failed.  Must stop." << endl;
				
				exit(EXIT_FAILURE);
			}
		}
		else
		{
			cerr << "Developer error: unknown source type; don't know how to compute state vectors." << endl;

			exit(EXIT_FAILURE);
		}

		// give the spacecraft table the right name so it can be linked to the source
		snprintf(ds->name, DIFXIO_NAME_LENGTH, "%s", vexSource->defName.c_str());
	}

	// Fill in the spacecraft IDs in the DifxInput object
	for(int s = 0; s < D->nSource; ++s)
	{
		for(int sc = 0; sc < D->nSpacecraft; ++sc)
		{
			if(strcmp(D->spacecraft[sc].name, D->source[s].name) == 0)
			{
				D->source[s].spacecraftId = sc;
				
				break;
			}
		}
	}
}

// Generate spectral channel flagging file for in-band edge channels.
// Format of each line:
//    <antenna name> <mjd start> <mjd stop> <difx freq id> <spectral channel start> <spectral channel stop> <flag reason string|'edge'>
static void writeDifxChannelFlags(const DifxInput *D, const CorrSetup* corrSetup, const CorrParams *corrParams)
{
	const char *flagReason = "edge";
	char chflagFile[DIFXIO_FILENAME_LENGTH];
	FILE *out;

	if(corrParams->globalOutputbands.size() < 1)
	{
		return;
	}

	const GlobalOutputband* ob = &corrParams->globalOutputbands.back();
	if(ob->outputBandwidthMode == OutputBandwidthOff)
	{
		return;
	}

	generateDifxJobFileBase(D->job, chflagFile);
	strcat(chflagFile, ".channelflags");

	out = fopen(chflagFile, "w");
	if(!out)
	{
		cerr << "Error: cannot open " << chflagFile << " for write." << endl;

		return;
	}

	for(int a = 0; a < D->nAntenna; a++)
	{
		for(size_t n = 0; n < ob->autobands.outputbands.size(); n++)
		{
			// Locate DiFX freq Id
			int outputFreqId = -1;
			for(int f = 0; f < D->nFreq; ++f)
			{
				const DifxFreq *df = D->freq + f;
				if(df->freq == ob->autobands.outputbands[n].fbandstart*1e-6
					&& df->bw == ob->autobands.outputbands[n].bandwidth*1e-6
					&& df->sideband == 'U')
				{
					outputFreqId = f;
					break;
				}
			}

			// Flag all in-band edge channels of that freq Id
			// note1: due to FITS FL#1 table structure, only one channel is named per flag file entry
			// note2: <job>.channelflags refers to full difxfreq table freq Ids, in contrast to existing <job>.flag referring to recBand ids!
			std::deque<int> channels;
			ob->autobands.listEdgeChannels(n, channels, corrSetup->FFTSpecRes, corrSetup->outputSpecRes);
			for(size_t m = 0; m < channels.size(); m++)
			{
				fprintf(out, "%s %lf %lf %d %d %d '%s'\n", D->antenna[a].name, D->mjdStart, D->mjdStop, outputFreqId, channels[m], channels[m], flagReason);
			}
		}
	}

	fclose(out);
}

static int writeJob(const Job& J, const VexData *V, const CorrParams *P, const std::list<Event> &events, const Shelves &shelves, int verbose, ofstream *of, int nDigit, char ext, int strict)
{
	DifxInput *D;
	const CorrSetup *corrSetup;
	const AntennaSetup *antennaSetup;
	const VexScan *vexScan;
	set<string> configSet;
	set<string> sourceSet;
	set<string> spacecraftSet;
	vector<pair<string,string> > configs;
	vector<freq> freqs;
	vector<vector<unsigned int> > toneSets;
	int nPulsar=0;
	int nbin, maxPulsarBins;
	unsigned int maxScanPhaseCentres;
	int nZoomBands, fqId, destFqId, polcount, zoomChans = 0, minChans;
	int decimation, worstcaseguardns;
	DifxDatastream *dd;
	double globalBandwidth;
	vector<set<int> > blockedfreqids;	// vector index is over antennaId

	// Initialize toneSets with the trivial case, which is used for all zoom bands
	vector<unsigned int> noTones;
	toneSets.push_back(noTones);

	// Assume same correlator setup for all scans
	if(J.scans.empty())
	{
		cerr << "Developer error: writeJob(): J.scans is empty" << endl;

		exit(EXIT_FAILURE);
	}
	
	vexScan = V->getScanByDefName(J.scans.front());
	if(!vexScan)
	{
		cerr << "Developer error: writeJob() top: scan[" << J.scans.front() << "] = 0" << endl;

		exit(EXIT_FAILURE);
	}
	const std::string &corrSetupName = P->findSetup(vexScan->defName, vexScan->sourceDefName, vexScan->modeDefName);
	corrSetup = P->getCorrSetup(corrSetupName);
	if(!corrSetup)
	{
		cerr << "Error: writeJob(): correlator setup " << corrSetupName << ": Not found!" << endl;

		exit(EXIT_FAILURE);
	}

	// make set of unique config names
	for(vector<string>::const_iterator si = J.scans.begin(); si != J.scans.end(); ++si)
	{
		string configName;

		vexScan = V->getScanByDefName(*si);
		if(!vexScan)
		{
			cerr << "Developer error: writeJob() loop: scan[" << *si << "] = 0" << endl;

			exit(EXIT_FAILURE);
		}
		configName = vexScan->modeDefName + string("_") + corrSetupName;
		configSet.insert(configName);
	}

	D = newDifxInput();

	D->mjdStart = J.mjdStart;
	D->mjdStop  = J.mjdStop;
	D->visBufferLength = P->visBufferLength;
	D->dataBufferFactor = P->dataBufferFactor;
	D->outputFormat = P->outputFormat;
	D->nDataSegments = P->nDataSegments;

	D->antenna = makeDifxAntennas(J, V, &(D->nAntenna));
	D->job = makeDifxJob(V->getDirectory(), J, D->nAntenna, V->getExper()->getFullName(), &(D->nJob), nDigit, ext, P);
	
	D->nConfig = configSet.size();
	D->config = newDifxConfigArray(D->nConfig);

	blockedfreqids.resize(D->nAntenna);

	maxPulsarBins = 0;
	maxScanPhaseCentres = 0;

	// Make rule table
	populateRuleTable(D, P);

	// put all the sources (both pointing sources and phase centers) into the destination DiFX source table
	populateSourceTable(D, J, V, sourceSet);

	getSpacecraftSet(V, sourceSet, spacecraftSet);

	// now run through all scans, populating things as we go
	populateScanTable(D, J, V, P, corrSetup, configs, maxScanPhaseCentres);

	// look for pulsars
	for(int configId = 0; configId < D->nConfig; ++configId)
	{
		corrSetup = P->getCorrSetup(configs[configId].second);
		if(!corrSetup->binConfigFile.empty())
		{
			++nPulsar;
		}
	}
	if(nPulsar > 0)
	{
		D->pulsar = newDifxPulsarArray(nPulsar);
	}

	// configure datastreams
	
	// Shelves are a bit awkward...  They are currently tied to an antenna, but really they belong to a datastream.
	D->datastream = makeDifxDatastreams(J, V, D->nConfig, D->antenna, shelves);
	D->nDatastream = 0;
	for(int configId = 0; configId < D->nConfig; ++configId)
	{
		const VexMode *mode;
		DifxConfig *config;

		config = D->config + configId;
		config->nDatastream = 0;

		mode = V->getModeByDefName(configs[configId].first);
		if(mode == 0)
		{
			cerr << "Developer error: writeJob: mode[" << configs[configId].first << "] is null" << endl;

			exit(EXIT_FAILURE);
		}

		// Currently only this is supported
		decimation = 1;

		corrSetup = P->getCorrSetup(configs[configId].second);
		if(corrSetup == 0)
		{
			cerr << "Developer error: writeJob: correlator setup[" << configs[configId].second << "] is null" << endl;

			exit(EXIT_FAILURE);
		}

		if(!corrSetup->binConfigFile.empty())
		{
			int ok;

			ok = checkCRLF(corrSetup->binConfigFile.c_str(), (verbose > 1));
			if(ok < 0)
			{
				cerr << "The pulsar bin config file " << corrSetup->binConfigFile << " has problems.  Exiting." << endl;

				exit(EXIT_FAILURE);
			}

			config->pulsarId = D->nPulsar;
			loadPulsarConfigFile(D, corrSetup->binConfigFile.c_str());
			nbin = D->pulsar[D->nPulsar-1].nBin;
			if(D->pulsar[D->nPulsar-1].scrunch > 0)
			{
				nbin = 1;
			}
			if(nbin > maxPulsarBins)
			{
				maxPulsarBins = nbin;
			}

			for(int p = 0; p < D->pulsar[D->nPulsar-1].nPolyco; ++p)
			{
				ok = checkCRLF(D->pulsar[D->nPulsar-1].polyco[p].fileName, (verbose > 1));
				if(ok < 0)
				{
					cerr << "The pulsar polyco file " << D->pulsar[D->nPulsar-1].polyco[p].fileName << " has problems.  Exiting." << endl;

					exit(EXIT_FAILURE);
				}
			}
		}

		if(!corrSetup->phasedArrayConfigFile.empty())
		{
			config->phasedArrayId = D->nPhasedArray;
			snprintf(D->phasedarray[D->nPhasedArray].fileName, DIFXIO_FILENAME_LENGTH, "%s", corrSetup->phasedArrayConfigFile.c_str());
			++D->nPhasedArray;
		}

		// iterate over all antennas, making sure all recorded bands are allocated and all utilized datastreams populated
		assert(D->nDatastream == 0);
		for(std::map<std::string,VexSetup>::const_iterator it = mode->setups.begin(); it != mode->setups.end(); ++it)
		{
			const std::string &antName = it->first;
			const VexSetup &setup = it->second;

			// do some prodding, any antenna
			for(unsigned int ds = 0, startBand = 0; ds < setup.nStream(); ++ds)
			{
				const VexStream &stream = setup.streams[ds];
				// the zero below is just to provide a legal slot to do some prodding
				setFormat(D, 0, freqs, toneSets, mode, antName, startBand, setup, stream, corrSetup, P);
				startBand += stream.nRecordChan;
			}

			// for antenna in job go ahead and populate all datastreams
			if(find(J.jobAntennas.begin(), J.jobAntennas.end(), antName) == J.jobAntennas.end())
			{
				continue;
			}

			for(unsigned int ds = 0, startBand = 0; ds < setup.nStream(); ++ds)
			{
				const VexStream &stream = setup.streams[ds];
				setFormat(D, D->nDatastream, freqs, toneSets, mode, antName, startBand, setup, stream, corrSetup, P);
				if(stream.nRecordChan)
				{
					config->datastreamId[config->nDatastream] = D->nDatastream;
					++D->nDatastream;
					++config->nDatastream;
				}
				startBand += stream.nRecordChan;
			}
		}

		// if outputbands are present, register all associated zooms and output freqs
		if(!P->globalOutputbands.empty() && P->globalOutputbands.back().outputBandwidthMode != OutputBandwidthOff)
		{
			const GlobalOutputband* gob = &(P->globalOutputbands.back());

			// register output freqs into job freqs[] list
			std::vector<AutoBands::Outputband>::const_iterator itOb;
			for(itOb = gob->autobands.outputbands.begin(); itOb != gob->autobands.outputbands.end(); ++itOb)
			{
				std::string rxname(""); // TODO: how to determine rxName for any global outputband, if possible at all?

				// register outputband
				int fqId = addFreqId(
					freqs,
					itOb->fbandstart, itOb->bandwidth, 'U',
					corrSetup->FFTSpecRes, corrSetup->outputSpecRes,
					decimation, /*zoom:*/1, 0, rxname // final zero points to the noTone pulse cal setup.
				);
				if (verbose > 0)
				{
					cout << "Output band at " << std::fixed << itOb->fbandstart*1e-6 << " MHz USB "
						<< std::fixed << itOb->bandwidth*1e-6 << " MHz received fq id " << fqId << "\n";
				}
			}

			// block all recorded frequencies that do not contribute to either a zoom band or to an outputband, otherwise
			// these "unused" freqs would inadverently end up getting correlated and disrupt the uniform output bandwidth
			for(int configds = 0; configds < config->nDatastream; ++configds)
			{
				const int dsid = config->datastreamId[configds];
				const int antid = D->datastream[dsid].antennaId;
				for(int fq = 0; fq < D->datastream[dsid].nRecFreq; ++fq)
				{
					const int fqId = D->datastream[dsid].recFreqId[fq];
					const int dstFqId = gob->autobands.lookupDestinationFreq(freqs[fqId], freqs);
					if (dstFqId < 0)
					{
						blockedfreqids[antid].insert(fqId);
						// cout << "ant " << antid << " blocking recFq " << fq << " freqId " << fqId << " because of no dstFqId\n";
					}
					else
					{
						// Update the default 1:1 destFreqid that had been set earlier in setFormat(), before outputbands had been generated
						D->datastream[dsid].recFreqDestId[fq] = dstFqId;
					}
				}
			}

		}

		// iterate over all antennas and fill in zoom band, LO offset, clock offset details
		minChans = corrSetup->minInputChans();
		for(std::map<std::string,VexSetup>::const_iterator it = mode->setups.begin(); it != mode->setups.end(); ++it)
		{
			const std::string &antName = it->first;
			const VexSetup &setup = it->second;
			unsigned int startBand, startFreq;
			int currDatastream;

			startBand = 0;
			startFreq = 0;

			if(find(J.jobAntennas.begin(), J.jobAntennas.end(), antName) == J.jobAntennas.end())
			{
				continue;
			}

			for(currDatastream = 0; currDatastream < config->nDatastream; ++currDatastream)
			{
				int ai = D->datastream[currDatastream].antennaId;

				if(antName == D->antenna[ai].name)
				{
					break;
				}
			}

			if(currDatastream == -1)
			{
				std::cerr << "Developer error: currDatastream=-1  antenna=" << antName << std::endl;

				exit(0);
			}

			const VexAntenna *antenna = V->getAntenna(antName);
			antennaSetup = P->getAntennaSetup(antName);

			for(unsigned int ds = 0; ds < setup.nStream(); ++ds)
			{
				const VexStream &stream = setup.streams[ds];
				int v = setFormat(D, currDatastream, freqs, toneSets, mode, antName, startBand, setup, stream, corrSetup, P);
				if(v)
				{
					dd = D->datastream + currDatastream;
					dd->phaseCalIntervalMHz = setup.phaseCalIntervalMHz();
					dd->phaseCalBaseMHz = setup.phaseCalBaseMHz();
					dd->tcalFrequency = antenna->tcalFrequency;

					// FIXME: eventually zoom bands will migrate to the VexMode/VexSetup infrastructure.  until then, use antennaSetup directly
					if(antennaSetup)
					{
						nZoomBands = 0;
						
						int nZoomFreqs = antennaSetup->zoomFreqs.size();

						if(nZoomFreqs > 0)
						{
							int *parentFreqIndices = new int[nZoomFreqs];
							int nZoom;	// actual number of zoom freqs used
							int nZoomSkip = 0;

							DifxDatastreamAllocZoomFreqs(dd, nZoomFreqs);
							
							nZoom = 0;
							for(int i = 0; i < nZoomFreqs; ++i)
							{
								const ZoomFreq &zf = antennaSetup->zoomFreqs[i];

								std::vector<int> matchingFreqs;
								for(int j = 0; j < dd->nRecFreq; ++j)
								{
									const freq* dsfreq = &freqs[dd->recFreqId[j]];
									if(zf.matchesFreq(dsfreq))
									{
										if(!corrSetup->correlateFreqId(dd->recFreqId[j]))
										{
											if(verbose > 0)
											{
												cout << "Warning: Antenna " << antName << " datastream " << ds << " recorded freq " << j << " (global " << dd->recFreqId[j] << ") could provide zoom band " << i << ", but is excluded in the v2d freqId setting." << std::endl;
											}
											continue;
										}
										matchingFreqs.push_back(j); // multiple recfreqs can provide the same zoom when recfreqs overlap (ALMA)
									}
								}

								parentFreqIndices[nZoom] = bestMatchingFreq(zf, matchingFreqs, dd, freqs);
								if(parentFreqIndices[nZoom] < 0)
								{
									nZoomSkip++;
									cerr << "Warning: Cannot find a parent freq for zoom band " << i << " (freq@" << std::fixed << std::setprecision(3) << zf.frequency*1e-6 << " MHz) of datastream " << ds << " for antenna " << antName << endl;

									continue;
								}

								zoomChans = static_cast<int>(zf.bandwidth/corrSetup->FFTSpecRes);
								if(zoomChans < minChans)
								{
									minChans = zoomChans;
								}
								fqId = addFreqId(freqs, zf.frequency, zf.bandwidth, 'U', corrSetup->FFTSpecRes, corrSetup->outputSpecRes, decimation, /*zoom:*/1, 0, "");	// final zero points to the noTone pulse cal setup.
								if(P->globalOutputbands.size() >= 1 && P->globalOutputbands.back().outputBandwidthMode != OutputBandwidthOff)
								{
									destFqId = P->globalOutputbands.back().autobands.lookupDestinationFreq(freqs[fqId], freqs);
									if (destFqId < 0)
									{
										cout << "Info: Could not look up destination frequency for zoom with freq " << fqId << ", will default to outputting the zoom as itself." << endl;
										destFqId = fqId;
									}
								}
								else
								{
									destFqId = fqId;
								}
								dd->zoomFreqId[nZoom] = fqId;
								dd->zoomFreqDestId[nZoom] = destFqId;
								dd->nZoomPol[nZoom] = dd->nRecPol[parentFreqIndices[nZoom]];
								nZoomBands += dd->nRecPol[parentFreqIndices[nZoom]];
								if(!zf.correlateparent)
								{
									blockedfreqids[dd->antennaId].insert(dd->recFreqId[parentFreqIndices[nZoom]]);
								}
//Corner case BUG: if an LSB-flipping-only zoom *actually* is output, i.e. zoom equal to output,
//_but_ output fqId differs due to bookkeeping from zoom fqId index, we incorrectly block the LSB-flip...
//Commenting out for now:
//								if(dd->zoomFreqId[nZoom] != dd->zoomFreqDestId[nZoom])
//								{
//									blockedfreqids[dd->antennaId].insert(dd->zoomFreqId[nZoom]);
//								}
								++nZoom;
							}
							if(nZoomSkip > 0)
							{
								cerr << "Warning: dropped " << nZoomSkip << " zoom bands for " << antName << " datastream " << ds << ", " << nZoom << " remain" << endl;
							}
							dd->nZoomFreq = nZoom;
							DifxDatastreamAllocZoomBands(dd, nZoomBands);

							nZoomBands = 0;
							for(int i = 0; i < nZoom; ++i)
							{
								int k = 0;

								polcount = 0;
								for(int j = 0; j < dd->nZoomPol[i]; ++j)
								{
									dd->zoomBandFreqId[nZoomBands] = i;
									for(; k < dd->nRecBand; ++k)
									{
										if(dd->recBandFreqId[k] == parentFreqIndices[i])
										{
											dd->zoomBandPolName[nZoomBands] = dd->recBandPolName[k];

											++polcount;
											++k;

											break;
										}
									}
									++nZoomBands;
								}
								if(polcount != dd->nZoomPol[i])
								{
									cout << "Developer error: didn't find all zoom pols (was looking for " << dd->nZoomPol[i] << ", only found " << polcount << ")!!" << endl;
									cout << "  Ant=" << antName << " zoomfreq " << i << "/" << nZoom << " nZoomPol[" << i << "]=" << dd->nZoomPol[i] << std::endl;

									exit(EXIT_FAILURE);
								}
							}
							delete [] parentFreqIndices;
						} // if zoom freqs

						unsigned int nFreqClockOffsets = antennaSetup->freqClockOffs.size();
						unsigned int nFreqClockOffsetsDelta = antennaSetup->freqClockOffsDelta.size();
						unsigned int nFreqPhaseDelta = antennaSetup->freqPhaseDelta.size();
						if(nFreqClockOffsets > 0)
						{
							if((startFreq + D->datastream[currDatastream].nRecFreq) > nFreqClockOffsets ||
							   (startFreq + D->datastream[currDatastream].nRecFreq) > nFreqClockOffsetsDelta ||
							   (startFreq + D->datastream[currDatastream].nRecFreq) > nFreqPhaseDelta)
							{
								cerr << endl;
								// FIXME: message '<n> recorded frequencies needed' currently shows <n> that tends to be wrong, ignores yet unprocessed datastreams, TODO use total nr of expected rec freqs
								cerr << "Error: AntennaSetup for " << antName << " has only " << nFreqClockOffsets << " freqClockOffsets specified but " << (startFreq + dd->nRecFreq) << " recorded frequencies needed, or" << endl;
								cerr << "Error: AntennaSetup for " << antName << " has only " << nFreqClockOffsetsDelta << " freqClockOffsetsDelta specified but " << (startFreq + dd->nRecFreq) << " recorded frequencies needed, or" << endl;
								cerr << "Error: AntennaSetup for " << antName << " has only " << nFreqPhaseDelta << " freqPhaseDelta specified but " << (startFreq + dd->nRecFreq) << " recorded frequencies needed." << endl;

								exit(EXIT_FAILURE);
							}
							if(antennaSetup->freqClockOffs.front() != 0.0)
							{
								if(P->allowAllClockOffsets)
								{
									cerr << endl;
									cerr << "Warning: AntennaSetup for " << antName << " has a non-zero clock offset for the first" << " frequency offset - this can compromise model accountability," << " proceeding because allowAllClockOffsets = true." << endl;
								}
								else
								{
									cerr << endl;
									cerr << "Error: AntennaSetup for " << antName << " has a non-zero clock offset for the first" << " frequency offset. This is not allowed for model " << "accountability reasons." << endl;

									exit(EXIT_FAILURE);
								}
							}
							for(int i = 0; i < D->datastream[currDatastream].nRecFreq; ++i)
							{
								double freqClockOffs, freqClockOffsDelta, freqPhaseDelta;

								freqClockOffs = (startFreq + i < nFreqClockOffsets) ? antennaSetup->freqClockOffs.at(startFreq + i) : 0.0;
								D->datastream[currDatastream].clockOffset[i] = freqClockOffs;

								freqClockOffsDelta = (startFreq + i < nFreqClockOffsetsDelta) ? antennaSetup->freqClockOffsDelta.at(startFreq + i) : 0.0;
								D->datastream[currDatastream].clockOffsetDelta[i] = freqClockOffsDelta;
								
								freqPhaseDelta = (startFreq + i < nFreqPhaseDelta) ? antennaSetup->freqPhaseDelta.at(startFreq + i) : 0.0;
								D->datastream[currDatastream].phaseOffset[i] = freqPhaseDelta;
							}
						}

						// TODO: consider specifying loOffsets per datastream rather than per antenna
						unsigned int nLoOffsets = antennaSetup->loOffsets.size();
						if(nLoOffsets > 0)
						{
							if((startFreq + D->datastream[currDatastream].nRecFreq) > nLoOffsets)
							{
								cerr << endl;
								cerr << "Error: AntennaSetup for " << antName << " has only " << nLoOffsets << " loOffsets specified but " << (startFreq + dd->nRecFreq) << " recorded frequencies needed." << endl;

								exit(EXIT_FAILURE);
							}
							for(int i = 0; i < D->datastream[currDatastream].nRecFreq; ++i)
							{
								double loOffset;

								loOffset = (startFreq + i < nLoOffsets) ? antennaSetup->loOffsets.at(startFreq + i) : 0.0;
								D->datastream[currDatastream].freqOffset[i] = loOffset;
							}
						}
					} // if antennaSetup
					startFreq += D->datastream[currDatastream].nRecFreq;
					++currDatastream;
				} // if valid format
				startBand += stream.nRecordChan;
			} // datastream loop
		} // antenna loop
		if(corrSetup->xmacLength > minChans)
		{
			cerr << "Error: xmacLength set explicitly to " << corrSetup->xmacLength << ", but minChans (from a zoom freq) was " << minChans << endl;
				
			exit(EXIT_FAILURE);
		}
		if(config->guardNS > 0)
		{
			worstcaseguardns = calculateWorstcaseGuardNS(mode->getLowestSampleRate(), config->subintNS, mode->getMinBits(), mode->getMinSubbands());
			if(config->guardNS < worstcaseguardns)
			{
				cerr << "vex2difx calculates the worst-case guardNS as " << worstcaseguardns << ", but you have explicitly set " << config->guardNS << ". It is possible that mpifxcorr will refuse to run! Unless you know what you are doing, you should probably set guardNS to " << worstcaseguardns << " or above, or just leave it unset!" << endl;
				if(strict)
				{
					cerr << "\nExiting since strict mode was enabled" << endl;

					exit(EXIT_FAILURE);
				}
				else
				{
					cerr << "\nContinuing since --force was specified" << endl;
				}
			}
		}
	} // configId loop

	if(nPulsar != D->nPulsar)
	{
		cerr << "Error: nPulsar=" << nPulsar << " != D->nPulsar=" << D->nPulsar << endl;
		
		exit(EXIT_FAILURE);
	}

	// Make EOP table
	populateEOPTable(D, V->getEOPs());

	// Populate spacecraft table
	if(!spacecraftSet.empty())
	{
		populateSpacecraftTable(D, V, spacecraftSet, verbose);
	}

	// Make frequency table
	populateFreqTable(D, freqs, toneSets);

	// Make sure all polarizations are capitalized before writing, and round up to 2^n record channels if needed
	fixDatastreamTable(D);	// be sure to call simplifyDifxFreqs(D) after doing this

	// Make baseline table
	globalBandwidth = populateBaselineTable(D, P, corrSetup, blockedfreqids);
	if(globalBandwidth < 0)	// Implies conflicting bandwidths found
	{
		cerr << "Warning: differing correlation channel bandwidths found in scan " << vexScan->defName <<".  You can correlate this data, but won't be able to convert to FITS!" << endl;
	}
	if(globalBandwidth == 0) // Implies no baselines found
	{
		cerr << "Note: no correlatable baselines were found for scan " << vexScan->defName << "." << endl;
	}

	// Merge identical table entries
	simplifyDifxFreqs(D);
	simplifyDifxDatastreams(D);
	simplifyDifxBaselines(D);
	simplifyDifxConfigs(D);

	//delete any unused rules
	simplifyDifxRules(D);

	//delete the "blocked freq" array
	blockedfreqids.clear();

	if(P->simFXCORR)
	{
		// nudge integration times and start times to match those of the VLBA HW correlator
		DifxInputSimFXCORR(D);
	}

	//All averaging will always be in correlator by default, not difx2fits
	D->specAvg  = 1;

	if(D->nBaseline > 0 || P->minSubarraySize == 1)
	{
		// write input file
		writeDifxInput(D);

		// write calc file
		writeDifxCalc(D);

		// write threads file if requested
		if(P->nCore > 0 && P->nThread > 0)
		{
			DifxInputAllocThreads(D, P->nCore);
			DifxInputSetThreads(D, P->nThread);
			DifxInputWriteThreads(D);
		}

		// write machines file if possible
		if(!P->machines.empty())
		{
			char machinesFile[DIFXIO_FILENAME_LENGTH];
			FILE *out;

			generateDifxJobFileBase(D->job, machinesFile);
			strcat(machinesFile, ".machines");

			out = fopen(machinesFile, "w");
			if(!out)
			{
				cerr << "Error: cannot open " << machinesFile << " for write." << endl;
			}
			else
			{
				list<string>::const_iterator m = P->machines.begin();

				fprintf(out, "%s\n", m->c_str());
				++m;
				
				for(int a = 0; a < D->nAntenna; ++a)
				{
					const AntennaSetup *A = P->getAntennaSetup(D->antenna[a].name);
					if(!A)
					{
						if(m == P->machines.end())
						{
							cerr << "Warning: fewer than nDatastream+1 machines specified in .v2d file" << endl;
							break;
						}
						fprintf(out, "%s\n", m->c_str());
						++m;
					}
					else
					{
						for(std::vector<DatastreamSetup>::const_iterator dsit = A->datastreamSetups.begin(); dsit != A->datastreamSetups.end(); ++dsit)
						{
							if(!dsit->machine.empty())
							{
								fprintf(out, "%s\n", dsit->machine.c_str());
							}
							else
							{
								if(m == P->machines.end())
								{
									cerr << "Warning: fewer than nDatastream+1 machines specified in .v2d file" << endl;
									break;
								}
								fprintf(out, "%s\n", m->c_str());
								++m;
							}
						}
					}
				}

				for( ;m != P->machines.end(); ++m)
				{
					fprintf(out, "%s\n", m->c_str());
				}
				fclose(out);
			}
		}

		// write flag file
		J.generateFlagFile(*V, events, D->job->flagFile, P->invalidMask);

		// write channel flagging file
		writeDifxChannelFlags(D, corrSetup, P);

		if(verbose > 2)
		{
			printDifxInput(D);
		}

		if(of)
		{
			char fileBase[DIFXIO_FILENAME_LENGTH];
			double tops;    // Trillion operations
			int p;

			generateDifxJobFileBase(D->job, fileBase);

			tops = J.calcOps(V, 2*corrSetup->maxInputChans(), corrSetup->doPolar) * 1.0e-12;

			*of << fileBase << " " << J.mjdStart << " " << J.mjdStop << " " << D->nAntenna << " ";
			*of << maxPulsarBins << " ";
			*of << maxScanPhaseCentres << " ";
			p = of->precision();
			of->precision(4);
			*of << tops << " ";
			*of << (J.dataSize/1000000) << "  #";
			of->precision(p);

			for(vector<string>::const_iterator ai = J.jobAntennas.begin(); ai != J.jobAntennas.end(); ++ai)
			{
				*of << " " << *ai;
			}
			*of << endl;
		}
	}
	else
	{
		char fileBase[DIFXIO_FILENAME_LENGTH];

		generateDifxJobFileBase(D->job, fileBase);

		cerr << "Warning: job " << fileBase << " not written since it correlates no data" << endl;
		cerr << "This is often due to media not being specified, all frequency Ids being" << endl;
		cerr << "unselected, or too many antennas being explicitly unselected by time." << endl;
		cerr << "It is also possible that the vex file is faulty and missing, e.g., an $IF" << endl;
		cerr << "section, leading to missing polarisation information." << endl;

		cerr << "nBaseline=" << D->nBaseline << "  minSubarraySize=" << P->minSubarraySize << endl;
	}

	if(D->nBaseline > 0 || P->minSubarraySize == 1)
	{
		// clean up and return that job was created
		deleteDifxInput(D);

		return 1;
	}
	else
	{
		// clean up and return that job was not created
		deleteDifxInput(D);

		return 0;
	}
}

static void usage(int argc, char **argv)
{
	cout << endl;
	cout << program << " version " << version << "  " << author << " " << verdate << endl;
	cout << endl;
	cout << "Usage:  " << argv[0] << " [<options>] <v2d file>" << endl;
	cout << endl;
	cout << "  <options> can include:" << endl;
	cout << "     -h" << endl;
	cout << "     --help        display this information and quit." << endl;
	cout << endl;
	cout << "     -v" << endl;
	cout << "     --verbose     increase the verbosity of the output; -v -v for more." << endl;
	cout << endl;
	cout << "     -o" << endl;
	cout << "     --output      create a v2d file with all defaults populated." << endl;
	cout << endl;
	cout << "     -d" << endl;
	cout << "     --delete-old  delete all job(s) in this series before running." << endl;
	cout << endl;
	cout << "     -f" << endl;
	cout << "     --force       continue desipte warnings." << endl;
	cout << endl;
	cout << "     -s" << endl;
	cout << "     --strict      treat some warnings as errors and quit [default]." << endl;
	cout << endl;
	cout << "     -6" << endl;
	cout << "     --mk6         call mk62v2d utility to generate mark6 related files" << endl;
	cout << endl;
	cout << "  <v2d file> is the vex2difx configuration file to process." << endl;
	cout << endl;
	cout << "When running " << program << " you will likely see some output to the screen." << endl;
	cout << "Some messages may be important.  Most messages are categorized" << endl;
	cout << "with one of three qualifiers:" << endl;
	cout << "  * Note    This may be normal but usually indicates " << program << " is changing" << endl;
	cout << "            something automatically and that may not be what you want." << endl;
	cout << "  * Warning This is something that does not prevent " << program << " from running" << endl;
	cout << "            but has a high likelihood of doing something different than" << endl;
	cout << "            you intend." << endl;
	cout << "  * Error   " << program << " could not complete due to this problem." << endl;
	cout << endl;
	cout << "See http://cira.ivec.org/dokuwiki/doku.php/difx/vex2difx for more information" << endl;
	cout << endl;
	cout << "NOTE: This version now supports much of the vex2 specification (as well as the" << endl;
	cout << "vex 1.5 specification) but it is only lightly tested.  Proceed with caution." << endl;
	cout << endl;
}

static void runCommand(const char *cmd, int verbose)
{
	if(verbose > 0)
	{
		cout << "Executing: " << cmd << endl;
	}
	int s = system(cmd);
	if(s == -1)
	{
		cerr << "Error executing: " << cmd << endl;

		exit(EXIT_FAILURE);
	}
}

// Note: this is approximate, assumes all polarizations matched and no IFs being selected out
static void calculateScanSizes(VexData *V, const CorrParams &P)
{
	int nScan;

	nScan = V->nScan();

	for(int s = 0; s < nScan; ++s)
	{
		const VexScan *scan;
		const VexMode *mode;
		const CorrSetup *setup;
		unsigned int nSubband, nBaseline;
		
		scan = V->getScan(s);
		if (!scan)
		{
			cerr << "Warning: scan " << s << " could not be looked up!" << endl;
			continue;
		}

		mode = V->getModeByDefName(scan->modeDefName);
		if (!mode)
		{
			cerr << "Warning: scan " << scan->defName << " has undefined VEX mode " << scan->modeDefName << "!" << endl;
			continue;
		}

		const std::string &corrSetupName = P.findSetup(scan->defName, scan->sourceDefName, scan->modeDefName);
		setup = P.getCorrSetup(corrSetupName);
		if(!setup)
		{
			cerr << "Warning: calculateScanSizes: CorrSetup for scan " << corrSetupName << " not found" << endl;
		}
		else
		{
			nSubband = mode->subbands.size();
			nBaseline = scan->stations.size()*(scan->stations.size()+1)/2;
			V->setScanSize(s, scan->duration()*86400*nBaseline*nSubband*setup->bytesPerSecPerBLPerBand());
		}
	}
}

void writeRemovedAntennasFile(const std::string &missingDataFile, const list<pair<int,string> > &removedAntennas)
{
	set<string> uniq;
	ofstream of;
	int lastJobId = -1;
	int n = 0;

	of.open(missingDataFile.c_str());
	of << "The following job number(s) had antennas removed because they have no baseband data:";
	for(list<pair<int,string> >::const_iterator it = removedAntennas.begin(); it != removedAntennas.end(); ++it)
	{
		if(it->first != lastJobId)
		{
			uniq.clear();
			of << endl;
			of << "job " << it->first << " :";
			lastJobId = it->first;
			++n;
		}
		if(uniq.find(it->second) == uniq.end())
		{
			of << " " << it->second;
			uniq.insert(it->second);
		}
	}
	of << endl;
	of.close();

	cout << endl;
	cout << "Note: " << n << " job(s) had one or more antennas removed due to missing baseband data." << endl;
	cout << "See " << missingDataFile << " for details." << endl;
}

int main(int argc, char **argv)
{
	CorrParams *P;
	VexData *V;
	Shelves shelves;
	const VexScan *S;
	const SourceSetup *sourceSetup;
	list<Event> events;
	set<string> canonicalVDIFUsers;
	vector<Job> J;
	string shelfFile;
	string missingDataFile;	// created if file-based and no files for a particular antenna/job are found
	string v2dFile;
	string command;
	int verbose = 0;
	int ok;
	bool writeParams = false;
	bool deleteOld = false;
	bool strict = true;
	bool mk6 = false;
	unsigned int nWarn = 0;
	unsigned int nError = 0;
	unsigned int nSkip = 0;
	unsigned int nDigit;
	unsigned int nJob = 0;
	std::list<std::pair<int,std::string> > removedAntennas;

	if(argc < 2)
	{
		usage(argc, argv);

		return EXIT_FAILURE;
	}

	// force program to work in Universal Time
	setenv("TZ", "", 1);
	tzset();


	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argc, argv);

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-v") == 0 ||
				strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-o") == 0 ||
				strcmp(argv[a], "--output") == 0)
			{
				writeParams = 1;
			}
			else if(strcmp(argv[a], "-d") == 0 ||
				strcmp(argv[a], "--delete-old") == 0)
			{
				deleteOld = 1;
			}
			else if(strcmp(argv[a], "-f") == 0 ||
				strcmp(argv[a], "--force") == 0)
			{
				strict = 0;
			}
			else if(strcmp(argv[a], "-s") == 0 ||
				strcmp(argv[a], "--strict") == 0)
			{
				strict = 1;
			}
			else if(strcmp(argv[a], "-6") == 0 ||
				strcmp(argv[a], "--mk6") == 0)
			{
				mk6 = 1;
			}
			else
			{
				cerr << "Error: unknown option " << argv[a] << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(EXIT_FAILURE);
			}
		}
		else
		{
			if(!v2dFile.empty())
			{
				cerr << "Error: multiple configuration files provided, only one expected." << endl;
				cerr << "Run with -h for help information." << endl;
				
				exit(EXIT_FAILURE);
			}
			v2dFile = argv[a];
		}
	}

	if(v2dFile.size() > DIFX_MESSAGE_PARAM_LENGTH-2)
	{
		// job numbers into the tens of thousands will be truncated in difxmessage.  Better warn the user.
		cout << "Filename " << v2dFile << " is too long - its job name might be truncated by difxmessage!" << endl;
		cout << "You are strongly encouraged to choose a shorter .v2d name (root shorter than 26 characters)" << endl;
	}

	if(v2dFile.empty())
	{
		cerr << "Error: configuration (.v2d) file expected." << endl;
		cerr << "Run with -h for help information." << endl;
		
		exit(EXIT_FAILURE);
	}

	if(v2dFile.find("_") != string::npos)
	{
		cerr << "Error: you cannot have an underscore (_) in the filename!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(EXIT_FAILURE);
	}

	if(!isalpha(v2dFile[0]))
	{
		cerr << "Error: pass name (.v2d file name) must start with a letter!" << endl;
		cerr << "Please rename it and run again." << endl;
		
		exit(EXIT_FAILURE);
	}

	ok = checkCRLF(v2dFile.c_str(), (verbose > 1));
	if(ok < 0)
	{
		cerr << "The .v2d file has problems.  Exiting." << endl;

		exit(EXIT_FAILURE);
	}

        // call mk62v2d script for mark6 related v2d prework
	// mk62v2d exits without altering v2d if no mark6 modules are present in .vex.obs
	if(mk6)
	{
		command = "mk62v2d " + v2dFile;
	      	system(command.c_str());
	}

	P = new CorrParams(v2dFile);
	if(P->vexFile.empty())
	{
		cerr << "Error: vex file parameter (vex) not found in file." << endl;
		
		exit(EXIT_FAILURE);
	}

	ok = checkCRLF(P->vexFile.c_str(), (verbose > 1));
	if(ok < 0)
	{
		cerr << "The vex file has problems.  Exiting." << endl;

		exit(EXIT_FAILURE);
	}

	nWarn = P->parseWarnings;

	umask(02);

	shelfFile = P->vexFile.substr(0, P->vexFile.find_last_of('.'));
	shelfFile += string(".shelf");
	nWarn += shelves.load(shelfFile);

	if(verbose > 1 && !shelves.empty())
	{
		std::cout << shelves << std::endl;
	}

	// delete "no data" file before starting
	missingDataFile = v2dFile.substr(0, v2dFile.find_last_of('.'));
	missingDataFile += string(".nodata");
	command = "rm -f " + missingDataFile;
	system(command.c_str());

	/* All reports of incomplete modes to be presented */
	setReportIncompleteModes(1);

	V = loadVexFile(P->vexFile, &nWarn);
	if(!V)
	{
		cerr << "Error: cannot load vex file: " << P->vexFile << endl;
		
		exit(EXIT_FAILURE);
	}

	applyCorrParams(V, *P, nWarn, nError, canonicalVDIFUsers);
	calculateScanSizes(V, *P);

	if(!canonicalVDIFUsers.empty())
	{
		cout << "Note: canonical VDIF threads were assumed for the following antennas:";

		for(set<string>::const_iterator it = canonicalVDIFUsers.begin(); it != canonicalVDIFUsers.end(); ++it)
		{
			cout << " " << *it;
		}

		cout << endl;
	}
	
	V->generateEvents(events);
	V->addBreakEvents(events, P->manualBreaks);
	// find a function for this
	for(std::vector<AntennaSetup>::const_iterator as = P->antennaSetups.begin(); as != P->antennaSetups.end(); ++as)
	{
		if(as->mjdStart > 0.0)
		{
			addEvent(events, as->mjdStart, Event::ANTENNA_START, as->vexName);
		}
		if(as->mjdStop > 0.0)
		{
			addEvent(events, as->mjdStop, Event::ANTENNA_STOP, as->vexName);
		}
	}
	events.sort();

	if(verbose > 1)
	{
		cout << *V << endl;
		cout << *P << endl;
	}

	// set min and max bandwidths for each setup
	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);
		const std::string &corrSetupName = P->findSetup(scan->defName, scan->sourceDefName, scan->modeDefName);
		CorrSetup *corrSetup = P->getNonConstCorrSetup(corrSetupName);
		const VexMode *mode = V->getModeByDefName(scan->modeDefName);

		if(!mode)
		{
			continue;
		}
		for(map<string,VexSetup>::const_iterator sp = mode->setups.begin(); sp != mode->setups.end(); ++sp)
		{
			for(vector<VexChannel>::const_iterator cp = sp->second.channels.begin(); cp != sp->second.channels.end(); ++cp)
			{
				corrSetup->addRecordedBandwidth(cp->bbcBandwidth);
			}
		}
	}

	// set datastream channels
	// for each antenna with an ANTENNA section
	//   * if ndatastream = 0, just zero the nBand and startBand params to keep them flexible
	//   * else
	//     * get number of recorded channels
	//     * if it changes with mode, then bail out
	//     * in cases where nBand is not set, set it to totalrecchans/ndatastream
	//     * increment startBand to make set of datastreams contiguous
	//     * if startBand + nBand for the last datastream does not sum to ndatastream then bail out
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		const VexAntenna *ant = V->getAntenna(a);
		AntennaSetup *antSetup = P->getNonConstAntennaSetup(ant->name);
		if(antSetup)
		{
			int nads = antSetup->datastreamSetups.size();	// number of antenna datastreams

			if(nads == 0)
			{
				cerr << "Developer error: Number of Antenna Datastreams for antenna " << ant->name << " is zero!" << endl;

				exit(EXIT_FAILURE);
			}
			else if(nads == 1)
			{
				antSetup->datastreamSetups[0].nBand = 0;
				antSetup->datastreamSetups[0].startBand = 0;
				if(antSetup->filelistReadFail)
				{
					cerr << "Error: file list for antenna " << ant->name << " (" << antSetup->filelistFile << ") could not be read." << endl;
					++nError;
				}
			}
			else
			{
				int nRecChan = V->getNumAntennaRecChans(ant->name);	// returns < 0 if it varies with mode
				int chanCount;

				if(nRecChan < 0)
				{
					cerr << "Error: Cannot use multiple explicit datastreams in cases where number of record channels varies with mode." << endl;
					++nError;
				}

				chanCount = 0;
				for(int ads = 0; ads < nads; ++ads)
				{
					if(antSetup->datastreamSetups[ads].filelistReadFail)
					{
						cerr << "Error: file list for datastream " << ads << " of antenna " << ant->name << " (" << antSetup->datastreamSetups[ads].filelistFile << ") could not be read." << endl;
						++nError;
					}
					antSetup->datastreamSetups[ads].startBand = chanCount;
					if(antSetup->datastreamSetups[ads].nBand == 0)
					{
						if(nRecChan % nads != 0)
						{
							cerr << "Error: Number of record channels (" << nRecChan << ") does not divide evenly into number of datastreams (" << nads << ").  This error can be avoided by explicitly setting number of channels allocated to each datastream in the DATASTREAM blocks." << endl;
							++nError;
						}
						antSetup->datastreamSetups[ads].nBand = nRecChan/nads;
					}
					chanCount += antSetup->datastreamSetups[ads].nBand;
				}
				if(chanCount != nRecChan)
				{
					cerr << "Error: Number of channels represented in DATASTREAMS (" << chanCount << ") does not equal number in vex file (" << nRecChan << ")." << endl;
					++nError;
				}
			}
		}
	}
	
	if(nError > 0)
	{
		cerr << endl;
		cerr << nError << " fatal errors encountered.  Quitting." << endl;
		cerr << endl;

		exit(EXIT_FAILURE);
	}

	nWarn += P->sanityCheck();
	if(!P->fakeDatasource)
	{
		nWarn += V->sanityCheck();
	}
	else
	{
		cout << "Note: some sanity checks have been disabled because fake data is selected." << endl;
		if(V->nEOP() == 0)
		{
			cout << "Assuming EOPs with zero values" << endl;
			for(int i = -2; i <= 2; ++i)
			{
				V->newEOP()->mjd = static_cast<int>(V->getExper()->mjdStart) + i;
			}
		}
	}
	nWarn += sanityCheckConsistency(V, P);
	if(strict && nWarn > 0)
	{
		if(nWarn == 1)
		{
			cerr << "Quitting since there was a warning and strict mode was enabled." << endl;
		}
		else
		{
			cerr << "Quitting since " << nWarn << " warnings were found and strict mode was enabled." << endl;
		}
		cerr << "Strict mode can be disabled with --force if needed." << endl;
		
		exit(EXIT_FAILURE);
	}
	else if(nWarn > 0)
	{
		if(nWarn == 1)
		{
			cout << "Note: Proceeding even though there was a warning." << endl;
		}
		else
		{
			cout << "Note: Proceeding even though there were " << nWarn << " warnings." << endl;
		}
	}

	// run through all the scans once, creating source setups for any sources that don't have one
	for(unsigned int i = 0; i < V->nScan(); ++i)
	{
		SourceSetup * added;
		S = V->getScan(i);
		sourceSetup = P->getSourceSetup(S->sourceDefName);
		if(!sourceSetup)
		{
			const VexSource *src = V->getSourceByDefName(S->sourceDefName);
			
			added = new SourceSetup(S->sourceDefName);
			added->doPointingCentre = true;
			added->pointingCentre = PhaseCentre(src->ra, src->dec, src->sourceNames[0]);
			P->addSourceSetup(*added);
		}
	}

	if(verbose > 3)
	{
		cout << "Pre-job making events:" << endl;
		printEventList(events);
	}

	makeJobs(J, V, P, events, removedAntennas, verbose);
	P->updateZoomBandsForOutputBands(J, V, verbose);

	if(verbose > 2)
	{
		printEventList(events);
	}

	if(deleteOld)
	{
		const int CommandSize = 512;
		char cmd[CommandSize];
		int v;

		v = snprintf(cmd, CommandSize, "rm -f %s.params %s_*.{input,calc,flag}", v2dFile.c_str(), P->jobSeries.c_str());
		if(v < CommandSize)
		{
			if(verbose > 1)
			{
				cerr << "About to execute: " << cmd << endl;
			}
			runCommand(cmd, verbose);
		}
		else
		{
			cerr << "Developer warning: deletion of old files failed due to string length: " << v << " >= " << CommandSize << endl;
		}
	}

	if(writeParams)
	{
		ofstream of;
		string paramsFile = v2dFile + ".params";

		of.open(paramsFile.c_str());
		of << *P << endl;
		of.close();
	}

	ofstream of;
	string jobListFile = P->jobSeries + ".joblist";
	const char *difxVersion;
	const char *difxLabel;

	difxVersion = getenv("DIFX_VERSION");
	if(!difxVersion)
	{
		cout << endl;
		cout << "Warning: env. variable DIFX_VERSION is not set.  Setting to 'Unknown'" << endl;
		cout << "This means that your version accountability is being compromised!" << endl;
		cout << endl;
		difxVersion = "Unknown";
	}
	difxLabel = getenv("DIFX_LABEL");
	of.open(jobListFile.c_str());
	of.precision(12);
	of << "exper=" << V->getExper()->getFullName() << "  v2d=" << v2dFile <<"  pass=" << P->jobSeries << "  mjd=" << current_mjd() << "  DiFX=" << difxVersion << "  vex2difx=" << version << "  vex=" << P->vexFile;
	if(difxLabel)
	{
		of << "  label=" << difxLabel;
	}
	of << endl;
	
	nDigit=0;
	for(int l = J.size()+P->startSeries-1; l > 0; l /= 10)
	{
		++nDigit;
	}
	
	for(vector<Job>::iterator j = J.begin(); j != J.end(); ++j)
	{
		if(verbose > 0)
		{
			cout << *j;
		}
		if(j->jobSeries == "-")
		{
			++nSkip;
		}
		else
		{
			nJob += writeJob(*j, V, P, events, shelves, verbose, &of, nDigit, 0, strict);
		}
	}
	of.close();

	cout << endl;
	cout << nJob << " job(s) created." << endl;

	if(nJob > 0 && P->v2dComment.length() > 0)
	{
		cout << endl;
		cout << "The user supplied the following comments in the .v2d file:" << endl;
		cout << P->v2dComment << endl;
	}

	if(!removedAntennas.empty())
	{
		writeRemovedAntennasFile(missingDataFile, removedAntennas);
	}

	delete V;
	delete P;

	cout << endl;

	if(nJob > 0)
	{
		return EXIT_SUCCESS;
	}
	else
	{
		return EXIT_FAILURE;
	}
}
