/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken & Adam Deller               *
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
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdlib>
#include "applycorrparams.h"

int applyCorrParams(VexData *V, const CorrParams &params, int &nWarn, int &nError, std::set<std::string> &canonicalVDIFUsers)
{
	VexStream tmpVS;	// used to hold output of format parsing

	// merge sets of EOPs from vex and corr params file
	if(!params.eops.empty())
	{
		if(V->nEOP() > 0)
		{
			std::cerr << "Warning: Mixing EOP values from vex and v2d files.  Your mileage may vary!" << std::endl;
			++nWarn;
		}

		for(std::vector<VexEOP>::const_iterator e = params.eops.begin(); e != params.eops.end(); ++e)
		{
			V->addEOP(*e);
		}
	}

	// remove unwanted antennas
	for(unsigned int a = 0; a < V->nAntenna(); )
	{
		const VexAntenna *A;

		A = V->getAntenna(a);
		if(!A)
		{
			std::cerr << "Developer error: applyCorrParams: Antenna number " << a << " cannot be gotten even though nAntenna() reports " << V->nAntenna() << std::endl;

			exit(EXIT_FAILURE);
		}
		if(!params.useAntenna(A->name))
		{
			V->removeAntenna(A->name);
		}
		else
		{
			++a;
		}
	}

	// apply source parameters
	for(unsigned int s = 0; s < V->nSource(); ++s)
	{
		const VexSource *S = V->getSource(s);
		if(!S)
		{
			std::cerr << "Developer error: applyCorrParams: Source number " << s << " cannot be gotten even though nSource() reports " << V->nSource() << std::endl;

			exit(EXIT_FAILURE);
		}
		
		const SourceSetup *ss = params.getSourceSetup(S->defName);
		if(ss)
		{
			if(ss->pointingCentre.calCode != ' ')
			{
				V->setSourceCalCode(S->defName, ss->pointingCentre.calCode);
			}
		}

		// FIXME: here put multiphasecenter things, eventually

	}

	// remove scans that are not linked from v2d setups
	for(unsigned int s = 0; s < V->nScan(); )
	{
		const VexScan *S = V->getScan(s);
		if(!S)
		{
			std::cerr << "Developer error: applyCorrParams: Scan number " << s << " cannot be gotten even though nScan() reports " << V->nScan() << std::endl;

			exit(EXIT_FAILURE);
		}

		const VexSource *src = V->getSourceByDefName(S->sourceDefName);
		if(!src)
		{
			std::cerr << "Developer error: applyCorrParams: Scan " << S->defName << " (current index is " << s << ") not found" << std::endl;

			exit(EXIT_FAILURE);
		}

		const std::string &corrSetupName = params.findSetup(S->defName, S->sourceDefName, S->modeDefName);

		if(corrSetupName == "" || corrSetupName == "SKIP")
		{
			V->removeScan(S->defName);
		}
		else
		{
			if(params.getCorrSetup(corrSetupName) == 0)
			{
				std::cerr << "Error: Scan=" << S->defName << " correlator setup " << corrSetupName << " not defined!" << std::endl;

				exit(EXIT_FAILURE);
			}

			++s;
		}
	}

	// remove scans with too few antennas or with scans outside the specified time range
	V->reduceScans(params.minSubarraySize, params);

	// swap antenna polarizations
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		const VexAntenna *A;

		A = V->getAntenna(a);
		if(!A)
		{
			std::cerr << "Developer error: applyCorrParams: Antenna number " << a << " cannot be gotten even though nAntenna() reports " << V->nAntenna() << std::endl;

			exit(EXIT_FAILURE);
		}

		if(params.swapPol(A->name))
		{
			V->swapPolarization(A->name);
		}
	}

	// MODES / SETUPS / formats

	for(unsigned int m = 0; m < V->nMode(); ++m)
	{
		const VexMode *M = V->getMode(m);
		if(!M)
		{
			std::cerr << "Developer error: applyCorrParams: Mode number " << m << " cannot be gotten even though nMode() reports " << V->nMode() << std::endl;

			exit(EXIT_FAILURE);
		}
		for(std::map<std::string,VexSetup>::const_iterator it = M->setups.begin(); it != M->setups.end(); ++it)
		{
			const AntennaSetup *as = params.getAntennaSetup(it->first);
			if(!as)
			{
				// no antenna setup defined, continue;
				continue;
			}
			int corrparamsDatastreams = as->datastreamSetups.size();
			if(corrparamsDatastreams == 0)
			{
				// no setup datastreams defined, continue;
				continue;
			}
			int nDatastream = it->second.streams.size();
			if(nDatastream > 1 && nDatastream != corrparamsDatastreams)
			{
				std::cerr << "Error: multiple streams defined for mode " << M->defName << " antenna " << it->first << " but a non-matching number of DATASTREAMS are defined for this antenna in the .v2d file" << std::endl;

				++nError;

				continue;
			}

			// if number of DATASTREAMS in .v2d > 1 and number of streams found in vex == 1, then replicate the later to be of the size of the former, initializing with the same data.

			if(nDatastream == 1 && corrparamsDatastreams > 1)
			{
				V->cloneStreams(M->defName, it->first, corrparamsDatastreams);
			}

			for(int ds = 0; ds < corrparamsDatastreams; ++ds)
			{
				const DatastreamSetup &DS = as->datastreamSetups[ds];

				if(!DS.format.empty())
				{
					bool A, B;
					bool v;

					v = tmpVS.parseFormatString(DS.format);
					if(!v)
					{
						std::cerr << "Error: format " << DS.format << " did not parse sensibly," << std::endl;
						++nError;
					}

					A = isVDIFFormat(V->getFormat(M->defName, it->first, ds));
					B = isVDIFFormat(tmpVS.format);
					if(A != B)
					{
						if(A)
						{
							std::cerr << "Error: cannot change format from VDIF to any other non-VDIF format (antenna " << as->vexName << ") with the .v2d file.  You need to change the .vex file." << std::endl;
						}
						else
						{
							std::cerr << "Error: cannot change format to VDIF from any other non-VDIF format (antenna " << as->vexName << ") with the .v2d file.  You need to change the .vex file." << std::endl;
						}
						++nError;
					}

					v = V->setFormat(M->defName, it->first, ds, DS.format);
				}

				if(DS.nBand > 0 || DS.startBand >= 0)
				{
					V->setStreamBands(M->defName, it->first, ds, DS.nBand, DS.startBand);
				}
			}

			// apply canonical VDIF mapping if appropriate and if needed
			if(usesCanonicalVDIF(it->first) && it->second.usesFormat(VexStream::FormatVDIF))
			{
				V->setCanonicalVDIF(M->defName, it->first);
				canonicalVDIFUsers.insert(it->first);
			}
		}
	}

	// For modes where record channels were not explicitly assigned, use ordering by channel name
	V->generateRecordChans();

	// Data and data source
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		unsigned int nAssigned;
		const VexAntenna *A = V->getAntenna(a);
		std::vector<VexBasebandData> *vsns;
		if(!A)
		{
			std::cerr << "Developer error: applyCorrParams: Antenna " << a << " cannot be gotten" << std::endl;

			exit(EXIT_FAILURE);
		}

		const AntennaSetup *as = params.getAntennaSetup(A->name);
		if(!as)
		{
			// No antenna setup here, so continue...
			continue;
		}
		int nDatastreamSetup = as->datastreamSetups.size();

		vsns = V->getVSNs(A->name);
		if(!vsns)
		{
			std::cerr << "Developer error: applyCorrParams: Antenna " << a << " gave null vsns vector" << std::endl;

			exit(EXIT_FAILURE);
		}

		if(!vsns->empty())
		{
			// first go through vex-provided data and see if a datastream should be associated with it based on VexDatastream::recorderIds

			nAssigned = 0;
			if(nDatastreamSetup > 0)
			{
				std::vector<VexBasebandData>::iterator vit;

				for(vit = vsns->begin(); vit != vsns->end(); ++vit)
				{
					if(vit->streamId < 0 && vit->recorderId >= 0)
					{
						int nDSS;

						nDSS = 0;
						for(int i = 0; i < nDatastreamSetup; ++i)
						{
							const DatastreamSetup &dss = as->datastreamSetups[i];

							if(dss.recorderIds.find(vit->recorderId) != dss.recorderIds.end())
							{
								vit->streamId = i;
								if(nDSS == 0)
								{
									++nAssigned;
								}
								++nDSS;
							}
						}
						if(nDSS > 1)
						{
							std::cerr << "Error: multiple DATASTREAMS for ANTENNA " << A->name << " claimed recorderId=" << vit->recorderId << std::endl;

							++nError;
						}
					}
				}
			}

			// next go through vex-provided data and assign recorderIds to streamIds in logical sequence
			// if nDatastreamSetup == 0, then use the number of unique reduced recorderIds to set number of actual datastreams
			if(nAssigned == 0)	// Only try this if no assignment via recorderId was attempted
			{
				std::set<int> reducedRecorderIds;	// actual recorderId integer-divided by antenna-specific divisor
				int divisor, offset;
				int n;
				
				if(A->isVLBA())
				{
					divisor = 2;	// VLBA reports each bank of a Mark5 as separate recorderId
					offset = 1;	// VLBA recorderId starts at 1
				}
				else
				{
					divisor = 100;	// For now, don't try to use TAPELOG_OBS section to define datastreams
					offset = 0;
				}

				for(std::vector<VexBasebandData>::const_iterator vit = vsns->begin(); vit != vsns->end(); ++vit)
				{
					reducedRecorderIds.insert( (vit->recorderId - offset)/divisor );
				}
				n = 0;
				for(std::set<int>::const_iterator rit = reducedRecorderIds.begin(); rit != reducedRecorderIds.end(); ++rit)
				{
					for(std::vector<VexBasebandData>::iterator vit = vsns->begin(); vit != vsns->end(); ++vit)
					{
						if( (vit->recorderId - offset)/divisor == *rit)
						{
							vit->streamId = n;
						}
					}

					++n;
				}
				if(n > 1)
				{
					if(nDatastreamSetup > 1 && nDatastreamSetup != n)
					{
						std::cerr << "Error: mismatch between number of DATASTREAM setups and number of unique recorders for antenna " << A->name << std::endl;

						++nError;
					}

					for(unsigned int m = 0; m < V->nMode(); ++m)
					{
						const VexMode *M = V->getMode(m);
						int nRecChan;

						if(!M)
						{
							std::cerr << "Developer error: applyCorrParams: Mode number " << m << " cannot be gotten even though nMode() reports " << V->nMode() << " inside stream assignment loop." << std::endl;

							exit(EXIT_FAILURE);
						}

						nRecChan = M->nRecordChan(A->name);

						if(nRecChan % n != 0)
						{
							std::cerr << "Error: number of frequency recorded bands " << nRecChan << " is not divisible by inferred number of data streams " << n << ", which is based on TAPELOG_OBS section of vex file, for mode " << M->defName << " ." << std::endl;

							++nError;
						}

						std::map<std::string,VexSetup>::const_iterator it = M->setups.find(A->name);
						if(it != M->setups.end())
						{
							V->cloneStreams(M->defName, it->first, n);

							for(int ds = 0; ds < n; ++ds)
							{
								V->setStreamBands(M->defName, it->first, ds, nRecChan/n, nRecChan/n*ds);
							}

							// apply canonical VDIF mapping if appropriate and if needed
							if(usesCanonicalVDIF(it->first) && it->second.usesFormat(VexStream::FormatVDIF))
							{
								V->setCanonicalVDIF(M->defName, it->first);
								canonicalVDIFUsers.insert(it->first);
							}
						}
					}
				}
			}
			else if(nAssigned != vsns->size())
			{
				std::cerr << "Warning: one or more VSN(s) in the TAPELOG_OBS section were not assigned to a DATASTREAM for antenna " << A->name << std::endl;
			}

			// and then set the data source for each stream
			for(std::vector<VexBasebandData>::const_iterator vit = vsns->begin(); vit != vsns->end(); ++vit)
			{
				if(vit->isMark5())
				{
					V->setDataSource(a, vit->streamId, DataSourceModule);
				}
				else if(vit->isMark6())
				{
					V->setDataSource(a, vit->streamId, DataSourceMark6);
				}
				else
				{
					if(A->name[0] == '/')
					{
						V->setDataSource(a, vit->streamId, DataSourceFile);
					}
					else
					{
						std::cerr << "Error: TAPELOG_OBS entry " << vit->filename << " for antenna " << A->name << " is of unrecognized form.  If it is actually a file, it must have a full path, starting with / ." << std::endl;
						++nError;
					}
				}
			}
		}

		// finally, if datastreamSetups are provided with baseband data, apply to the mix
		// Note that the "recorder Id" in the VexBaseband structure does not get set for these insertions

		if(nDatastreamSetup <= 0)
		{
			// nothing provided
			continue;
		}

		for(int i = 0; i < nDatastreamSetup; ++i)
		{
			// Here just directly copy updated values from v2d into existing structure
			const DatastreamSetup &dss = as->datastreamSetups[i];

			V->setDifxTsys(a, i, dss.tSys);

			switch(dss.dataSource)
			{
			case DataSourceNone:
				V->setNoDatastream(a, i);
				break;
			case DataSourceFile:
				V->setFiles(a, i, dss.basebandFiles);
				break;
			case DataSourceModule:
				V->setModule(a, i, dss.vsn);
				break;
			case DataSourceNetwork:
				V->setNetworkParameters(a, i, dss.networkPort, dss.windowSize);
				break;
			case DataSourceFake:
				V->setFake(a, i);
				break;
			case DataSourceMark6:
				V->setMark6Files(a, i, dss.basebandFiles);
				break;
			case DataSourceUnspecified:
				if(!A->vsns.empty())
				{
					V->setDataSource(a, i, DataSourceModule);
				}
				break;
			case NumDataSources:
				std::cerr << "Developer error: mergeCorrParams: DataSource=NumDataSources encountered." << std::endl;
				exit(EXIT_FAILURE);
			}

			if(dss.dataSampling != NumSamplingTypes)
			{
				V->setSampling(A->name, i, dss.dataSampling);
			}
		}
	}

	// Tones
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		const VexAntenna *A;

		A = V->getAntenna(a);
		if(!A)
		{
			std::cerr << "Developer error: mergeCorrParams: Antenna number " << a << " cannot be gotten even though nAntenna() reports " << V->nAntenna() << std::endl;

			exit(EXIT_FAILURE);
		}


		const AntennaSetup *as = params.getAntennaSetup(A->name);
		if(!as)
		{
			// No antenna setup implies doing "smart" tone extraction (-1.0 implies 1/8 band guard)
			V->selectTones(A->name, ToneSelectionSmart, -1.0);

			continue;
		}

		if(as->toneSelection == ToneSelectionNone)
		{
			// change to having no injected tones
			V->setPhaseCalInterval(A->name, -1);

			continue;
		}

		if(as->phaseCalIntervalMHz >= 0)
		{
			// this sets phase cal interval and removes tones that are not multiples of it
			// interval = 0 implies no pulse cal
			V->setPhaseCalInterval(A->name, as->phaseCalIntervalMHz);
		}

		if(as->toneSelection != ToneSelectionVex)
		{
			V->selectTones(A->name, as->toneSelection, as->toneGuardMHz);
		}
	}

	// Override clocks and other antenna parameters
	for(unsigned int a = 0; a < V->nAntenna(); ++a)
	{
		const VexAntenna *A;

		A = V->getAntenna(a);
		if(!A)
		{
			std::cerr << "Developer error: mergeCorrParams: Antenna number " << a << " cannot be gotten even though nAntenna() reports " << V->nAntenna() << std::endl;

			exit(EXIT_FAILURE);
		}

		const AntennaSetup *as = params.getAntennaSetup(A->name);
		if(!as)
		{
			continue;
		}

		const VexClock *paramClock = params.getAntennaClock(A->name);
		if(paramClock)
		{
			V->setClock(A->name, *paramClock);
		}

		if(as->X != ANTENNA_COORD_NOT_SET || as->Y != ANTENNA_COORD_NOT_SET || as->Z != ANTENNA_COORD_NOT_SET)
		{
			if(as->X == ANTENNA_COORD_NOT_SET || as->Y == ANTENNA_COORD_NOT_SET || as->Z == ANTENNA_COORD_NOT_SET)
			{
				std::cerr << std::endl;
				std::cerr << "Error: Antenna " << A->name << " has some antenna position coordinates set but not all three.  When explicitly setting coordinates all three of X, Y and Z must be provided." << std::endl;
				++nError;
			}
			V->setAntennaPosition(A->name, as->X, as->Y, as->Z);
		}

		if(as->tcalFrequency != 0)
		{
			V->setTcalFrequency(A->name, as->tcalFrequency);
		}

		if(as->axisOffset != AXIS_OFFSET_NOT_SET)
		{
			V->setAntennaAxisOffset(A->name, as->axisOffset);
		}

		V->setAntennaPolConvert(A->name, as->polConvert);
	}

	// remove any datastreams with no data source
	V->removeStreamsWithNoDataSource();

	return 0;
}
