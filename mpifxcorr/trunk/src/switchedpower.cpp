/***************************************************************************
 *   Copyright (C) 2010-2016 by Walter Brisken                             *
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
#include "switchedpower.h"
#include "configuration.h"

using namespace std;

SwitchedPower::SwitchedPower()
{
	init();
}

SwitchedPower::SwitchedPower(const Configuration * conf, int mpiid)
{
	init();

	datastreamId = mpiid-1;
	filepath = conf->getOutputFilename();
	startMJD = conf->getStartMJD();
	startSeconds = conf->getStartSeconds();
}

SwitchedPower::~SwitchedPower()
{
	close();
}

void SwitchedPower::init()
{
	frequency = 80;
	interval = 1;
	startsec = -1;
	datastreamId = -1;
	nchan = 0;
	counts = 0;
	opened = false;
	failed = false;
	nOn = nOff = highOn = highOff = 0;
}

int SwitchedPower::open()
{
	char switchedPowerFilename[256];

	sprintf(switchedPowerFilename, "%s/SWITCHEDPOWER_%05d_%06d_%d", filepath.c_str(), startMJD, startSeconds, datastreamId);

	output.open(switchedPowerFilename, ios::out | ios::app);
	if(!output.fail())
	{
		opened = true;
	}
	else
	{
		failed = true;
	}

	return 0;
}

int SwitchedPower::close()
{
	if(opened)
	{
		flush();

		output.close();

		opened = false;
	}

	realloc(0);

	return 0;
}

int SwitchedPower::realloc(int n)
{
	if(nchan == n)
	{
		return 0;
	}

	if(nchan > 0)
	{
		if(nOn) delete [] nOn;
		if(nOff) delete [] nOff;
		if(highOn) delete [] highOn;
		if(highOff) delete [] highOff;
		if(counts) delete [] counts;
	}
	nchan = n;
	if(nchan > 0)
	{
		nOn = new double[nchan];
		nOff = new double[nchan];
		highOn = new double[nchan];
		highOff = new double[nchan];
		counts = new unsigned int[nchan];

		for(int i = 0; i < nchan; i++)
		{
			highOn[i] = highOff[i] = 0.0;
			nOn[i] = nOff[i] = 0.0;
		}
	}

	return 0;
}

int SwitchedPower::flush()
{
	if(failed)
	{
		return -1;
	}
	if(nchan > 0)
	{
		if(!opened)
		{
			open();
		}
		if(nOn[0] > 0 && nOff[0] > 0 && startsec >= 0)
		{
			double mjd0 = startmjd + (startsec + startns*1.0e-9)/86400.0;
			double mjd1 = endmjd + (endsec + endns*1.0e-9)/86400.0;
			output.precision(14);
			output << mjd0 << " " << mjd1;
			output.precision(8);
			for(int i = 0; i < nchan; i++)
			{
				if(nOn[i] > 0.5 && nOff[i] > 0.5)
				{
					double powerOn, powerOff;
					double fOn, fOff;
					double dfOn, dfOff;
					double sigmaPowerOn, sigmaPowerOff;

					fOn = highOn[i]/nOn[i];
					fOff = highOff[i]/nOff[i];

					// from statistics of the binomial distribution
					dfOn = sqrt(fOn*(1.0-fOn)/nOn[i]);
					dfOff = sqrt(fOff*(1.0-fOff)/nOff[i]);

					powerOn = high_state_fraction_to_power(fOn);
					powerOff = high_state_fraction_to_power(fOff);

					// Poor man's error propagation
					sigmaPowerOn  = 0.5*(high_state_fraction_to_power(fOn+dfOn) -
					                     high_state_fraction_to_power(fOn-dfOn));
					sigmaPowerOff = 0.5*(high_state_fraction_to_power(fOff+dfOff) -
					                     high_state_fraction_to_power(fOff-dfOff));

					output << " " << powerOn << " " << sigmaPowerOn << " " << powerOff << " " << sigmaPowerOff;
				}
				else
				{
					output << " 0 0 0 0";
				}
			}
			output << endl;
		}
		for(int i = 0; i < nchan; i++)
		{
			highOn[i] = highOff[i] = 0.0;
			nOn[i] = nOff[i] = 0.0;
		}
	}

	startsec = -1;

	return 0;
}

// the feed function(s) is/are responsible for keeping startsec, endsec, and other state variables up to date.

int SwitchedPower::feed(mark5_stream *ms)
{
	int mjd, sec;
	double ns;
	int status;
	int phaseStart, sampleNumStart, nextPhaseStart;

	if(ms->count == 0)
	{
		return 0;
	}

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	
	if(nchan != ms->nchan || (sec / interval) != (startsec / interval) || mjd != startmjd)
	{
		flush();
		startmjd = mjd;
		startsec = sec;
		startns  = ns;

		realloc(ms->nchan);
	}

	endsec = sec;
	endmjd = mjd;

	sampleNumStart = static_cast<int>(ns*ms->samprate/1000000000);
	phaseStart = 0;

	// phase is number of half-cycles of switched power since beginning of the second in which the first sample started
	for(int phase = static_cast<int>(ns * frequency * 2e-9); ; phase++)
	{
		if(phase % (2*frequency) == 0 && ( (sec + phase/(2*frequency)) % interval == 0 || sec + phase/(2*frequency) == 86400) )
		{
			flush();
			startns = 0.0;
			startsec = sec + phase/(2*frequency);
			if(startsec >= 86400)
			{
				startsec -= 86400;
				startmjd++;
			}
		}

		nextPhaseStart = ms->samprate*(phase+1LL)/(2*frequency) - sampleNumStart;
		if(nextPhaseStart % ms->samplegranularity > 0)
		{
			nextPhaseStart += ms->samplegranularity-(nextPhaseStart % ms->samplegranularity) - sampleNumStart;
		}

		for(int i = 0; i < nchan; i++)
		{
			counts[i] = 0;
		}
		status = mark5_stream_count_high_states(ms, nextPhaseStart - phaseStart, counts);
		if(status > 0)
		{
			if(phase % 2 == 0)
			{
				for(int i = 0; i < nchan; i++)
				{
					highOn[i] += counts[i];
					nOn[i] += status;
				}
			}
			else
			{
				for(int i = 0; i < nchan; i++)
				{
					highOff[i] += counts[i];
					nOff[i] += status;
				}
			}
			endns = ns + (phaseStart + status)*1.0e9/ms->samprate;
		}
		else
		{
			break;
		}

		phaseStart = nextPhaseStart;
	}

	return 0;
}
