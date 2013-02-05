/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken / Adam Deller               *
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
 * $Id: optscans.cpp 4563 2012-05-18 04:06:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/optscans.cpp $
 * $LastChangedRevision: 4563 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2012-05-17 22:06:06 -0600 (Thu, 17 May 2012) $
 *
 *==========================================================================*/

#include <cstdlib>
#include <cmath>
#include <sstream>
#include <time.h>
#include <string.h>
#include "evladefaults.h"
#include "optscans.h"

using namespace std;

void optscans::open(const string& antennaName, const VexData *V)
{
	string extension;

	evlaIntSec     = DEFAULT_EVLA_INT_SEC;
	evlasbBits     = DEFAULT_EVLA_SB_BITS;
	evlasbChan     = DEFAULT_EVLA_SB_CHAN;
	evlaVCIDir     = DEFAULT_EVLA_VCI_DIR;
	evlaVCIVersion = DEFAULT_EVLA_VCI_VER;
	ant = antennaName;
	lastValid = 0.0;
	lastSourceId = -1;
	lastModeId = -1;
	lastChannelSet = -1;
	obsCode = V->getExper()->name;
	if(obsCode == "")
	{
		obsCode = "Unknown";
	}
	for(int i = 0; i < 4; ++i)
	{
		sw[i] = "";
	}
	mjd0 = V->obsStart();
	
	extension = ".scans";

	fileName = string(obsCode) + string(".") + antennaName + extension;
	ofstream::open(fileName.c_str());
}

void optscans::addPhasingSource(const string &sourceName)
{
	phasingSources.push_back(sourceName);
}

void optscans::close()
{
	ofstream::close();
}

int optscans::writeHeader(const VexData *V)
{

}

int optscans::writeComment(const string &commentString)
{
	*this << "# " << commentString << endl;
	*this << endl;

	return 0;
}

int optscans::writeScans(const VexData *V)
{
	int p;
	int n = 0;
	int nScan;
	map<string,VexIF>::const_iterator it;
	double recordSeconds = 0.0;
    double day = floor(mjd0);
    double allsec = floor((mjd0-floor(mjd0))*86400.0 + 0.5);
	double lastStop = 0.0;

	int daysinmonths[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	int doy = atoi((char*)(&V->vexStartTime[5]));
	int year = atoi((char*)(&V->vexStartTime[0]));
	int month=1;
	if( year%400 == 0 || (year%100 != 0 && year%4 == 0) ) {
		daysinmonths[1]++; // leap year
	}
	while( doy > daysinmonths[month-1] ) {
		doy	-= daysinmonths[month-1];
		month++;
	}

	*this << "SRC-CAT; " << obsCode << "." << ant << ".sources;" << endl;
	*this << "HDWR-CAT; " << obsCode << "." << ant << ".resources;" << endl;
	*this << "SCHED-BLOCK; " << obsCode << ";Fixed;1;" 
		<< (char*)(&V->vexStartTime[0]) << "-"
		<< ((month<10)?"0":"") << month << "-"
		<< ((doy<10)?"0":"") << doy << ";"
		<< (char*)(&V->vexStartTime[9]) << ":"
		<< (char*)(&V->vexStartTime[12]) << ":"
		<< (char*)(&V->vexStartTime[15])
		<< "; ; ; ; ; ; ; ; ;" << endl << endl;

	nScan = V->nScan();
cerr << " num of scans: " << nScan << endl;
	p = precision();
	precision(14);

	*this << "# <scan name>; <source>; <resource>; <time type>; <time>; <wrap>; "
			<< "<applyRefPtg>; <applyPhase>; <record>; <overTop>; <intents>; <comments>;" << endl;

	for(int s = 0; s < nScan; ++s)
	{	
		const VexScan *scan = V->getScan(s);
		*this << "# Scan " << s << " = " << scan->defName << endl;
		if(scan->stations.count(ant) == 0)
		{
			*this << "# Antenna " << ant << " not in scan " << scan->defName << endl;
		}
		else
		{
			const VexInterval *arange = &scan->stations.find(ant)->second;

			int modeId = V->getModeIdByDefName(scan->modeDefName);
			const VexMode* mode = V->getModeByDefName(scan->modeDefName);
			if(mode == 0)
			{
				cerr << "Error: scan=" << scan->defName << " ant=" << ant << " mode=" << scan->modeDefName << " -> mode=0" << endl;
				continue;
			}
			
			const VexSetup* setup = mode->getSetup(ant);

			if(setup == 0)
			{
				cerr << "Error: scan=" << scan->defName << " ant=" << ant << " mode=" << scan->modeDefName << " -> setup=0" << endl;
				continue;
			}

			*this << "# Intent: " << scan->intent << endl;
//			cerr << "intent: " << scan->intent << endl;
			int start = 0;
			int stop = 0;
			string intent = scan->intent;
			while( intent.find(",", start) != string::npos && intent.find(",", start+1) != string::npos ) {
				stop = intent.find(",", start+1);
				string tmp = intent.substr(start, stop);
//				cerr << "start: " << start << " stop: " << stop << " tmp: " << tmp << endl;
				if( tmp.find("VLA:") == string::npos ) {
					intent = intent.substr(0, start) + intent.substr(stop, string::npos);
				}
//				cerr << "intent: " << scan->intent << endl << " new: " << intent << endl;
				start = stop;
			}

			// TODO Once we control antenna movements, make sure we do not include antenna movement in
			// the setup scan if end time of first scan has already elapsed, so we do not interfere with
			// movement of antenna for subsequent scans. We still must execute all other setups steps.
			int    hour;
			int    min;
			int    sec;
			double deltat1 = floor((arange->mjdStart-mjd0)*86400.0 + 0.5);
			double deltat2 = floor((arange->mjdStop-mjd0)*86400.0 + 0.5);

			bool applyRefPtg = ((scan->intent.find("VLA:REFERENCE_POINTING_APPLY") != string::npos)
								&& (scan->intent.find("VLA:REFERENCE_POINTING_OFF") == string::npos))?true:false;
            if( intent.find("VLA:REFERENCE_POINTING_APPLY") != string::npos ) {
                int p = intent.find("VLA:REFERENCE_POINTING_APPLY");
                intent = intent.substr(0, p) + intent.substr(p+28, string::npos);
            }
			bool applyPhase = (scan->intent.find("VLA:AUTOPHASE_APPLY") != string::npos)?true:false;
            if( intent.find("VLA:AUTOPHASE_APPLY") != string::npos ) {
                int p = intent.find("VLA:AUTOPHASE_APPLY");
                intent = intent.substr(0, p) + intent.substr(p+20, string::npos);
            }
			bool adjustRefPtg = (scan->intent.find("VLA:REFERENCE_POINTING_ADJUST") != string::npos)?true:false;
            if( intent.find("VLA:REFERENCE_POINTING_ADJUST") != string::npos ) {
                int p = intent.find("VLA:REFERENCE_POINTING_ADJUST");
                intent = intent.substr(0, p) + intent.substr(p+38, string::npos);
            }
			bool   nonrec = !(scan->recordEnable.find(ant)->second);

			cerr << "ant: " << ant << " scan: " << scan->defName << " lastStop: " << lastStop
					<< " deltat1: " << deltat1 << " deltat2: " << deltat2 << endl;
			cerr << "# total length: " << (deltat2-lastStop) << endl;
			// pointing scans
			if(scan->intent.find("VLA:REFERENCE_POINTING_DETERMINE") != string::npos
				|| scan->intent.find("VLA:REFERENCE_POINTING_ADJUST") != string::npos) {
				// PTG; <scan>; <source>; <resource>; <timeType>; <timing>; <wrap>; <applyRefPtg>; <applyPhase>; <record>; <overTop>; <comments>
				//  PTG; X Ptg; J2345+0123; X Pointing; Duration (LST); 00:05:00; ; n; n; n; ;
				hour = (int)floor( (deltat2-lastStop) / 3600);
				min  = (int)floor(((deltat2-lastStop)-(hour*3600)) / 60);
				sec  = (int)floor( (deltat2-lastStop)-(hour*3600)-(min*60));
//				bool adjustRefPtg = (scan->intent.find("VLA:REFERENCE_POINTING_ADJUST") != string::npos)?true:false;

				*this << "PTG; " << optscans::obsCode << " " << scan->defName <<"; "
					<< scan->sourceDefName << "; " << "loif" << modeId << "; "
					<< " UTD; " << hour << "h" << min << "m" << sec << "s"
					<< "; ; " << ((adjustRefPtg)?"Y":"N") << "; N; N; ; ;" << endl << endl;
			}
			// phase up scans
			else if(scan->intent.find("VLA:AUTOPHASE_DETERMINE") != string::npos) {
				// insert non recording scan to break up scans for VLA's Mark5C
				hour = (int)floor( (deltat1-lastStop) / 3600);
				min  = (int)floor(((deltat1-lastStop)-(hour*3600)) / 60);
				sec  = (int)floor( (deltat1-lastStop)-(hour*3600)-(min*60));
				// dummy scan, or non recording scan to prevent one long scan
				*this << "STD; " << optscans::obsCode << " " << scan->defName << "NR; "
					<< scan->sourceDefName << "; " << "loif" << modeId << "; "
					<< " UTD; " << hour << "h" << min << "m" << sec << "s"
					<< "; ; " << ((applyRefPtg)?"Y":"N") << "; N; N; ; "
					<< "ObsTgt," << "; ;" << endl << endl;

				double subscanLen = 10.0;
				if(scan->intent.find("VLA:PHASE_SUBSCAN") != string::npos) {
					int start = scan->intent.find("VLA:PHASE_SUBSCAN") + 18; // pos points to number
					int end = scan->intent.find(",", start) - 1; // pos points to char after number
					if( start == string::npos || end == string::npos ) {
						cerr << "bad subscan format " << scan->intent << endl;
					} else {
						subscanLen = atoi(scan->intent.substr(start,end).c_str()); 
					}
//					cerr << "subscan: " << scan->intent.substr(start,end) << endl;
				} else {
					cerr << "subscan length (VLA:PHASE_SUBSCAN) not found - assuming 10sec length" << endl;
				}
//				cerr << "subscan length: " << subscanLen << endl;
				// figure out how many phase up scans we will do
				double scanLen = deltat2-deltat1;
//				double firstScan = (int)(deltat2-lastStop) % (int)subscanLen; // account for partial scans in numScans
				double firstScan = (int)(scanLen) % (int)subscanLen; // account for partial scans in numScans
//				int numScans = (int)(((firstScan==0.0?0:1)) + (deltat2-lastStop) / subscanLen);
				int numScans = (int)(((firstScan==0.0?0:1)) + (scanLen) / subscanLen);
				// check that we have enough sub scans for phase up
				if( ((scanLen) / subscanLen) < 4) {
					cerr << "Not enough scans for phase up!" << endl;
					exit(EXIT_FAILURE);
				}
				double scantime;
				*this << "# total length: " << (scanLen) << endl;
				for( int loop = 0; loop < numScans; loop++ ) {
					if( loop == 0 && firstScan != 0 )
						scantime = firstScan;
					else
						scantime = subscanLen;
					hour = (int)floor( (scantime) / 3600);
					min  = (int)floor(((scantime)-(hour*3600)) / 60);
					sec  = (int)floor( (scantime)-(hour*3600)-(min*60));
				
					// always use STD scans for phasing up
					*this << "STD; " << optscans::obsCode << " " << scan->defName << " PhaseUp " << loop << "; "
						<< scan->sourceDefName << "; " << "loif" << modeId << "; "
						<< " UTD; " << hour << "h" << min << "m" << sec << "s"
						<< "; ; " << ((applyRefPtg)?"Y":"N") << "; " 
						<< ((loop==0)?"N":"Y") << "; " 
						<< ((nonrec)?"N":"Y") << "; ; "
						<< "CALIBRATE_PHASE,DETERMINE_AUTOPHASE; ;" << endl << endl;
						//<scan name>; <source>; <resource>; <time type>; <time>; <wrap>; "
						// << "<applyRefPtg>; <applyPhase>; <record>; <overTop>; <intents>; <comments>;"
				}
			}
			// regular STD scans
			else {
//				bool   nonrec = !(scan->recordEnable.find(ant)->second);

				// insert non-recording scan between recording scans, to force OPT to stop the Mark5C between scans
				// don't do this for AUTOPHASE_OFF (aka: dummy) scans
				string dummyIntent;
/*				bool applyRefPtg = ((scan->intent.find("VLA:REFERENCE_POINTING_APPLY") != string::npos)
									&& (scan->intent.find("VLA:REFERENCE_POINTING_OFF") == string::npos))?true:false;
				bool applyPhase = (scan->intent.find("AUTOPHASE_APPLY") != string::npos)?true:false;
*/
//cerr << "## applyRefPtg: " << ((applyRefPtg)?"Y":"N") << " applyPhase: " << applyPhase << endl;
				// strip out intents that are not meant for OPT
				string newIntent = intent;
				if( intent.find("VLA:REFERENCE_POINTING_OFF") != string::npos ) {
					int p = intent.find("VLA:REFERENCE_POINTING_OFF");
/*cerr << "0: " << scan->intent << endl;
cerr << "1: " << scan->intent.substr(0, p) << endl;
cerr << "2: " << scan->intent.substr(p+27, string::npos) << endl;
*/
					newIntent = intent.substr(0, p) + intent.substr(p+27, string::npos);
				}
//cerr << "newintent: " << newIntent << endl;
				if( newIntent.find("VLA:AUTOPHASE_OFF") != string::npos ) {
					int p = newIntent.find("VLA:AUTOPHASE_OFF");
/*cerr << "0: " << newIntent << endl;
cerr << "1: " << newIntent.substr(0, p) << endl;
cerr << "2: " << newIntent.substr(p+18, string::npos) << endl;
*/
					newIntent = "ObsTgt," + newIntent.substr(0, p) + newIntent.substr(p+18, string::npos);
				}
//cerr << "newintent: " << newIntent << endl;
				if(intent.find("VLA:AUTOPHASE_OFF") == string::npos) {
					// insert non recording scan to break up scans for VLA's Mark5C
					hour = (int)floor( (deltat1-lastStop) / 3600);
					min  = (int)floor(((deltat1-lastStop)-(hour*3600)) / 60);
					sec  = (int)floor( (deltat1-lastStop)-(hour*3600)-(min*60));
					dummyIntent = "ObsTgt,";
					nonrec = true;
				} else {
					// dummy scan, takes up whole scan time
					hour = (int)floor( (deltat2-lastStop) / 3600);
					min  = (int)floor(((deltat2-lastStop)-(hour*3600)) / 60);
					sec  = (int)floor( (deltat2-lastStop)-(hour*3600)-(min*60));
					dummyIntent = newIntent;
					// dummy scan may or may not record
					nonrec = !(scan->recordEnable.find(ant)->second);
					if( !nonrec )
						recordSeconds += (deltat2-lastStop);
				}
//cerr << "## nonrec: " << nonrec << endl;

				// dummy scan, or non recording scan to prevent one long scan
				*this << "STD; " << optscans::obsCode << " " << scan->defName << ((nonrec)?"NR":"") << "; "
					<< scan->sourceDefName << "; " << "loif" << modeId << "; "
					<< " UTD; " << hour << "h" << min << "m" << sec << "s"
					<< "; ; " << ((applyRefPtg)?"Y":"N") << "; N; " << ((nonrec)?"N":"Y") << "; ; "
					<< dummyIntent << "; ;" << endl << endl;

				// actual recording scan
				if( intent.find("VLA:AUTOPHASE_OFF") == string::npos ) {
					// calculate scan times for our recording scan
					hour = (int)floor( (deltat2-deltat1) / 3600);
					min  = (int)floor(((deltat2-deltat1)-(hour*3600)) / 60);
					sec  = (int)floor( (deltat2-deltat1)-(hour*3600)-(min*60));
					*this << "STD; " << optscans::obsCode << " " << scan->defName << "; "
						<< scan->sourceDefName << "; " << "loif" << modeId << "; " 
						<< " UTD; " << hour << "h" << min << "m" << sec << "s"
						<< "; ; " << ((applyRefPtg)?"Y":"N") << "; "
						<< ((applyPhase)?"Y":"N") << "; " 
						<< ((scan->nRecordChan(V, ant) == 0)?"N":"Y") << "; ; ObsTgt,";
					if( intent.find("AUTOPHASE_DETERMINE") != string::npos )
						 *this << ",CALIBRATE_PHASE," << intent;
					*this <<"; ;" << endl;
					recordSeconds += (deltat2-deltat1);
				}
			}
		lastStop = deltat2;
		}
		*this << endl;
	}

	cout << "There are " << static_cast<int>(recordSeconds) << " seconds of recording at " << ant << endl;

	precision(p);

	return n;
}
