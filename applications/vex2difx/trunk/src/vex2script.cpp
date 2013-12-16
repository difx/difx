/***************************************************************************
 *   Copyright (C) 2009-2013 by Walter Brisken, Adam Deller, Matthias Bark *
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
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <fstream>
#include <cstring>
#include <cstdlib>
#include <errno.h>
#include "pystream.h"
#include "corrparams.h"
#include "vexload.h"
#include <stdio.h>

using namespace std;

extern char **environ;

const string program("vex2script");
const string version("0.25");
const string verdate("20131213");
const string author("Walter Brisken, Adam Deller, Matthias Bark");

#ifndef HAVE_GETLINE
extern "C" {
  ssize_t getline(char **lineptr, size_t *n, FILE *stream);
}
#endif

static void usage(int argc, char **argv, pystream *py)
{
	cout << endl;
	cout << program << " version " << version << "  " << author << " " << verdate << endl;
	cout << endl;
	cout << "Usage:  %s <vexfile> [options]" << endl;
	cout << endl;
	cout << "The first argument must be the name of a valid vex file." << endl;
	cout << endl;
	cout << "The optional arguments can be:" << endl;
	cout << "  --phasingsources=source1,source2... (for EVLA)" << endl;
	cout << "  --dbepersonality=[path/]filename (for VLBA)" << endl;
	cout << "  --mark5a  Set up DBE as PFB, but don't record" << endl;
    cout << "  -gb, for GB script only" << endl;
	cout << endl;
	cout << "Default Personalities: PFB:" << py->pfbName << " DDC:" << py->ddcName << " VDIF:" << py->vdifName << endl;
	cout << endl;
}

bool isVLBA(const string& antName)
{
	const string VLBAantennas[] = 
		{"BR", "FD", "HN", "KP", "LA", "MK", "NL", "OV", "PT", "SC", ""};	// terminate list with "" !

	for(unsigned int i = 0; VLBAantennas[i] != ""; ++i)
	{
		if(VLBAantennas[i] == antName)
		{
			return true;
		}
	}

	return false;
}

bool isEVLA(const string& ant)
{
	if(ant == "Y" || ant == "Y27" || ant == "Y1")
	{
		return true;
	}

	return false;
}

bool isGBT(const string& ant)
{
	if(ant == "GB")
	{
		return true;
	}
	
	return false;
}

pystream::DataFormat getDataFormat(const VexData *V, const string &antName)
{
	int nMode = V->nMode();
	pystream::DataFormat f = pystream::FORMAT_NONE;

	for(int m = 0; m < nMode; ++m)
	{
		const VexSetup *setup = V->getMode(m)->getSetup(antName);
		if(setup)
		{
			pystream::DataFormat g;

			if(setup->formatName.empty() || setup->formatName == "NONE")
			{
				continue;
			}

			if(setup->formatName == "MARK5B")
			{
				g = pystream::FORMAT_MARK5B;
			}
			else if(setup->formatName == "KVN5B")
			{
				g = pystream::FORMAT_KVN5B;
			}
			else if(setup->formatName.substr(0, 4) == "VDIF")
			{
				g = pystream::FORMAT_VDIF;
			}
			else if(setup->formatName.substr(0, 4) == "VLBA")
			{
				g = pystream::FORMAT_VLBA;
			}
			else
			{
		cout << "Format unknown: <" << setup->formatName << ">" << endl;
				g = pystream::FORMAT_UNKNOWN;
			}

			if(f == pystream::FORMAT_NONE)
			{
				f = g;
			}
			else if(f != g)
			{
				return pystream::FORMAT_MIXED;
			}
		}
	}

	return f;
}

void readFiles(char *program, char *pfb, char *ddc, char *vdif)
{
	FILE *f = fopen("vex2script.files", "r");
	char *string = NULL;
	char *file;
	size_t size;
	ssize_t read;
	char cmd[100] = "dirname ";
	char *path = NULL;
	char fullPath[100];
	int len;

	strcpy(pfb,  "<unknown>");
	strcpy(ddc,  "<unknown>");
	strcpy(vdif, "<unknown>");

	// look for path to personality file list
	path = getenv("PERSONALITYFILEPATH");
	if( path == NULL ) {
		printf("Can't read PERSONALITYFILEPATH from environment to get default personality names! Check that PERSONALITYFILEPATH is set correctly.\n");
		printf("Exiting!\n");
		exit(-1);
	}

	strcpy(fullPath, path);
	len = strlen(path);
	if( fullPath[len-1] != '/' ) {
		fullPath[len] = '/';
		fullPath[len+1] = '\0';
	}
	len = strlen(fullPath);
	strcpy(&(fullPath[len]), "vex2script.files");
	
	string = NULL;
	f = fopen(fullPath, "r");
	if( f == NULL ) {
		printf("Can't read vex2script.files to get default personality names! Check that PERSONALITYFILEPATH is set correctly.\n");
		printf("errno: %i\nPERSONALITYFILEPATH=%s\nExiting!\n", errno, fullPath);
		exit(-1);
	}
	while ( (read = getline(&string, &size, f)) != -1 ) {
		if( string[0] == '#' ) {
			continue;
		}
		file = strchr(string, '=');
		*file = '\0';
		file++;
		file[strlen(file)-1] = '\0';
		if( strcmp(string, "pfb") == 0)
			strcpy(pfb, file);
		if( strcmp(string, "ddc") == 0)
			strcpy(ddc, file);
		if( strcmp(string, "vdif") == 0)
			strcpy(vdif, file);
	}
	if( string )
		free(string);
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	const VexAntenna *A;
	int nAntenna, nScan, atchar, lastchar;
	pystream py;
	pystream::ScriptType sType = pystream::SCRIPT_VLBA;
	int nWarn = 0;
	bool gb_only = false;

	readFiles(argv[0], py.pfbName, py.ddcName, py.vdifName);

	if(argc < 2 || strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
	{
		usage(argc, argv, &py);

		return EXIT_SUCCESS;
	}

	printf("Default Personalities: PFB <%s> DDC <%s> VDIF <%s>\n", py.pfbName, py.ddcName, py.vdifName);

	P = new CorrParams();
	P->vexFile = string(argv[1]);
	P->defaultSetup();
	P->minSubarraySize = 1;

	for(int count = 2; count < argc; ++count)
	{
		//this is an interim measure to set the phasing sources for EVLA
		if(strncmp(argv[count], "--phasingsources=", 17) == 0)
		{
			atchar = 17;
			lastchar = 17;
			while(argv[count][atchar] != '\0')
			{
				if(argv[count][atchar] == ',')
				{
					argv[count][atchar] = '\0';
					py.addPhasingSource(string(argv[count]+lastchar));
					++atchar;
					lastchar = atchar;
				}
				++atchar;
			}
			py.addPhasingSource(string(argv[count]+lastchar));
		}
		else if(strncmp(argv[count], "--dbepersonality=", 17) == 0)
		{
			atchar = 17;
			lastchar = 17;
			while(argv[count][atchar] != '\0')
			{
				++atchar;
			}
			py.setDBEPersonality(string(argv[count]+lastchar));
		}
		else if(strcasecmp(argv[count], "--mark5A") == 0)
		{
			py.setMark5A(true);
			//py.setDBEPersonalityType(pystream::RDBE_PFB);
			py.setRecorderType(pystream::RECORDER_NONE);
		}
		else if(strncmp(argv[count], "-gb", 3) == 0) 
		{
			gb_only = true;
		}
		else
		{
			cout << "Ignoring argument " << argv[count] << endl;
		}
	}	

	V = loadVexFile(*P, &nWarn);

	if(V == NULL)
	{
		cerr << "ERROR loading vex file! File may not exist!" << endl;

		exit(EXIT_FAILURE);
	}

	nAntenna = V->nAntenna();
	nScan = V->nScan();

	cout << "nAntenna = " << nAntenna << "  nScan = " << nScan << endl;

	for(int a = 0; a < nAntenna; ++a)
	{
		A = V->getAntenna(a);

		if(isEVLA(A->name))
		{
                    if (!gb_only)
                    {
			cout << "Skipping VLA antenna" << endl;
			continue;

			//cout << "VLA antenna " << a << " = " << A->name << endl;
			//sType = pystream::SCRIPT_EVLA;
		    }
		}
		else if(isGBT(A->name))
		{
			cout << "GBT antenna " << a << " = " << A->name << endl;
			sType = pystream::SCRIPT_GBT;
		}
		else if(isVLBA(A->name))
		{
	            if (!gb_only)
			cout << "VLBA antenna " << a << " = " << A->name << endl;
		    sType = pystream::SCRIPT_VLBA;
		}
		else 
		{
			cout << "Skipping unknown antenna " << A->name << endl;
			continue;
		}

		if (gb_only && (sType != pystream::SCRIPT_GBT))
		{
			continue;
		}

		pystream::DataFormat df = getDataFormat(V, A->name);
		if(df == pystream::FORMAT_UNKNOWN)
		{
			cout << "Antenna " << A->name << " has an unsupported data format(" << df << ").  Skipping." << endl;

			continue;
		}
		else if(df == pystream::FORMAT_MIXED)
		{
			cout << "Antenna " << A->name << " has multiple data formats specified.  Skipping." << endl;

			continue;
		}
		else if(df == pystream::FORMAT_VDIF)
		{
//			cout << "  Note: This is VDIF formatted and is still supported only experimentally" << endl;
		}

		py.open(A->name, V, sType);

		py.setDataFormat(df);
		py.writeHeader(V);
		py.writeComment(string("File written by ") + program + string(" version ") + version + string(" vintage ") + verdate);
		py.writeDbeInit(V);
		py.writeRecorderInit(V);
		if( sType != pystream::SCRIPT_GBT )
			py.writeXCubeInit();
		if(py.getDBEPersonalityType() == pystream::RDBE_DDC) {
			py.writeDDCLoifTable(V);
			py.writeDDCSourceTable(V);
			py.writeDDCScans(V);
		} else {
			py.writeLoifTable(V);
			py.writeSourceTable(V);
			py.writeScans(V);
		}

		py.close();
	}
	

	return EXIT_SUCCESS;
}

