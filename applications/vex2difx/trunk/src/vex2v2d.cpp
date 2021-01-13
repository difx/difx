/***************************************************************************
 *   Copyright (C) 2021 by Walter Brisken                                  *
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
 * $Id: $
 * $HeadURL: $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <limits.h>
#include <vexdatamodel.h>
#include <vex_utility.h>
#include "testvex.h"

const char program[] = "vex2v2d";
const char version[] = "0.1";
const char verdate[] = "20210113";
const char author[] = "Walter Brisken";

const double defaultTInt = 2.0;		// [sec]
const double defaultSpecRes = 0.25;	// [MHz]

void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options] <vexFile> [ <tInt> [ <specRes> ] ]\n\n", pgm);
	fprintf(stderr, "<vexFile> is the full path to the vex file\n\n");
	fprintf(stderr, "<tInt> is the integration time (seconds).  Default = %f\n\n", defaultTInt);
	fprintf(stderr, "<specRes> is the spectral resolution (MHz).  Default = %f\n\n", defaultSpecRes);
	fprintf(stderr, "options can be one or more of the following:\n\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h       print this help info and quit\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v       be more verbose in execution\n\n");
	fprintf(stderr, "  --nopolar\n");
	fprintf(stderr, "  -n       don't form cross-polar products\n\n");
	fprintf(stderr, "  --polar\n");
	fprintf(stderr, "  -p       form cross-polar products [default]\n\n");
	fprintf(stderr, "  --machines\n");
	fprintf(stderr, "  -m       add information about VLBA machines and threads\n\n");
	fprintf(stderr, "  --split\n");
	fprintf(stderr, "  -s       specify the format as VLBA1032\n\n");
	fprintf(stderr, "  --VDIF\n");
	fprintf(stderr, "  -V       specify the format as VLBA5032\n\n");
	fprintf(stderr, "  --file\n");
	fprintf(stderr, "  -F       set up for file-based correlation\n\n");
	fprintf(stderr, "  --force\n");
	fprintf(stderr, "  -f       force execution, even if output file already exists\n\n");
	fprintf(stderr, "  -2       set up for two datastreams per antenna\n\n");
	fprintf(stderr, "  -4       set up for four datastreams per antenna\n\n");
	fprintf(stderr, "  -8       set up for eight datastreams per antenna\n\n");
}

const char *getDatastreamMachine(const std::string &ant)
{
	if(ant == "BR") 
	{
		return "swc001";
	}
	else if(ant == "FD")
	{
		return "swc002";
	}
	else if(ant == "GB")
	{
		return "swc011";
	}
	else if(ant == "HN")
	{
		return "swc003";
	}
	else if(ant == "KP")
	{
		return "swc004";
	}
	else if(ant == "LA")
	{
		return "swc005";
	}
	else if(ant == "MK")
	{
		return "swc006";
	}
	else if(ant == "NL")
	{
		return "swc007";
	}
	else if(ant == "OV")
	{
		return "swc008";
	}
	else if(ant == "PT")
	{
		return "swc009";
	}
	else if(ant == "SC")
	{
		return "swc010";
	}
	else
	{
		return "swc020";
	}
}

int write_v2d(const VexData *V, const char *vexFile, const char *outFile, bool force, bool doPolar, double tInt, double specRes, int nDatastream, bool doMachines, int vdifFrameSize, bool doFilelist)
{
	FILE *out;
	unsigned int nAntenna = V->nAntenna();
	unsigned int nSource = V->nSource();
	bool doDatastreams = false;
	std::string lexper = V->getExper()->name;
	Lower(lexper);

	if(vdifFrameSize > 0 || doFilelist || nDatastream > 1)
	{
		doDatastreams = true;
	}

	if(outFile == 0 || outFile[0] == 0)	// assume stdout
	{
		out = stdout;
	}
	else
	{
		out = fopen(outFile, "w");
	}
	if(!out)
	{
		return EXIT_FAILURE;
	}

	fprintf(out, "# base .v2d file generated by %s version %s on file %s\n\n", program, version, vexFile);
	fprintf(out, "vex = %s\n\n", vexFile);
	fprintf(out, "antennas =");
	for(unsigned int a = 0; a < nAntenna; ++a)
	{
		const VexAntenna *A = V->getAntenna(a);
		if(a == 0)
		{
			fprintf(out, " %s", A->name.c_str());
		}
		else
		{
			fprintf(out, ", %s", A->name.c_str());
		}
	}
	fprintf(out, "\n\n");
	if(doMachines)
	{
		fprintf(out, "machines = swc000, swc011, swc012, swc013, swc014, swc015, swc016, swc017, swc018, swc019, swc020\n");
		fprintf(out, "nCore = 10\n");
		fprintf(out, "nThread = 4\n\n");
	}
	fprintf(out, "delayModel = difxcalc\n\n");

	for(unsigned int s = 0; s < nSource; ++s)
	{
		const VexSource *S = V->getSource(s);

		fprintf(out, "SOURCE %s { }\n", S->defName.c_str());
	}
	fprintf(out, "\n");

	for(unsigned int a = 0; a < nAntenna; ++a)
	{
		const VexAntenna *A = V->getAntenna(a);
		const char *machine;
		std::string lname = A->name;
		Lower(lname);
		machine = getDatastreamMachine(A->name);

		if(doMachines && machine == 0)
		{
			fprintf(stderr, "Error: Machine mode was used for unsupported antenna: %s\n", A->name.c_str());

			exit(EXIT_FAILURE);
		}

		if(doDatastreams)
		{
			if(a > 0)
			{
				fprintf(out, "\n");
			}
			for(int d = 0; d < nDatastream; ++d)
			{
				fprintf(out, "DATASTREAM %s%d {", A->name.c_str(), d);
				if(vdifFrameSize > 0)
				{
					fprintf(out, " format=VDIF%d", vdifFrameSize);
				}
				if(doFilelist)
				{
					if(nDatastream > 1)
					{
						fprintf(out, " filelist=%s.%s%d.filelist", lexper.c_str(), lname.c_str(), d);
					}
					else
					{
						fprintf(out, " filelist=%s.%s.filelist", lexper.c_str(), lname.c_str());
					}
				}
				if(doMachines)
				{
					fprintf(out, " machine=%s", machine);
				}
				fprintf(out, " }\n");
			}
		}

		fprintf(out, "ANTENNA %s { toneSelection=smart", A->name.c_str());
		if(doMachines && !doDatastreams)
		{
			fprintf(out, " machine=%s", machine);
		}
		if(doDatastreams)
		{
			fprintf(out, " datastreams");
			for(int d = 0; d < nDatastream; ++d)
			{
				fprintf(out, "%c%s%d", (d == 0 ? '=' : ','), A->name.c_str(), d);
			}
		}
	
		fprintf(out, " }\n");
	}
	fprintf(out, "\n");

	fprintf(out, "SETUP default\n");
	fprintf(out, "{\n");
	fprintf(out, "  tInt = %f\n", tInt);
	fprintf(out, "  fftSpecRes = %f\n", specRes);
	fprintf(out, "  specRes = %f\n", specRes);
	fprintf(out, "  doPolar = %s\n", (doPolar ? "True" : "False") );
	fprintf(out, "  numBufferedFFTs = 10\n");
	fprintf(out, "  maxNSBetweenACAvg = 2000000\n");
	fprintf(out, "}\n");

	if(out != stdout)
	{
		fclose(out);
	}

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	VexData *V;
	int a;
	int v;
	unsigned int nWarn = 0;
	const char *vexFile = 0;
	char outFile[PATH_MAX] = "";
	char *ext;
	int verbose = 0;
	double tInt = 0.0;		// [sec]
	double specRes = 0.0;		// [MHz]
	bool force = false;
	bool doPolar = true;
	bool doMachines = false;
	bool doFilelist = false;
	bool doStdout = false;
	int nDatastream = 1;
	int vdifFrameSize = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		        strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else if(strcmp(argv[a], "-") == 0)
		{
			doStdout = true;
		}
		else if(strcmp(argv[a], "-n") == 0 ||
		        strcmp(argv[a], "--nopolar") == 0)
		{
			doPolar = false;
		}
		else if(strcmp(argv[a], "-p") == 0 ||
		        strcmp(argv[a], "--polar") == 0)
		{
			doPolar = true;
		}
		else if(strcmp(argv[a], "-m") == 0 ||
		        strcmp(argv[a], "--machines") == 0)
		{
			doMachines = true;
		}
		else if(strcmp(argv[a], "-s") == 0 ||
		        strcmp(argv[a], "--split") == 0)
		{
			vdifFrameSize = 1032;
		}
		else if(strcmp(argv[a], "-V") == 0 ||
		        strcmp(argv[a], "--VDIF") == 0)
		{
			vdifFrameSize = 5032;
		}
		else if(strcmp(argv[a], "-F") == 0 ||
		        strcmp(argv[a], "--file") == 0)
		{
			doFilelist = true;
		}
		else if(strcmp(argv[a], "-2") == 0)
		{
			nDatastream = 2;
		}
		else if(strcmp(argv[a], "-4") == 0)
		{
			nDatastream = 4;
		}
		else if(strcmp(argv[a], "-8") == 0)
		{
			nDatastream = 8;
		}
		else if(strcmp(argv[a], "-f") == 0 ||
		        strcmp(argv[a], "--force") == 0)
		{
			force = true;
		}
		else if(argv[a][0] == '-')
		{
			printf("Unknown option %s .  Run with -h for help.\n\n", argv[a]);

			return EXIT_FAILURE;
		}
		else if(specRes > 0.0)
		{
			printf("Error: too many non-optional parameters provided.\n\n");

			return EXIT_FAILURE;
		}
		else if(vexFile == 0)
		{
			vexFile = argv[a];
		}
		else if(tInt <= 0.0)
		{
			tInt = atof(argv[a]);
		}
		else
		{
			specRes = atof(argv[a]);
		}
	}

	if(vexFile == 0)
	{
		printf("No file name provided.  Run with -h for help.\n\n");

		return EXIT_FAILURE;
	}
	if(!doStdout)
	{
		strcpy(outFile, vexFile);
		ext = strstr(outFile, ".vex");
		if(!ext)
		{
			fprintf(stderr, "Error: the input filename should contain '.vex'\n");

			return EXIT_FAILURE;
		}
		strcpy(ext, ".v2d");
	}

	if(tInt <= 0.0)
	{
		tInt = defaultTInt;

		fprintf(stderr, "Setting tInt to default value of %f\n", tInt);
	}

	if(specRes <= 0.0)
	{
		specRes = defaultSpecRes;

		fprintf(stderr, "Setting specRes to default value of %f\n", specRes);
	}

	v = testVex(vexFile);
	if(v != 0)
	{
		fprintf(stderr, "Cannot parse vex file.  Error code = %d\n", v);

		return EXIT_FAILURE;
	}

	V = loadVexFile(std::string(vexFile), &nWarn);

	if(verbose)
	{
		std::cout << *V << std::endl;
		std::cout << std::endl;
	}

	v = write_v2d(V, vexFile, outFile, force, doPolar, tInt, specRes, nDatastream, doMachines, vdifFrameSize, doFilelist);

	if(outFile[0])
	{
		if(v == EXIT_SUCCESS)
		{
			fprintf(stderr, "\nOutput written to %s\n\n", outFile);
		}
		else
		{
			fprintf(stderr, "\nAn error occurred.\n\n");
		}
	}

	delete(V);

	return v;
}
