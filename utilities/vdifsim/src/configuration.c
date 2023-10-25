#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <regex.h>
#include "configuration.h"


/* keep up to date with enum FeatureType in configuration.h */
const char FeatureTypeStrings[][MAX_FEATURE_TYPE_STRING_LENGTH] =
{
	"Gaussian",
	"Sinc",
	"Triangle",
	"Box",
	"Tone",

	"!"		/* list terminator; corresponds to NumFeatureType */
};



/* Sample exerpt from configuration

# blank lines are comments
# from # to end of line is a comment
# additional whitespace is meaningless
# first word is "type", which is case insensitive

# Type    freq     width  fl.den.  flux  spec.ind.  # comment
Gaussian  8825.25  10.0   40                        # 10.0 MHz wide Gaussian centered at 8825.25 MHz, peak flux density of 40 Jy 
Gaussian  8855.25  10.0   40                        # 10.0 MHz wide Gaussian centered at 8855.25 MHz, peak flux density of 40 Jy 
Sinc      8879.15  5      20                        # 5 MHz wide Sinc centered at 8879.15 MHz, peak flux density of 20 Jy
FluxDensity               5                         # 5 Jy of broad-band flux density
Index     9000.00                         -1        # set spectral index to -1, with the reference FluxDensity set at 9000 MHz
Tone      8850.00                   10              # add a tone at 8850.0 MHz with flux of 10 Jy.GHz

# "Box" and "Triangle" are similar to "Gaussian"

 */


AntennaParameters *getAntennaParameters(Configuration *config, const char *antName)
{
	int p;

	for(p = 0; p < config->nAntParm; ++p)
	{
		if(strcasecmp(config->antParms[p].name, antName) == 0)
		{
			return config->antParms + p;
		}
	}

	return 0;
}

const AntennaParameters *getAntennaParametersConst(const Configuration *config, const char *antName)
{
	int p;

	for(p = 0; p < config->nAntParm; ++p)
	{
		if(strcasecmp(config->antParms[p].name, antName) == 0)
		{
			return config->antParms + p;
		}
	}

	return 0;
}


Configuration *loadConfigration(const char *filename)
{
	const int MaxLineLength = 1024;
	Configuration *config;
	FILE *in;
	int lineNumber = 0;
	Feature *F;
	AntennaParameters *A = 0;
	regex_t headingMatch;
	regmatch_t matchPtr[2];

	regcomp(&headingMatch, "\\[[a-zA-Z0-9]+\\]", REG_EXTENDED);

	config = newConfiguration();
	F = config->features;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open '%s' for read.\n", filename);

		return 0;
	}

	for(;;)
	{
		int i;
		double p[5];
		char *rv;
		char typeStr[MAX_FEATURE_TYPE_STRING_LENGTH];
		char line[MaxLineLength];
		int n;

		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			fclose(in);

			return config;
		}
		++lineNumber;
		for(i = 0; line[i]; ++i)
		{
			if(line[i] == '#' || line[i] == '\n' || line[i] == '\r')
			{
				line[i] = 0;
				break;
			}
		}
		if(line[0] == 0)
		{
			continue;
		}
		n = sscanf(line, "%s%lf%lf%lf%lf%lf", typeStr, p+0, p+1, p+2, p+3, p+4);
		if(n == 0 || typeStr[0] == 0)
		{
			continue;	/* must be a blank line or a pure comment */
		}
		if(regexec(&headingMatch, typeStr, 2, matchPtr, 0) == 0)
		{
			typeStr[matchPtr[0].rm_eo - 1] = 0;
			A = getAntennaParameters(config, typeStr + matchPtr[0].rm_so + 1);
			if(A == 0)
			{
				if(config->nAntParm < MAX_ANTENNA_PARAMETERS)
				{
					A = config->antParms + config->nAntParm;
					++config->nAntParm;
					snprintf(A->name, DIFXIO_NAME_LENGTH, "%s", typeStr + matchPtr[0].rm_so + 1);
				}
				else
				{
					fprintf(stderr, "Error: too many antennas in %s.  Max is %d\n", filename, MAX_ANTENNA_PARAMETERS);
					break;
				}
			}
		}
		else if(A)
		{
			if(strcasecmp(typeStr, "SEFD") == 0)
			{
				if(n != 2)
				{
					fprintf(stderr, "%s line %d: SEFD needs <rate>\n", filename, lineNumber);
					break;
				}
				A->SEFD = p[0];
			}
			else if(strcasecmp(typeStr, "DroppedPacketRate") == 0)
			{
				if(n != 2)
				{
					fprintf(stderr, "%s line %d: DroppedPacketRate needs <rate>\n", filename, lineNumber);
					break;
				}
				A->droppedPacketRate = p[0];
			}
			else if(strcasecmp(typeStr, "InvalidPacketRate") == 0)
			{
				if(n != 2)
				{
					fprintf(stderr, "%s line %d: InvalidPacketRate needs <rate>\n", filename, lineNumber);
					break;
				}
				A->invalidPacketRate = p[0];
			}
			else if(strcasecmp(typeStr, "ClockOffset") == 0)
			{
				if(n != 2)
				{
					fprintf(stderr, "%s line %d: ClockOffset needs <offset> (in microseconds)\n", filename, lineNumber);
					break;
				}
				A->clockOffset = p[0];
			}
			else if(strcasecmp(typeStr, "SwitchedPower") == 0)
			{
				if(n != 3)
				{
					fprintf(stderr, "%s line %d: SwitchedPower needs <Freq[Hz]> and <power[%%]>\n", filename, lineNumber);

					break;
				}
				A->switchedPowerFreq = (int)(p[0] + 0.5);
				A->switchedPowerFrac = p[1];
			}
			else if(strcasecmp(typeStr, "PulseCal") == 0)
			{
				if(n < 3 || n > 4)
				{
					fprintf(stderr, "%s line %d: PulseCal needs <Interval[MHz]> and <power[%%]>, and optionally <delay[us]>\n", filename, lineNumber);

					break;
				}
				A->pulseCalInterval = (int)(p[0] + 0.5);
				A->pulseCalFrac = p[1];
				if(n >= 4)
				{
					A->pulseCalDelay = p[2];
				}
			}
			else
			{
				fprintf(stderr, "%s line %d : Unknown antenna parameter: %s\n", filename, lineNumber, typeStr);
				break;
			}
		}
		else
		{
			if(strcasecmp(typeStr, "FluxDensity") == 0)
			{
				if(n != 2)
				{
					fprintf(stderr, "%s line %d: FluxDensity needs <flux density>\n", filename, lineNumber);
					break;
				}
				config->fluxDensity = p[0];
			}
			else if(strcasecmp(typeStr, "Index") == 0)
			{
				if(n != 3)
				{
					fprintf(stderr, "%s line %d: Index needs <ref freq> <spec index>\n", filename, lineNumber);
					break;
				}
				config->specIndexFreq = p[0];
				config->specIndex = p[1];
			}
			else if(strcasecmp(typeStr, "Gaussian") == 0)
			{
				if(n != 4)
				{
					fprintf(stderr, "%s line %d: Gaussian needs <freq> <width> <flux density>\n", filename, lineNumber);
					break;
				}
				F->type = Feature_Gaussian;
				F->freq = p[0];
				F->width = p[1];
				F->fluxDensity = p[2];
				++F;
				++config->nFeature;
			}
			else if(strcasecmp(typeStr, "Sinc") == 0)
			{
				if(n != 4)
				{
					fprintf(stderr, "%s line %d: Sinc needs <freq> <width> <flux density>\n", filename, lineNumber);
					break;
				}
				F->type = Feature_Sinc;
				F->freq = p[0];
				F->width = p[1];
				F->fluxDensity = p[2];
				++F;
				++config->nFeature;
			}
			else if(strcasecmp(typeStr, "Triangle") == 0)
			{
				if(n != 4)
				{
					fprintf(stderr, "%s line %d: Triangle needs <freq> <width> <flux density>\n", filename, lineNumber);
					break;
				}
				F->type = Feature_Triangle;
				F->freq = p[0];
				F->width = p[1];
				F->fluxDensity = p[2];
				++F;
				++config->nFeature;
			}
			else if(strcasecmp(typeStr, "Box") == 0)
			{
				if(n != 4)
				{
					fprintf(stderr, "%s line %d: Box needs <freq> <width> <flux density>\n", filename, lineNumber);
					break;
				}
				F->type = Feature_Box;
				F->freq = p[0];
				F->width = p[1];
				F->fluxDensity = p[2];
				++F;
				++config->nFeature;
			}
			else if(strcasecmp(typeStr, "Tone") == 0)
			{
				if(n != 3)
				{
					fprintf(stderr, "%s line %d: Tone needs <freq> <flux>\n", filename, lineNumber);
					break;
				}
				F->type = Feature_Tone;
				F->freq = p[0];
				F->flux = p[1] * 1e9;
				++F;
				++config->nFeature;
			}
			else
			{
				fprintf(stderr, "%s line %d : Unknown feature type: %s\n", filename, lineNumber, typeStr);
				break;
			}
		}
	}

	/* must have hit some error */
	free(config);

	return 0;
}

Configuration *newConfiguration()
{
	Configuration *config;

	config = (Configuration *)calloc(1, sizeof(Configuration));

	return config;
}

void deleteConfiguration(Configuration *config)
{
	if(config)
	{
		free(config);
	}
}

void printConfiguration(const Configuration *config)
{
	printf("Configuration [%p]\n", config);
	if(config)
	{
		int i;

		for(i = 0; i < config->nAntParm; ++i)
		{
			const AntennaParameters *A;

			A = config->antParms + i;
			printf("  Antenna %s:\n", A->name);
			printf("    Dropped packet rate = %f\n", A->droppedPacketRate);
			printf("    Invalid packet rate = %f\n", A->invalidPacketRate);
			printf("    Clock offset = %f us\n", A->clockOffset);
			printf("    SEFD = %f Jy\n", A->SEFD);
			if(A->switchedPowerFrac > 0.0)
			{
				printf("    Switched power frequency = %d Hz\n", A->switchedPowerFreq);
				printf("    Switched power = %f%% of SEFD\n", A->switchedPowerFrac * 100.0);
			}
			if(A->pulseCalFrac > 0.0)
			{
				printf("    Pulse cal interval = %d MHz\n", A->pulseCalInterval);
				printf("    Pulse cal power = %f%% of SEFD\n", A->pulseCalFrac);
				printf("    Pulse cal delay = %f us\n", A->pulseCalDelay);
			}
		}

		if(config->fluxDensity)
		{
			printf("  Flux density = %f Jy\n", config->fluxDensity);
			if(config->specIndexFreq > 0 && config->specIndex != 0.0)
			{
				printf("    Spec index = %f\n", config->specIndex);
				printf("    Ref freq = %f MHz\n", config->specIndexFreq);
			}
		}
		for(i = 0; i < config->nFeature; ++i)
		{
			const Feature *F;

			F = config->features + i;
			switch(F->type)
			{
			case Feature_Gaussian:
				printf("  Gaussian:\n");
				printf("    Center = %f MHz\n", F->freq);
				printf("    FWHM = %f MHz\n", F->width);
				printf("    Peak flux density = %f Jy\n", F->fluxDensity);
				break;
			case Feature_Sinc:
				printf("  Sinc:\n");
				printf("    Center = %f MHz\n", F->freq);
				printf("    Width = %f MHz\n", F->width);
				printf("    Peak flux density = %f Jy\n", F->fluxDensity);
				break;
			case Feature_Triangle:
				printf("  Triange:\n");
				printf("    Center = %f MHz\n", F->freq);
				printf("    Width = %f MHz\n", F->width);
				printf("    Peak flux density = %f Jy\n", F->fluxDensity);
				break;
			case Feature_Box:
				printf("  Box:\n");
				printf("    Center = %f MHz\n", F->freq);
				printf("    Width = %f MHz\n", F->width);
				printf("    Flux density = %f Jy\n", F->fluxDensity);
				break;
			case Feature_Tone:
				printf("  Tone:\n");
				printf("    Freq = %f MHz\n", F->freq);
				printf("    Flux = %f Jy.GHz\n", F->fluxDensity * 1e-9);
				break;
			default:
				printf("  Unknown feature %d\n", F->type);
				break;
			}
		}
		for(i = 0; i < config->nAntParm; ++i)
		{
			const AntennaParameters *A;

			A = config->antParms + i;
			printf("  Antenna %s\n", A->name);
			printf("    Dropped packet rate = %f\n", A->droppedPacketRate);
			printf("    Invalid packet rate = %f\n", A->invalidPacketRate);
			printf("    Clock offset = %f us\n", A->clockOffset);
		}
	}
}
