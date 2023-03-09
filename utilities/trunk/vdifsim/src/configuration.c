#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
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


Configuration *loadConfigration(const char *filename)
{
	const int MaxLineLength = 1024;
	Configuration *config;
	FILE *in;
	int lineNumber = 0;
	Feature *f;

	config = newConfiguration();
	f = config->features;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open '%s' for read.\n", filename);

		return 0;
	}

	/* FIXME: parse file */
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
			f->type = Feature_Gaussian;
			f->freq = p[0];
			f->width = p[1];
			f->fluxDensity = p[2];
			++f;
			++config->nFeature;
		}
		else if(strcasecmp(typeStr, "Sinc") == 0)
		{
			if(n != 4)
			{
				fprintf(stderr, "%s line %d: Sinc needs <freq> <width> <flux density>\n", filename, lineNumber);
				break;
			}
			f->type = Feature_Sinc;
			f->freq = p[0];
			f->width = p[1];
			f->fluxDensity = p[2];
			++f;
			++config->nFeature;
		}
		else if(strcasecmp(typeStr, "Triangle") == 0)
		{
			if(n != 4)
			{
				fprintf(stderr, "%s line %d: Triangle needs <freq> <width> <flux density>\n", filename, lineNumber);
				break;
			}
			f->type = Feature_Triangle;
			f->freq = p[0];
			f->width = p[1];
			f->fluxDensity = p[2];
			++f;
			++config->nFeature;
		}
		else if(strcasecmp(typeStr, "Box") == 0)
		{
			if(n != 4)
			{
				fprintf(stderr, "%s line %d: Box needs <freq> <width> <flux density>\n", filename, lineNumber);
				break;
			}
			f->type = Feature_Box;
			f->freq = p[0];
			f->width = p[1];
			f->fluxDensity = p[2];
			++f;
			++config->nFeature;
		}
		else if(strcasecmp(typeStr, "Tone") == 0)
		{
			if(n != 3)
			{
				fprintf(stderr, "%s line %d: Tone needs <freq> <flux>\n", filename, lineNumber);
				break;
			}
			f->type = Feature_Tone;
			f->freq = p[0];
			f->flux = p[1] * 1e9;
			++f;
			++config->nFeature;
		}
		else
		{
			fprintf(stderr, "%s line %d : Unknown feature type: %s\n", filename, lineNumber, typeStr);
			break;
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
			const Feature *f;

			f = config->features + i;
			switch(f->type)
			{
			case Feature_Gaussian:
				printf("  Gaussian:\n");
				printf("    Center = %f MHz\n", f->freq);
				printf("    FWHM = %f MHz\n", f->width);
				printf("    Peak flux density = %f Jy\n", f->fluxDensity);
				break;
			case Feature_Sinc:
				printf("  Sinc:\n");
				printf("    Center = %f MHz\n", f->freq);
				printf("    Width = %f MHz\n", f->width);
				printf("    Peak flux density = %f Jy\n", f->fluxDensity);
				break;
			case Feature_Triangle:
				printf("  Triange:\n");
				printf("    Center = %f MHz\n", f->freq);
				printf("    Width = %f MHz\n", f->width);
				printf("    Peak flux density = %f Jy\n", f->fluxDensity);
				break;
			case Feature_Box:
				printf("  Box:\n");
				printf("    Center = %f MHz\n", f->freq);
				printf("    Width = %f MHz\n", f->width);
				printf("    Flux density = %f Jy\n", f->fluxDensity);
				break;
			case Feature_Tone:
				printf("  Tone:\n");
				printf("    Freq = %f MHz\n", f->freq);
				printf("    Flux = %f Jy.GHz\n", f->fluxDensity * 1e-9);
				break;
			default:
				printf("  Unknown feature %d\n", f->type);
				break;
			}
		}
	}
}
