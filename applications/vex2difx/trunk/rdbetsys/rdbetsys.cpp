#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <glob.h>
#include <string>

const char defaultSwitchedPowerPath[] = "/home/wbrisken/bd152i0";

void mjd2yearday(int mjd, int *year, int *doy)
{
	time_t t;
	struct tm tt;

	t = (mjd-40587LL)*86400LL;
	gmtime_r(&t, &tt);

	if(year)
	{
		*year = tt.tm_year+1900;
	}
	if(doy)
	{
		*doy = tt.tm_yday+1;
	}	
}

double filename2fracday(const char *fn)
{
	char *f;
	int start = -1;
	int num[5];
	int n = 0;
	double v = -1.0;

	f = strdup(fn);

	for(int i = 0; f[i] && n < 5; i++)
	{
		if(f[i] == '_' || f[i] == '.')
		{
			f[i] = 0;
			if(start >= 0)
			{
				num[n] = atoi(f+start);
				n++;
			}
			start = i+1;
		}
	}

	if(n == 5)
	{
		v = num[2]/24.0 + num[3]/1440.0 + num[4]/86400.0;
	}

	free(f);

	return v;
}

std::string genFileList(const char *switchedPowerPath, const char *stn, double mjd0, double mjd1)
{
	const int MaxFilenameLength = 256;
	std::string fileList = "";
	glob_t globbuf;

	for(int mjd = static_cast<int>(mjd0); mjd < mjd1; mjd++)
	{
		int year, doy;
		char match[MaxFilenameLength];

		mjd2yearday(mjd, &year, &doy);
		snprintf(match, MaxFilenameLength, "%s/%s_%d_%03d_*.switched_power.gz", switchedPowerPath, stn, year, doy);
		printf("Matching against: %s\n", match);

		if(glob(match, 0, 0, &globbuf) == 0)
		{
			if(globbuf.gl_pathc > 0)
			{
				double mjdStart, mjdEnd;

				mjdEnd = filename2fracday(globbuf.gl_pathv[0]) + mjd;
				for(unsigned int i = 0; i < globbuf.gl_pathc; i++)
				{
					mjdStart = mjdEnd;
					if(i+1 < globbuf.gl_pathc)
					{
						mjdEnd = filename2fracday(globbuf.gl_pathv[i+1]) + mjd;
					}
					else
					{
						mjdEnd = 1.0 + mjd;
					}

					printf(" -> %s %f %f\n", globbuf.gl_pathv[i], mjdStart, mjdEnd);
					if(mjdStart >= mjd0 || mjdEnd < mjd1)
					{
						printf("*\n");
						if(fileList.length() > 0)
						{
							fileList += " ";
						}
						fileList += globbuf.gl_pathv[i];
					}

				}
			}
			globfree(&globbuf);
		}
	}

	return fileList;
}


int main(int argc, char **argv)
{
	std::string fl;

	fl = genFileList(defaultSwitchedPowerPath, "la", 55862.2, 55863.3);

	printf("FL=%s\n", fl.c_str());

	return EXIT_SUCCESS;
}
