#include <stdio.h>
#include "vdifio.h"

int ymd2doy(int yr, int mo, int day)
{
        int monstart1[] = {0,31,59,90,120,151,181,212,243,273,304,334};
        int monstart2[] = {0,31,60,91,121,152,182,213,244,274,305,335};
        int L2;

        L2 = yr/4-(yr+7)/4-yr/100+(yr+99)/100+yr/400-(yr+399)/400;
        if(L2 == -1)
        {
                return day + monstart2[mo-1];
        }
        else
        {
                return day + monstart1[mo-1];
        }
}

int ymd2mjd(int yr, int mo, int day)
{
        int doy;
        int yr1 = yr - 1;

        doy = ymd2doy(yr, mo, day);

        return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}

int parse_vdif_header(char * rawheader, vdif_header * parsedheader)
{
	printf("Not yet implemented");
	return -1;
}

int getVDIFThreadID(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)((headerword >> 16) & 0x3FF);
}

int getVDIFFrameBytes(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[2];
	return (int)(headerword & 0xFFFFFF)*8;
}

int getVDIFStationID(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)(headerword & 0xFFFF);
}

int getVDIFBitsPerSample(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)(((headerword >> 26) & 0x1F) + 1);
}

int getVDIFNumChannels(char * rawheader)
{
	int numchans, i;
	unsigned int headerword = ((unsigned int*)rawheader)[2];
	unsigned int logchans = ((headerword >> 26) & 0x1F);

	numchans = 1;
	for(i=0;i<logchans;i++)
	{
		numchans *= 2;
	}
	return numchans;
}

int getVDIFFrameMJD(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[1];
        int epoch = (int)((headerword >> 24) & 0x3F);
	int mjd = ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
	headerword = ((int*)rawheader)[0];
	int seconds = (int)(headerword & 0x3FFFFFFF);
	return mjd + seconds/86400;
}

int getVDIFFrameSecond(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[0];
        return (int)((headerword & 0x3FFFFFFF)%86400);
}

int getVDIFFrameNumber(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[1];
        return (int)(headerword & 0xFFFFFF);
}


