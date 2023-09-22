#include "other.h"

/* return day of year given year, month, day */
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
