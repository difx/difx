#include <math.h>
#include <strings.h>
#include "antenna_db.h"

AntennaDBEntry antennaDBEntries[] =
{
	{ "Algonquin",        918034.4879,  -4346132.3267,   4561971.2292,  46.0 },
	{ "Apex",            2225039.5297,  -5441197.6292,  -2479303.3597,  12.0 },
	{ "Arecibo",         2390486.9000,  -5564731.4400,   1994720.4500,  25.0 },
	{ "Badary",          -838201.0685,   3865751.5652,   4987670.8885,  32.0 },
	{ "Brewster",       -2112065.2062,  -3705356.5048,   4726813.6759,  25.0 },
	{ "Cambridge",       3920356.1500,      2542.0200,   5014284.4200,  32.0 },
	{ "Ceduna",         -3753442.7457,   3912709.7530,  -3348067.6095,  30.0 },
	{ "DSS34",           -4461146.905,   2682439.289,   -3674393.334,   34.0 },
	{ "DSS43",          -4460894.7273,   2682361.5296,  -3674748.4238,  70.0 },
	{ "DSS45",          -4460935.9763,   2682765.7053,  -3674380.4619,  34.0 },
	{ "Effelsberg",      4033947.2616,    486990.7866,   4900430.9915,  25.0 },
	{ "Fort Davis",     -1324009.3266,  -5332181.9547,   3231962.3949,  25.0 },
	{ "GBT",             -882589.4102,  -4924872.3416,   3943729.4062,  100.0 },
	{ "Hart RAO 15",     5085490.8010,   2668161.4892,  -2768692.6239,  15.0 },
	{ "Hart RAO 26",     5085442.7637,   2668263.7913,  -2768696.7523,  26.0 },
	{ "Haystack",        1492404.4908,  -4457266.5294,   4296881.8536,  37.0 },
	{ "Hobart 12",      -3949990.7783,   2522421.2046,  -4311708.0242,  12.0 },
	{ "Hobart 26",      -3950237.3590,   2522347.6804,  -4311561.8790,  26.0 },
	{ "Irbene",          3183649.615,    1276902.735,    5359264.576,   32.0 },
	{ "Irbene 16",       3183295.000,    1276276.000,    5359611.000,   16.0 },
	{ "Iriki",          -3521719.7439,   4132174.6639,   3336994.1873,  20.0 },
	{ "Ishigaki",       -3263995.0986,   4808056.3516,   2619948.8797,  20.0 },
	{ "JCMT",           -5464584.676,   -2493001.1700,   2150653.982,   15.0 },
	{ "Jodrell Lovell",  3822626.0400,   -154105.6500,   5086486.0400,  76.0 },
	{ "Jodrell Mk2",     3822846.7600,   -153802.2800,   5086285.9000,  25.0 },
	{ "Kashima 11",     -3997506.4214,   3276877.6675,   3724240.3950,  11.0 },
	{ "Kashima 34",     -3997649.2415,   3276690.8602,   3724278.7239,  34.0 },
	{ "Katherine",      -4147354.6449,   4581542.3911,  -1573303.2196,  12.0 },
	{ "Kitt Peak",      -1995678.8402,  -5037317.6968,   3357328.0251,  25.0 },
	{ "Kunming",        -1281152.8016,   5640864.4199,   2682653.5123,  40.0 },
	{ "Los Alamos",     -1449752.5839,  -4975298.5757,   3709123.8459,  25.0 },
	{ "Matera",          4641938.4731,   1393003.3222,   4133325.7702,  20.0 },
	{ "Mauna Kea",      -5464075.1847,  -2495248.1055,   2148297.3649,  25.0 },
	{ "Medicina",        4461369.6954,    919597.1240,   4449559.3812,  32.0 },
	{ "Metsahovi",       2890652.4889,   1310295.5752,   5513958.8900,  14.0 },
	{ "Miyun",          -2201304.5880,   4324789.2160,   4125367.9130,  50.0 },
	{ "Mizusawa",       -3857244.4095,   3108783.1610,   4003899.2763,  20.0 },
	{ "Mopra",          -4682769.05850,  2802619.04217, -3291759.33837, 22.0 },
	{ "North Liberty",   -130872.4987,  -4762317.0925,   4226851.0014,  25.0 },
	{ "Noto",            4934562.8407,   1321201.5428,   3806484.7336,  32.0 },
	{ "NRAO 140ft",       882879.6538,  -4924482.3314,   3944130.7317,  42.7 },
	{ "Ny Alesund",      1202462.5260,    252734.5205,   6237766.2033,  20.0 },
	{ "Ogasawara",      -4491068.6486,   3481545.0064,   2887399.6888,  20.0 },
	{ "O'Higgins",       1525833.3121,  -2432463.6960,  -5676174.5055,  10.0 },
	{ "Onsala 60",       3370605.8010,    711917.7196,   5349830.9004,  20.0 },
	{ "Onsala 85",       3370965.9090,    711466.1978,   5349664.1947,  25.0 },
	{ "OVRO 130ft",     -2409601.0751,  -4478349.2866,   3838603.1236,  39.6 },
	{ "Owens Valley",   -2409150.4018,  -4478573.1180,   3838617.3385,  25.0 },
	{ "Parkes",         -4554232.4864,   2816758.8662,  -3454035.0137,  64.0 },
	{ "Pico Veleta",     5088967.74544,  -301681.18586,  3825012.20561, 30.0 },
	{ "Pie Town",       -1640953.9383,  -5014816.0237,   3575411.7916,  25.0 },
	{ "Saint Croix",     2607848.6379,  -5488069.5358,   1932739.7326,  25.0 },
	{ "Sardinia",        4865182.7660,    791922.6890,   4035137.1740,  65.0 },
	{ "Seshan",         -2831687.3927,   4675733.4877,   3275327.5043,  25.0 },
	{ "SMT",            -1828796.2000,   -5054406.800,   3427865.2000,  10.0 },
	{ "South Pole",          809.7616,      -816.8480,  -6359568.7286,  10.0 },
	{ "Svetloe",         2730173.6723,   1562442.7934,   5529969.1413,  32.0 },
	{ "Syowa",           1766194.2159,   1460410.9181,  -5932273.3585,  11.0 },
	{ "Tamna",          -3171731.4809,   4292677.3733,   3481040.4580,  21.0 },
	{ "Tianma",         -2826708.6030,   4679237.0770,   3274667.5510,  65.0 },
	{ "Tidbinbilla",    -4460894.7273,   2682361.5296,  -3674748.4238,  70.0 },
	{ "Torun",           3638558.5100,   1221969.7200,   5077036.7600,  32.0 },
	{ "Tsukuba",        -3957409.3668,   3310228.7947,   3737494.6747,  34.0 },
	{ "Ulsan",          -3287268.1566,   4023449.1207,   3687380.1796,  21.0 },
	{ "Urunqi",           228310.2129,   4631922.7656,   4367064.0638,  25.0 },
	{ "Usuda",          -3855355.4324,   3427427.5708,   3740971.1373,  64.0 },
	{ "Warkworth",      -5115425.635,     477880.304,   -3767042.837,   30.0 },
	{ "Westford",        1492206.3413,  -4458130.5326,   4296015.6009,  18.3 },
	{ "Wettzell",        4075539.6310,    931735.5366,   4801629.5267,  20.0 },
	{ "Yarragadee",     -2388896.1302,   5043349.9932,  -3078590.8577,  12.0 },
	{ "Yebes 40",        4848761.8217,   -261484.1869,   4123085.0387,  40.0 },
	{ "Yonsei",         -3042278.2562,   4045902.8094,   3867376.1435,  21.0 },
	{ "Zelenchukskya",   3451207.5353,   3060375.4139,   4391915.0384,  32.0 },
	{ "", 0.0, 0.0, 0.0, 0.0 },	/* this must terminate the list */

	/* incomplete records follow: */
	/* ALMA: 2225061.16360 m:-5440057.36994 m:-2481681.15054 m */
	/* SMA: -5464555.49300 m:-2492927.98900 m: 2150797.17600 m */
};

int lat2str(char *latstr, int length, double lat)
{
	double x, s;
	int d, m;
	char hemi = ' ';

	x = lat*180.0/M_PI;

	if(x < 0.0)
	{
		x = -x;
		hemi = 'S';
	}
	else if(x > 0.0)
	{
		hemi = 'N';
	}
	d = (int)x;
	x = (x - d)*60.0;
	m = (int)x;
	s = (x - m)*60.0;
	
	return snprintf(latstr, length, "%c%02dd%02dm%06.3fs", hemi, d, m, s);
}

int lon2str(char *lonstr, int length, double lon)
{
	double x, s;
	int d, m;
	char hemi = ' ';

	x = lon*180.0/M_PI;

	if(x < 0.0)
	{
		x = -x;
		hemi = 'W';
	}
	else if(x > 0.0)
	{
		hemi = 'E';
	}
	d = (int)x;
	x = (x - d)*60.0;
	m = (int)x;
	s = (x - m)*60.0;
	
	return snprintf(lonstr, length, "%c%02dd%02dm%06.3fs", hemi, d, m, s);
}

void fprintAntennaDBEntry(FILE *out, const AntennaDBEntry *ae)
{
	const int StrSize = 16;
	double lat, lon, alt; /* rad, rad, m */
	char latstr[StrSize];
	char lonstr[StrSize];
	
	ecef2lla(&lat, &lon, &alt, ae->x, ae->y, ae->z);
	lat2str(latstr, StrSize, lat);
	lon2str(lonstr, StrSize, lon);

	fprintf(out, "%s  (%3.1f, %3.1f, %3.1f) = (%s, %s, %3.1f) D=%3.1f\n", ae->name, ae->x, ae->y, ae->z, latstr, lonstr, alt, ae->diameter);
}

const AntennaDBEntry *antennaDBGetByIndex(unsigned int index)
{
	int N;

	N = sizeof(antennaDBEntries)/sizeof(AntennaDBEntry);

	if(index >= N)
	{
		return 0;
	}
	if(antennaDBEntries[index].name[0] == 0)
	{
		return 0;
	}

	return antennaDBEntries + index;
}

const AntennaDBEntry *antennaDBGetByXYZ(double x, double y, double z)
{
	const double tol = 5.0;	/* meters; tolerance per axis */
	int i;

	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if(fabs(antennaDBEntries[i].x - x) < tol &&
		   fabs(antennaDBEntries[i].y - y) < tol &&
		   fabs(antennaDBEntries[i].z - z) < tol)
		{
			return antennaDBEntries + i;
		}
	}

	return 0;
}

const AntennaDBEntry *antennaDBGetByName(const char *name)
{
	int i;

	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if(strcasecmp(antennaDBEntries[i].name, name) == 0)
		{
			return antennaDBEntries + i;
		}
	}

	return 0;
}

/* input:  ITRF x, y, z [m]
 * output: WGS84 latitude [rad], longitude [rad], altitude [m]
 */
void ecef2lla(double *lat, double *lon, double *alt, double x, double y, double z)
{
	/* WGS84 ellipsoid constants */
	const double a = 6378137; /* radius */
	const double e = 8.1819190842622e-2;  /* eccentricity */
	const double asq = a*a;
	const double esq = e*e;
	const double bsq = asq*(1.0 - esq);
	const double ep = sqrt(asq/bsq - 1.0);
	const double b = sqrt(bsq);

	double p, th, sinth, costh, la;

	p = sqrt(x*x+y*y);
	th = atan2(a*z, b*p);
	sinth = sin(th);
	costh = cos(th);
	if(lon)
	{
		*lon = atan2(y, x);
	}
	la = atan2( (z + ep*ep*b*sinth*sinth*sinth), (p - esq*a*costh*costh*costh) );
	if(lat)
	{
		*lat = la;
	}
	if(alt)
	{
		double sinla;
		double N;

		sinla = sin(la);
		N = a / sqrt(1.0 - esq*sinla*sinla);
		*alt = p/cos(la) - N;
	}
}
