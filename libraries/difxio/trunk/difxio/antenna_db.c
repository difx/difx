#include <math.h>
#include <strings.h>
#include "antenna_db.h"

/* Note: positions below should not be used for correlation.  Positions are intended
 * to allow unique identification of an antenna based on location, so ~1 meter accuracy
 * is all that should be assumed
 */
AntennaDBEntry antennaDBEntries[] =	/* FIXME: add individual antennas of arrays? */
{
	{ "AGGO",           "AGGO",      2765117.1886,  -4449232.5897,  -3626419.1095,  6.0 },
	{ "Algonquin",      "ALGOPARK",   918034.4879,  -4346132.3267,   4561971.2292,  46.0 },
	{ "ALMA",           "",          2225061.1636,  -5440057.36994, -2481681.15054, 12.0, 10.0 },
	{ "Apex",           "",          2225039.5297,  -5441197.6292,  -2479303.3597,  12.0, 0, "NASL" },
	{ "Arecibo",        "ARECIBO",   2390486.9000,  -5564731.4400,   1994720.4500,  300.0 },
	{ "ATCA",           "ATCA",     -4751685.98800,  2791621.22300, -3200491.70000, 22.0, 6.0 },
	{ "Azores",         "AZORES",    4552174.62052, -2186664.67586,  3882779.78919, 0.0 },
	{ "Badary",         "BADARY",    -838201.0685,   3865751.5652,   4987670.8885,  32.0 },
	{ "Brewster",       "BR-VLBA",  -2112065.2062,  -3705356.5048,   4726813.6759,  25.0 },
	{ "Cambridge",      "CAMBRIDG",  3920356.1500,      2542.0200,   5014284.4200,  32.0 },
	{ "Ceduna",         "CEDUNA",   -3753442.7457,   3912709.7530,  -3348067.6095,  30.0 },
	{ "Chilbolton",     "CHLBOLTN",  4008309.8765,   -100650.4260,   4943794.9323,  0.0 },
	{ "TIGO Conception","TIGOCONC",  1492051.10792, -4887961.52962, -3803541.76919, 6.0 },
	{ "DS3",            "DSA03",     1823351.5,     -4850434.0,     -3708961.7,     35.0 },  /* approximate coordinates */
	
	/* NASA DSN */
	/* Goldstone antennas */
	{ "DSS13",          "DSS13",    -2351112.659,   -4655530.636,    3660912.728,   34.0 },
	{ "DSS14",          "DSS14",    -2353621.420,   -4641341.472,    3677052.318,   70.0 },
	{ "DSS15",          "DSS15",    -2353538.958,   -4641649.429,    3676669.984,   34.0 },
	{ "DSS24",          "DSS24",    -2354906.711,   -4646840.095,    3669242.325,   34.0 },
	{ "DSS25",          "DSS25",    -2355022.014,   -4646953.204,    3669040.567,   34.0 },
	{ "DSS26",          "DSS26",    -2354890.797,   -4647166.328,    3668871.755,   34.0 },
	/* Canberra antenans */
	{ "DSS34",          "DSS34",    -4461146.905,    2682439.289,   -3674393.334,   34.0 },
	{ "DSS35",          "DSS35",    -4461147.093,    2682568.925,   -3674152.093,   34.0 },
	{ "DSS36",          "",         -4461168.415,    2682814.657,   -3674083.901,   34.0 },
	{ "DSS43",          "DSS43",    -4460894.7273,   2682361.5296,  -3674748.4238,  70.0 },
	{ "DSS45",          "DSS45",    -4460935.9763,   2682765.7053,  -3674380.4619,  34.0 },
	/* Robledo antennas */
	{ "DSS54",          "DSS54",     4849434.488,    -360723.8999,   4114618.835,   34.0 },
	{ "DSS55",          "DSS55",     4849525.256,    -360606.0932,   4114495.084,   34.0 },
	{ "DSS61",          "ROBLED32",  4849245.0234,   -360277.8190,   4114884.7620,  32.0 },
	{ "DSS63",          "DSS63",     4849092.518,    -360180.3480,   4115109.251,   70.0 },
	{ "DSS65",          "DSS65A",    4849339.634,    -360427.6637,   4114750.733,   34.0 },
	
	{ "Effelsberg",     "EFLSBERG",  4033947.2616,    486990.7866,   4900430.9915,  25.0 },
	{ "Fort Davis",     "FD-VLBA",  -1324009.3266,  -5332181.9547,   3231962.3949,  25.0 },
	{ "Fortaleza",      "FORTLEZA",  4985370.0032,  -3955020.3932,   -428472.0682,  0.0 },
	{ "GBT",            "GBT-VLBA",  -882589.4102,  -4924872.3416,   3943729.4062,  100.0 },
	{ "Goddard 12",     "GGAO12M",   1130730.24500, -4831245.95300,  3994228.22800, 12.0 },
	{ "Goddard 13",     "GGAO7108",  1130794.5021,  -4831233.8135,   3994217.0701,  13.2 },  /* is this the VGOS antenna? */
	{ "Hancock",        "HN-VLBA",   1446374.8658,  -4447939.6774,   4322306.1838,  25.0 },
	{ "Hart RAO 15",    "HART15M",   5085490.8010,   2668161.4892,  -2768692.6239,  15.0 },
	{ "Hart RAO 26",    "HARTRAO",   5085442.7637,   2668263.7913,  -2768696.7523,  26.0, 0, "EQUA" },
	{ "Haystack",       "HAYSTACK",  1492404.4908,  -4457266.5294,   4296881.8536,  37.0 },
	{ "Hobart 12",      "HOBART12", -3949990.7783,   2522421.2046,  -4311708.0242,  12.0 },
	{ "Hobart 26",      "HOBART26", -3950237.3590,   2522347.6804,  -4311561.8790,  26.0, 0, "XYEW" },
	{ "Irbene",         "",          3183649.615,    1276902.735,    5359264.576,   32.0 },
	{ "Irbene 16",      "",          3183295.000,    1276276.000,    5359611.000,   16.0 },
	{ "Iriki",          "VERAIRIK", -3521719.7439,   4132174.6639,   3336994.1873,  20.0 },
	{ "Ishigaki",       "VERAISGK", -3263995.0986,   4808056.3516,   2619948.8797,  20.0 },
	{ "Ishioka",        "ISHIOKA",  -3959636.0607,   3296825.4669,   3747042.6428,  0.0 },
	{ "JCMT",           "",         -5464584.676,   -2493001.1700,   2150653.982,   15.0 },
	{ "Jodrell Lovell", "",          3822626.0400,   -154105.6500,   5086486.0400,  76.0 },
	{ "Jodrell Mk2",    "",          3822846.7600,   -153802.2800,   5086285.9000,  25.0 },
	{ "Kashima 11",     "KASHIM11", -3997506.4214,   3276877.6675,   3724240.3950,  11.0 },
	{ "Kashima 34",     "KASHIM34", -3997649.2415,   3276690.8602,   3724278.7239,  34.0 },
	{ "Katherine",      "KATH12M",  -4147354.6449,   4581542.3911,  -1573303.2196,  12.0 },
	{ "Kitt Peak",      "KP-VLBA",  -1995678.8402,  -5037317.6968,   3357328.0251,  25.0 },
	{ "Koganei",        "KOGANEI",  -3941937.3975,   3368150.8927,   3702235.2449,  0.0 },
	{ "Kokee",          "KOKEE",    -5543837.7897,  -2054566.8097,   2387852.4958,  20.0 },
	{ "Kutunse",        "",          6346232.8,       -33799.5,       634818.7,     32.0 },  /* approximate coordinates */
	{ "Kunming",        "KUNMING",  -1281152.8016,   5640864.4199,   2682653.5123,  40.0 },
	{ "LMT",            "",          -768715.6320,  -5988507.0720,   2063354.8520,  50.0, 0, "NASL" },
	{ "Los Alamos",     "LA-VLBA",  -1449752.5839,  -4975298.5757,   3709123.8459,  25.0 },
	{ "Matera",         "MATERA",    4641938.4731,   1393003.3222,   4133325.7702,  20.0 },
	{ "Mauna Kea",      "MK-VLBA",  -5464075.1847,  -2495248.1055,   2148297.3649,  25.0 },
	{ "Medicina",       "MEDICINA",  4461369.6954,    919597.1240,   4449559.3812,  32.0 },
	{ "Metsahovi",      "METSAHOV",  2890652.4889,   1310295.5752,   5513958.8900,  14.0 },
	{ "Miyun",          "",         -2201304.5880,   4324789.2160,   4125367.9130,  50.0 },
	{ "Mizusawa",       "VERAMZSW", -3857244.4095,   3108783.1610,   4003899.2763,  20.0 },
	{ "Mopra",          "MOPRA",    -4682769.05850,  2802619.04217, -3291759.33837, 22.0 },
	{ "North Liberty",  "NL-VLBA",   -130872.4987,  -4762317.0925,   4226851.0014,  25.0 },
	{ "Noto",           "NOTO",      4934562.8407,   1321201.5428,   3806484.7336,  32.0 },
	{ "NRAO 140ft",     "NRAO_140",   882879.6538,  -4924482.3314,   3944130.7317,  42.7, 0, "EQUA" },
	{ "NRAO 20m",       "NRAO20",     883772.5007,  -4924385.6143,   3944042.5357,  20.0 },
	{ "NRAO 85-1",      "NRAO85_1",   883555.56249, -4924490.88792,  3943961.95994, 25.0 },
	{ "NRAO 85-3",      "NRAO85_3",   882325.57126  -4925137.98625,  3943397.67664, 25.0 },
	{ "Ny Alesund 13S", "NYALES13S", 1201071.41854,   252129.5366,   6238022.33843, 13.0 },
	{ "Ny Alesund",     "NYALES20",  1202462.5260,    252734.5205,   6237766.2033,  20.0 },
	{ "Ogasawara",      "VERAOGSW", -4491068.6486,   3481545.0064,   2887399.6888,  20.0 },
	{ "O'Higgins",      "OHIGGINS",  1525833.3121,  -2432463.6960,  -5676174.5055,  10.0 },
	{ "Onsala 13NE",    "ONSA13NE",  3370889.19,      711570.78,     5349691.10,    13.0 },
	{ "Onsala 13SW",    "ONSA13SW",  3370946.72,      711534.14,     5349659.94,    13.0 },
	{ "Onsala 60",      "ONSALS60",  3370605.8010,    711917.7196,   5349830.9004,  20.0 },
	{ "Onsala 85",      "ONSALA85",  3370965.9090,    711466.1978,   5349664.1947,  25.0, 0, "EQUA" },
	{ "OVRO 130ft",     "OVRO_130", -2409601.0751,  -4478349.2866,   3838603.1236,  39.6 },
	{ "Owens Valley",   "OV-VLBA",  -2409150.4018,  -4478573.1180,   3838617.3385,  25.0 },
	{ "Parkes",         "PARKES",   -4554232.4864,   2816758.8662,  -3454035.0137,  64.0 },
	{ "Penticton",      "PENTICTN", -2058840.8971,  -3621286.7090,   4814420.9163,  0.0 },
	{ "Pico Veleta",    "",          5088967.74544,  -301681.18586,  3825012.20561, 30.0, 0, "NASL" },
	{ "Pie Town",       "PIETOWN",  -1640953.9383,  -5014816.0237,   3575411.7916,  25.0 },
	{ "Saint Croix",    "SC-VLBA",   2607848.6379,  -5488069.5358,   1932739.7326,  25.0 },
	{ "Sardinia",       "",          4865182.7660,    791922.6890,   4035137.1740,  65.0 },
	{ "Santa Maria",    "RAEGSMAR",  4618520.154,   -2166021.902,    3816274.031,   13.2 },
	{ "Sejong",         "SEJONG",   -3110080.0378,   4082066.6316,   3775076.7904,  0.0 },
	{ "Seshan",         "SESHAN25", -2831687.3927,   4675733.4877,   3275327.5043,  25.0 },
	{ "SEST",           "SEST",      1838237.89336, -5258699.25068, -3100588.76895, 15.0 },
	{ "SMA",            "",         -5464555.4930,  -2492927.9890,   2150797.1760,  6.0, 3.0, "NASL" },
	{ "SMT",            "",         -1828796.2000,   -5054406.800,   3427865.2000,  10.0, 0, "NASR" },
	{ "South Pole",     "",              809.7616,      -816.8480,  -6359568.7286,  10.0 },
	{ "Svetloe",        "SVETLOE",   2730173.6723,   1562442.7934,   5529969.1413,  32.0 },
	{ "Syowa",          "SYOWA",     1766194.2159,   1460410.9181,  -5932273.3585,  11.0 },
	{ "Tamna",          "",         -3171731.4809,   4292677.3733,   3481040.4580,  21.0 },
	{ "Tianma",         "TIANMA65", -2826708.6030,   4679237.0770,   3274667.5510,  65.0 },
	{ "Tidbinbilla",    "TIDBIN64", -4460894.7273,   2682361.5296,  -3674748.4238,  64.0 },
	{ "Torun",          "TORUN",     3638558.5100,   1221969.7200,   5077036.7600,  32.0 },
	{ "Tsukuba",        "",         -3957409.3668,   3310228.7947,   3737494.6747,  34.0 },
	{ "Uchinour",       "UCHINOUR", -3586229.25030,  4114103.81060,  3290224.78860, 0.0 },
	{ "Ulsan",          "",         -3287268.1566,   4023449.1207,   3687380.1796,  21.0 },
	{ "Urunqi",         "URUMQI",     228310.2129,   4631922.7656,   4367064.0638,  25.0 },
	{ "Usuda",          "USUDA64",  -3855355.4324,   3427427.5708,   3740971.1373,  64.0 },
	{ "Ussurisk",       "USSURISK", -3059725.12787,  3427256.74229,  4409485.41773, 70.0 },
	{ "VLA",            "VLA",      -1601185.4286,  -5041977.1754,   3554875.6231,  25.0, 25.0 },
	{ "Warkworth",      "",         -5115425.635,     477880.304,   -3767042.837,   30.0 },
	{ "Warkworth 12m",  "WARK12M",  -5115324.4672,    477843.2759,  -3767192.7928,  12.0 },
	{ "Westerbork",     "WSTRBORK",  3828651.2900,    443447.4800,   5064921.5700,  25.0, 12.5, "EQUA" },
	{ "Westford",       "WESTFORD",  1492206.3413,  -4458130.5326,   4296015.6009,  18.3 },
	{ "Wettzell",       "WETTZELL",  4075539.6310,    931735.5366,   4801629.5267,  20.0 },
	{ "Wettzell 13N",   "WETTZ13N",  4075627.78800,   931774.12800,  4801552.27700, 13.2 },
	{ "Wettzell 13S",   "WETTZ13S",  4075659.10000,   931824.64000,  4801516.18000, 13.2 },
	{ "Yarragadee",     "YARRA12M", -2388896.1302,   5043349.9932,  -3078590.8577,  12.0 },
	{ "Yebes 40",       "YEBES40M",  4848761.8217,   -261484.1869,   4123085.0387,  40.0 },
	{ "Yebes VGOS",     "RAEGYEB",   4848831.0779,   -261629.5126,   4122976.4921,  0.0 },
	{ "Yonsei",         "",         -3042278.2562,   4045902.8094,   3867376.1435,  21.0 },
	{ "Zelenchukskya",  "ZELENCHK",  3451207.5353,   3060375.4139,   4391915.0384,  32.0 },
	{ "", "", 0.0, 0.0, 0.0, 0.0 },	/* this must terminate the list */

	/* Known Missing: 
		Nyalesund 13N VGOS
		Seshan VGOS
		Tianma VGOS
		Evpatoria
		"ZELRT13V"
	 */
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

	fprintf(out, "%s  (%3.1f, %3.1f, %3.1f) = (%s, %s, %3.1f) D=%3.1fm A=%3.1fkm mount=%s lon=%9.7f lat=%9.7f\n", ae->name, ae->x, ae->y, ae->z, latstr, lonstr, alt, ae->diameter, ae->arrayExtent, ae->mountType[0] ? ae->mountType : "AZEL", lon, lat);
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

	/* First look for single dishes */
	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if(antennaDBEntries[i].arrayExtent > 0.0)
		{
			continue;
		}
		if(fabs(antennaDBEntries[i].x - x) < tol &&
		   fabs(antennaDBEntries[i].y - y) < tol &&
		   fabs(antennaDBEntries[i].z - z) < tol)
		{
			return antennaDBEntries + i;
		}
	}

	/* Then look for arrays */
	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		double dx, dy, dz, r;
		if(antennaDBEntries[i].arrayExtent <= 0.0)
		{
			continue;
		}

		dx = antennaDBEntries[i].x - x;
		dy = antennaDBEntries[i].y - y;
		dz = antennaDBEntries[i].z - z;
		r = antennaDBEntries[i].arrayExtent;
		if(dx*dx + dy*dy + dz*dz < r*r)
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

const AntennaDBEntry *antennaDBGetByIVSName(const char *ivsName)
{
	int i;

	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if(strcasecmp(antennaDBEntries[i].ivsName, ivsName) == 0)
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
