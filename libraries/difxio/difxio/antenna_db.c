#include <math.h>
#include <strings.h>
#include "antenna_db.h"

/* Note: positions below should not be used for correlation.  Positions are intended
 * to allow unique identification of an antenna based on location, so ~1 meter accuracy
 * is all that should be assumed
 */

AntennaDBEntry antennaDBEntries[] =	/* FIXME: add individual antennas of arrays? */
{
	{ "AGGO",           "AGGO",      2765116.7009,  -4449233.7840,  -3626420.5633,  6.0 },		/* ITRF2014, 2019d solution */
	{ "Aira",           "AIRA",     -3530219.8988,   4118797.3814,   3344015.4850,  10.26 },	/* ITRF2014, 2019d solution; Kagoshima, JP */
	{ "Algonquin",      "ALGOPARK",   918034.3764,  -4346132.3595,   4561971.2493,  46.0 },		/* ITRF2014, 2019d solution */
	{ "ALMA",           "",          2225061.1636,  -5440057.36994, -2481681.15054, 12.0, 10.0 },
	{ "Apex",           "",          2225039.5297,  -5441197.6292,  -2479303.3597,  12.0, 0, "NASL" },
	{ "Arecibo",        "ARECIBO",   2390486.9000,  -5564731.4400,   1994720.4500,  300.0, 0, "", VLBI_GROUP_DEFUNCT },
	{ "Arecibo 12m",    "",          2390512.68647, -5564470.08390,  1995124.38504, 12.0 },
	{ "ASKAP 07",       "",         -2556279.416,    5097245.383,   -2848523.218,   12.0 },
	{ "ATCA",           "ATCA",     -4751685.98800,  2791621.22300, -3200491.70000, 22.0, 6.0, "", VLBI_GROUP_LBA | VLBI_GROUP_ARRAY },
	{ "Azores",         "AZORES",    4552174.62052, -2186664.67586,  3882779.78919, 3.9 },
	{ "Badary",         "BADARY",    -838201.2618,   3865751.5589,   4987670.8708,  32.0 },		/* ITRF2014, 2019d solution */
	{ "Badary 13m",     "BADAR13M",  -838326.334,    3865797.228,    4987598.328,   13.0 },
	{ "Brewster",       "BR-VLBA",  -2112065.3082,  -3705356.5142,   4726813.6124,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },
	{ "Cambridge",      "CAMBRIDG",  3920356.1500,      2542.0200,   5014284.4200,  32.0 },
	{ "Chichijima",     "CHICHI10", -4490617.9108,   3483908.7523,   2884899.3203,  10.26 },	/* ITRF2014, 2019d solution */
	{ "Ceduna",         "CEDUNA",   -3753442.7457,   3912709.7530,  -3348067.6095,  30.0, 0, "", VLBI_GROUP_LBA },
	{ "Chilbolton",     "CHLBOLTN",  4008309.8765,   -100650.4260,   4943794.9323,  26.0 },
	{ "TIGO Conception","TIGOCONC",  1492051.10792, -4887961.52962, -3803541.76919, 6.0, 0, "", VLBI_GROUP_DEFUNCT },
	{ "Crimea",         "CRIMEA",    3785230.5786,   2551207.7722,   4439796.6036,  22.0 },		/* ITRF2014, 2019d solution */
	{ "Darnhall",       "",          3829087.8990,   -169568.9550,   5081082.3460,  25.0 },
	{ "Defford",        "",          3923442.5660,   -146914.3300,   5009755.1250,  25.0 },
	{ "DS3",            "DSA03",     1823351.5,     -4850434.0,     -3708961.7,     35.0 },  /* approximate coordinates */
	
	/* NASA DSN */
	/* Goldstone antennas */
	{ "DSS13",          "DSS13",    -2351112.9506,  -4655530.5460,   3660912.6356,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	{ "DSS14",          "DSS14",    -2353621.420,   -4641341.472,    3677052.318,   70.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS15",          "DSS15",    -2353539.2278,  -4641649.3331,   3676669.8681,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	{ "DSS24",          "DSS24",    -2354906.711,   -4646840.095,    3669242.325,   34.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS25",          "DSS25",    -2355022.014,   -4646953.204,    3669040.567,   34.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS26",          "DSS26",    -2354890.797,   -4647166.328,    3668871.755,   34.0, 0, "", VLBI_GROUP_DSN },
	/* Canberra antennas */
	{ "DSS34",          "DSS34",    -4461147.7340,   2682439.2728,  -3674392.2725,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	{ "DSS35",          "DSS35",    -4461147.093,    2682568.925,   -3674152.093,   34.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS36",          "DSS36",    -4461169.0553,   2682814.7047,  -3674083.0790,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	{ "DSS43",          "DSS43",    -4460894.7273,   2682361.5296,  -3674748.4238,  70.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS45",          "DSS45",    -4460936.2219,   2682765.7129,  -3674380.1486,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	/* Robledo antennas */
	{ "DSS54",          "DSS54",     4849434.488,    -360723.8999,   4114618.835,   34.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS55",          "DSS55",     4849525.256,    -360606.0932,   4114495.084,   34.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS61",          "ROBLED32",  4849245.0234,   -360277.8190,   4114884.7620,  32.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS63",          "DSS63",     4849092.518,    -360180.3480,   4115109.251,   70.0, 0, "", VLBI_GROUP_DSN },
	{ "DSS65",          "DSS65A",    4849339.4917,   -360427.3685,   4114750.9317,  34.0, 0, "", VLBI_GROUP_DSN },		/* ITRF2014, 2019d solution */
	
	{ "Effelsberg",     "EFLSBERG",  4033947.1525,    486990.8961,   4900431.0604,  25.0, 0, "", VLBI_GROUP_HSA },		/* ITRF2014, 2019d solution */
	{ "Fort Davis",     "FD-VLBA",  -1324009.4120,  -5332181.9576,   3231962.3492,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },
	{ "MacDonald",      "MACGO12M", -1330788.46447, -5328106.58850,  3236427.49718, 12.0 },		/* Chet; VGOS positions 2020 */
	{ "MacDonald 13m",  "MCD 7850", -1330008.085,   -5328391.578,    3236502.734,   13.0 },
	{ "Fortaleza",      "FORTLEZA",  4985369.9984,  -3955020.4158,   -428471.9983,  14.2 },
	{ "GBT",            "GBT-VLBA",   882589.574,   -4924872.334,    3943729.336,   100.0, 0, "", VLBI_GROUP_HSA },	/* ITRF2014 */
	{ "GBANK_TS",       "GBANK_TS",   884084.2636,  -4924578.7481,   3943734.3354,  0.0 },
	{ "GMRT",           "",          1656988.9135,   5797576.2281,   2073756.22,    36.0, 12 },	/* Approximate location of a random antenna */
	{ "Goddard 12",     "GGAO12M",   1130730.24500, -4831245.95300,  3994228.22800, 12.0 },
	{ "Goddard 13",     "GGAO7108",  1130794.5021,  -4831233.8135,   3994217.0701,  13.2 },  /* is this the VGOS antenna? */
	{ "Hancock",        "HN-VLBA",   1446374.7571,  -4447939.6947,   4322306.2061,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Hart RAO 15",    "HART15M",   5085490.8071,   2668161.6274,  -2768692.5007,  15.0 },		/* ITRF2014, 2019d solution */
	{ "Hart RAO 26",    "HARTRAO",   5085442.7721,   2668263.9300,  -2768696.6299,  26.0, 0, "EQUA" },	/* ITRF2014, 2019d solution */
	{ "Haystack",       "HAYSTACK",  1492404.3883,  -4457266.5569,   4296881.8698,  37.0 },		/* ITRF2014, 2019d solution */
	{ "Hobart 12",      "HOBART12", -3949991.0556,   2522421.2681,  -4311707.7596,  12.0 },		/* ITRF2014, 2019d solution */
	{ "Hobart 26",      "HOBART26", -3950237.6192,   2522347.7349,  -4311561.5974,  26.0, 0, "XYEW" },	/* ITRF2014, 2019d solution */
	{ "Irbene",         "IRBENE",    3183649.341,    1276902.985,    5359264.715,   32.0 },		/* ITRF2014 */
	{ "Irbene 16",      "IRBENE16",  3183295.000,    1276276.000,    5359611.000,   16.0 },
	{ "Iriki",          "VERAIRIK", -3521719.7439,   4132174.6639,   3336994.1873,  20.0 },
	{ "Ishigaki",       "VERAISGK", -3263995.4716,   4808056.4149,   2619948.5632,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Ishioka",        "ISHIOKA",  -3959636.1869,   3296825.4610,   3747042.5843,  13.0 },		/* ITRF2014, 2019d solution */
	{ "JCMT",           "",         -5464584.676,   -2493001.1700,   2150653.982,   15.0 },
	{ "Jodrell Lovell", "",          3822626.0400,   -154105.6500,   5086486.0400,  76.0 },
	{ "Jodrell Mk2",    "",          3822846.7600,   -153802.2800,   5086285.9000,  25.0 },
	{ "Kashima 11",     "KASHIM11", -3997506.0707,   3276877.9110,   3724240.4400,  11.0 },		/* ITRF2014, 2019d solution */
	{ "Kashima 34",     "KASHIM34", -3997649.6605,   3276690.2581,   3724278.5848,  34.0 },		/* ITRF2014, 2019d solution */
	{ "Katherine",      "KATH12M",  -4147354.8905,   4581542.2955,  -1573302.8297,  12.0 },		/* ITRF2014, 2019d solution */
	{ "Kitt Peak",      "KP-VLBA",  -1995678.9310,  -5037317.6982,   3357327.9645,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Kitt Peak 12",   "",         -1995954.4000,  -5037389.4000,   3357044.3000,  12.0 },
	{ "Knockin",        "",          3860084.8980,   -202105.0390,   5056568.8480,  25.0 },
	{ "Koganei",        "KOGANEI",  -3941937.6547,   3368150.8022,   3702235.3081,  11.0 },		/* ITRF2014, 2019d solution */
	{ "Kokee",          "KOKEE",    -5543837.8341,  -2054566.4187,   2387852.6678,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Kokee 12",       "KOKEE12M", -5543831.405,   -2054585.733,    2387828.797,   12.0 },		/* Chet; VGOS 2020 */
	{ "Kutunse",        "",          6346232.8,       -33799.5,       634818.7,     32.0 },  /* approximate coordinates */
	{ "Kunming",        "KUNMING",  -1281153.0881,   5640864.4401,   2682653.3857,  40.0 },		/* ITRF2014, 2019d solution */
	{ "LMT",            "",          -768715.6320,  -5988507.0720,   2063354.8520,  50.0, 0, "NASL" },
	{ "Los Alamos",     "LA-VLBA",  -1449752.6814,  -4975298.5778,   3709123.7971,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Marpoint",       "MARPOINT",  1106628.9344,  -4882907.1606,   3938086.9735,  25.9 },		/* ITRF2014, 2019d solution */
	{ "Matera",         "MATERA",    4641938.3458,   1393003.4508,   4133325.8833,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Mauna Kea",      "MK-VLBA",  -5464075.2736,  -2495247.6825,   2148297.5617,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Medicina",       "MEDICINA",  4461369.5682,    919597.2489,   4449559.4702,  32.0 },		/* ITRF2014, 2019d solution */
	{ "Metsahovi",      "METSAHOV",  2892584.7339,   1311715.6918,   5512640.2335,  14.0 },		/* ITRF2014, 2019d solution */
	{ "Miyun",          "",         -2201304.5880,   4324789.2160,   4125367.9130,  50.0 },
	{ "Mizusawa",       "VERAMZSW", -3857244.2418,   3108783.5264,   4003899.4149,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Mizusawa 10m",   "MIZNAO10", -3857236.086,    3108803.330,    4003883.012,   10.0 },
	{ "Mopra",          "MOPRA",    -4682769.05850,  2802619.04217, -3291759.33837, 22.0 },
	{ "North Liberty",  "NL-VLBA",   -130872.6044,  -4762317.0907,   4226850.9773,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Noto",           "NOTO",      4934562.7154,   1321201.6601,   3806484.8461,  32.0 },		/* ITRF2014, 2019d solution */
	{ "NRAO 140ft",     "NRAO_140",   882879.5505,  -4924482.3480,   3944130.7468,  42.7, 0, "EQUA" },		/* ITRF2014, 2019d solution */
	{ "NRAO 20m",       "NRAO20",     883772.4123,  -4924385.6260,   3944042.5492,  20.0 },		/* ITRF2014, 2019d solution */
	{ "NRAO 85-1",      "NRAO85_1",   883555.2130,  -4924490.9278,   3943962.0221,  25.0 },		/* ITRF2014, 2019d solution */
	{ "NRAO 85-3",      "NRAO85_3",   882325.2253,  -4925138.0268,   3943397.7393,  25.0 },		/* ITRF2014, 2019d solution */
	{ "Ny Alesund 13S", "NYALES13S", 1201071.41854,   252129.5366,   6238022.33843, 13.2 },
	{ "Ny Alesund",     "NYALES20",  1202462.4209,    252734.5625,   6237766.2835,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Ogasawara",      "VERAOGSW", -4491068.6486,   3481545.0064,   2887399.6888,  20.0 },
	{ "O'Higgins",      "OHIGGINS",  1525833.4537,  -2432463.6962,  -5676174.4977,  10.0 },		/* ITRF2014, 2019d solution */
	{ "Onsala 13NE",    "ONSA13NE",  3370889.19,      711570.78,     5349691.10,    13.0 },
	{ "Onsala 13SW",    "ONSA13SW",  3370946.72,      711534.14,     5349659.94,    13.0 },
	{ "Onsala 60",      "ONSALS60",  3370605.7035,    711917.8146,   5349830.9852,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Onsala 85",      "ONSALA85",  3370965.9090,    711466.1978,   5349664.1947,  25.0, 0, "EQUA" },
	{ "OVRO 130ft",     "OVRO_130", -2409601.2327,  -4478349.2744,   3838603.1279,  39.6 },		/* ITRF2014, 2019d solution */
	{ "Owens Valley",   "OV-VLBA",  -2409150.5277,  -4478573.0798,   3838617.2999,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Parkes",         "PARKES",   -4554232.7408,   2816758.8590,  -3454034.6988,  64.0 },		/* ITRF2014, 2019d solution */
	{ "Penticton",      "PENTICTN", -2058840.8971,  -3621286.7090,   4814420.9163,  25.6 },
	{ "Pico Veleta",    "",          5088967.74544,  -301681.18586,  3825012.20561, 30.0, 0, "NASL" },
	{ "Pie Town",       "PIETOWN",  -1640954.0357,  -5014816.0281,   3575411.7374,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Saint Croix",    "SC-VLBA",   2607848.6985,  -5488069.4801,   1932739.8169,  25.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_VLBA },		/* ITRF2014, 2019d solution */
	{ "Sardinia",       "",          4865182.7660,    791922.6890,   4035137.1740,  65.0 },
	{ "Santa Maria",    "RAEGSMAR",  4618524.3781,  -2166020.7378,   3816270.3654,  13.2 },		/* ITRF2014, 2019d solution */
	{ "Santia 12m",     "SANTIA12",  1769693.5857,  -5044504.7132,  -3468434.7322,  12.0 },		/* ITRF2014, 2019d solution */
	{ "Sejong",         "SEJONG",   -3110080.2102,   4082066.5941,   3775076.7543,  22.0 },		/* ITRF2014, 2019d solution */
	{ "Seshan 13m",     "SESHAN13", -2831647.050,    4675729.464,    3275365.079,   13.0 },
	{ "Seshan",         "SESHAN25", -2831687.6066,   4675733.3922,   3275327.4239,  25.0 },		/* ITRF2014, 2019d solution */
	{ "SEST",           "SEST",      1838237.9028,  -5258699.2884,  -3100588.8399,  15.0 },		/* ITRF2014, 2019d solution */
	{ "SMA",            "",         -5464555.4930,  -2492927.9890,   2150797.1760,  6.0, 3.0, "NASL" },
	{ "SMT",            "",         -1828796.2000,   -5054406.800,   3427865.2000,  10.0, 0, "NASR" },
	{ "South Pole",     "",              809.7616,      -816.8480,  -6359568.7286,  10.0 },
	{ "Svetloe",        "SVETLOE",   2730173.5386,   1562442.8795,   5529969.2003,  32.0 },		/* ITRF2014, 2019d solution */
	{ "Syowa",          "SYOWA",     1766194.2549,   1460410.9101,  -5932273.3328,  11.0 },		/* ITRF2014, 2019d solution */
	{ "Tabley",         "",          3817549.9560,   -163031.1410,   5089896.6540,  25.0 },
	{ "Tamna",          "",         -3171731.4809,   4292677.3733,   3481040.4580,  21.0 },
	{ "Tianma 13m",     "TIANMA13", -2826801.912,    4679253.999,    3274516.066,   13.0 },
	{ "Tianma",         "TIANMA65", -2826708.8081,   4679236.9722,   3274667.4495,  65.0 },		/* ITRF2014, 2019d solution */
	{ "Tidbinbilla",    "TIDBIN64", -4460895.4973,   2682361.5220,  -3674747.2763,  64.0 },		/* ITRF2014, 2019d solution */
	{ "Torun",          "TORUN",     3638558.5100,   1221969.7200,   5077036.7600,  32.0 },
	{ "Tsukuba",        "TSUKUB32", -3957409.3668,   3310228.7947,   3737494.6747,  32.0 },
	{ "Uchinoura",      "UCHINOUR", -3586229.4414,   4114103.6829,   3290224.6489,  32.0 },		/* ITRF2014, 2019d solution */
	{ "Ulsan",          "",         -3287268.1566,   4023449.1207,   3687380.1796,  21.0 },
	{ "Urunqi",         "URUMQI",     228309.9922,   4631922.8975,   4367064.2407,  25.0 },		/* ITRF2014, 2019d solution */
	{ "Usuda",          "USUDA64",  -3855355.4324,   3427427.5708,   3740971.1373,  64.0 },
	{ "Usuda TS",       "",         -3855127.5600,   3427750.5400,   3741009.5700,  0.0 },
	{ "Ussurisk",       "USSURISK", -3059725.12787,  3427256.74229,  4409485.41773, 70.0 },
	{ "VLA-Y27",        "VLA-Y27",  -1601185.4286,  -5041977.1754,   3554875.6231,  0.0, 0, "", VLBI_GROUP_HSA | VLBI_GROUP_ARRAY },
	{ "VLA-W1",         "VLA-W1",   -1601189.0571,  -5042000.1631,   3554843.1461,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W2",         "VLA-W2",   -1601225.3052,  -5041980.0412,   3554855.4131,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W3",         "VLA-W3",   -1601265.1742,  -5041982.2374,   3554834.5855,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W4",         "VLA-W4",   -1601315.9227,  -5041984.9942,   3554808.0289,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W5",         "VLA-W5",   -1601377.0404,  -5041988.3479,   3554776.1224,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W6",         "VLA-W6",   -1601447.2375,  -5041992.1619,   3554739.4257,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W7",         "VLA-W7",   -1601526.3929,  -5041996.5215,   3554698.0465,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W8",         "VLA-W8",   -1601614.1208,  -5042001.3283,   3554652.2340,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W9",         "VLA-W9",   -1601710.0500,  -5042006.5989,   3554602.0752,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W10",        "VLA-W10",  -1601814.0835,  -5042012.5727,   3554547.9456,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W12",        "VLA-W12",  -1602044.9320,  -5042025.4990,   3554427.5600,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W14",        "VLA-W14",  -1602304.9527,  -5042039.5129,   3554291.4579,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W16",        "VLA-W16",  -1602592.8832,  -5042054.6787,   3554140.4270,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W18",        "VLA-W18",  -1602908.2172,  -5042071.9756,   3553975.6326,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W20",        "VLA-W20",  -1603249.7135,  -5042091.0794,   3553797.5266,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W24",        "VLA-W24",  -1604008.7704,  -5042135.5030,   3553403.4299,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W28",        "VLA-W28",  -1604865.6787,  -5042189.7242,   3552962.0919,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W32",        "VLA-W32",  -1605808.6867,  -5042229.7669,   3552458.9295,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W36",        "VLA-W36",  -1606841.9927,  -5042279.3743,   3551912.7474,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W40",        "VLA-W40",  -1607962.4855,  -5042337.8879,   3551324.6722,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W48",        "VLA-W48",  -1610451.9580,  -5042470.8110,   3550020.7849,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W56",        "VLA-W56",  -1613255.4369,  -5042612.7694,   3548545.6335,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W64",        "VLA-W64",  -1616361.6129,  -5042770.2026,   3546911.1679,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-W72",        "VLA-W72",  -1619757.3374,  -5042937.3531,   3545120.1144,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E1",         "VLA-E1",   -1601192.4968,  -5042022.5297,   3554810.1679,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E2",         "VLA-E2",   -1601150.0980,  -5042000.3007,   3554860.4546,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E3",         "VLA-E3",   -1601114.3827,  -5042022.8180,   3554844.6724,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E4",         "VLA-E4",   -1601068.8167,  -5042051.5957,   3554824.5596,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E5",         "VLA-E5",   -1601014.4786,  -5042085.9133,   3554800.5128,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E6",         "VLA-E6",   -1600951.6055,  -5042125.5814,   3554772.7320,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E7",         "VLA-E7",   -1600880.6058,  -5042170.0628,   3554741.1812,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E8",         "VLA-E8",   -1600801.9546,  -5042219.0419,   3554706.1713,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E9",         "VLA-E9",   -1600715.9812,  -5042272.8644,   3554667.9081,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E10",        "VLA-E10",  -1600622.9576,  -5042331.4406,   3554626.7633,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E12",        "VLA-E12",  -1600416.5386,  -5042462.1256,   3554535.7642,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E14",        "VLA-E14",  -1600183.9859,  -5042609.6600,   3554433.3509,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E18",        "VLA-E18",  -1599644.8917,  -5042953.3427,   3554196.7583,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E20",        "VLA-E20",  -1599340.8285,  -5043150.6404,   3554064.9425,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E24",        "VLA-E24",  -1598663.1181,  -5043581.0671,   3553766.7519,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E28",        "VLA-E28",  -1597899.9316,  -5044068.3514,   3553432.1680,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E32",        "VLA-E32",  -1597053.1443,  -5044604.3620,   3553058.7104,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E36",        "VLA-E36",  -1596127.7581,  -5045193.4187,   3552652.1423,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E40",        "VLA-E40",  -1595124.9507,  -5045829.1329,   3552210.4083,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E48",        "VLA-E48",  -1592894.1024,  -5047228.8004,   3551220.9262,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E56",        "VLA-E56",  -1590380.6222,  -5048809.9236,   3550108.1836,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E64",        "VLA-E64",  -1587600.2240,  -5050575.5422,   3548885.1217,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-E72",        "VLA-E72",  -1584460.9138,  -5052385.2827,   3547599.7357,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N1",         "VLA-N1",   -1601185.6548,  -5041977.8350,   3554876.1507,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N2",         "VLA-N2",   -1601180.9010,  -5041947.1178,   3554921.3474,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N3",         "VLA-N3",   -1601177.4113,  -5041924.7316,   3554954.3022,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N4",         "VLA-N4",   -1601174.0201,  -5041902.3254,   3554987.2493,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N5",         "VLA-N5",   -1601168.8039,  -5041868.7306,   3555036.6639,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N6",         "VLA-N6",   -1601162.6313,  -5041828.6653,   3555095.6184,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N7",         "VLA-N7",   -1601155.6643,  -5041783.5105,   3555162.1058,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N8",         "VLA-N8",   -1601147.726,   -5041733.509,    3555235.766,   25.0, 0, "", VLBI_GROUP_VLA },		/* ITRF2014 */
	{ "VLA-N9",         "VLA-N9",   -1601139.5107,  -5041678.7232,   3555316.2743,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N10",        "VLA-N10",  -1601130.3625,  -5041619.4477,   3555403.4595,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N12",        "VLA-N12",  -1601110.0805,  -5041487.7546,   3555597.1623,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N14",        "VLA-N14",  -1601087.1959,  -5041339.5354,   3555815.5928,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N16",        "VLA-N16",  -1601061.9856,  -5041175.5631,   3556057.7527,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N18",        "VLA-N18",  -1601034.4283,  -5040996.2193,   3556322.6594,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N20",        "VLA-N20",  -1601004.7375,  -5040802.4843,   3556609.8561,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N24",        "VLA-N24",  -1600930.1133,  -5040316.0759,   3557330.1081,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N28",        "VLA-N28",  -1600863.7131,  -5039884.9934,   3557965.0425,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N32",        "VLA-N32",  -1600781.0678,  -5039347.1309,   3558761.2651,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N36",        "VLA-N36",  -1600690.6346,  -5038758.4093,   3559631.7846,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N40",        "VLA-N40",  -1600592.7766,  -5038121.0264,   3560574.5734,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N48",        "VLA-N48",  -1600374.9276,  -5036703.8734,   3562667.6156,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N56",        "VLA-N56",  -1600128.4269,  -5035103.8229,   3565024.3932,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N64",        "VLA-N64",  -1599855.6997,  -5033332.0554,   3567636.3526,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "VLA-N72",        "VLA-N72",  -1599557.9635,  -5031396.0524,   3570494.4837,  25.0, 0, "", VLBI_GROUP_VLA },		/* Sched catalog */
	{ "Warkworth",      "WARK30M",  -5115423.680,     477880.102,   -3767040.597,   30.0 },
	{ "Warkworth 12m",  "WARK12M",  -5115324.5720,    477843.2770,  -3767192.6092,  12.0 },		/* ITRF2014, 2019d solution */
	{ "Westerbork",     "WSTRBORK",  3828651.2900,    443447.4800,   5064921.5700,  25.0, 12.5, "EQUA" },
	{ "Westford",       "WESTFORD",  1492206.2336,  -4458130.5489,   4296015.6228,  18.3 },		/* ITRF2014, 2019d solution */
	{ "Wettzell",       "WETTZELL",  4075539.5173,    931735.6497,   4801629.6028,  20.0 },		/* ITRF2014, 2019d solution */
	{ "Wettzell 13N",   "WETTZ13N",  4075627.5521,    931774.3813,   4801552.4379,  13.2 },		/* ITRF2014, 2019d solution */
	{ "Wettzell 13S",   "WETTZ13S",  4075659.227,     931824.891,    4801516.193,   13.2 },		/* Chet; VGOS 2020 */
	{ "Yamaguchi 32",   "",         -3502544.2588,   3950966.3969,   3566381.1649,  32.0 },
	{ "Yarragadee",     "YARRA12M", -2388896.4538,   5043350.0584,  -3078590.5092,  12.0 },		/* ITRF2014, 2019d solution */
	{ "Yebes 40",       "YEBES40M",  4848761.7579,   -261484.0570,   4123085.1343,  40.0 },		/* ITRF2014, 2019d solution */
	{ "Yebes VGOS",     "RAEGYEB",   4848831.338,    -261629.412,    4122976.448,   13.2 },		/* Chet; VGOS 2020 */
	{ "Yonsei",         "",         -3042278.2562,   4045902.8094,   3867376.1435,  21.0 },
	{ "Zelenchukskya",  "ZELENCHK",  3451207.3892,   3060375.5196,   4391915.1199,  32.0 },		/* ITRF2014, 2019d solution */
	{ "Zelenchukskya13","ZELEN13M",  3451257.496,    3060268.081,    4391933.089,   13.0 },
	{ "", "", 0.0, 0.0, 0.0, 0.0 },	/* this must terminate the list */

	/* Known Missing:
		Hartebeesthoek VGOS 13.2m
		Nyalesund 13N VGOS 13.2m
		Gran Canaria VGOS 13.2m
		Flores VGOS 13.2m
		Matera VGOS
		Urumqi VGOS 13.2m
		Jilin VGOS 13m
		Kashi VGOS 13m
		Sanya VGOS 13m
		Tahiti VGOS 12.0m
		TNRT (Thailand)
		Thailand VGOS
		Svetloe VGOS 13.2m
		Evpatoria

	 * Sources of info:
	 	https://raw.githubusercontent.com/anothnagel/antenna-info/master/antenna-info.txt
		https://ivscc.gsfc.nasa.gov/about/org/components/ns-list.html
		https://ivscc.gsfc.nasa.gov/technology/vgos-sites.html
		https://cddis.nasa.gov/archive/vlbi/gsfc/ancillary/solve_apriori/blokq.c11.dat
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

	fprintf(out, "%s  (%3.1f, %3.1f, %3.1f) = (%s, %s, %4.2f) D=%3.1fm A=%3.1fkm mount=%s lon=%9.7f lat=%9.7f\n", ae->name, ae->x, ae->y, ae->z, latstr, lonstr, alt, ae->diameter, ae->arrayExtent, ae->mountType[0] ? ae->mountType : "AZEL", lon, lat);
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

/* (x, y, z) in meters in ITRF */
int isAntennaInGroup(double x, double y, double z, uint32_t group)
{
	const double tol = 5.0;	/* meters; tolerance per axis */
	int i;

	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if((group & antennaDBEntries[i].group) == 0)
		{
			continue;
		}

		if(fabs(antennaDBEntries[i].x - x) < tol &&
		   fabs(antennaDBEntries[i].y - y) < tol &&
		   fabs(antennaDBEntries[i].z - z) < tol)
		{
			return 1;
		}
		
	}

	return 0;
}
int isDifxAntennaInGroup(const DifxAntenna *da, uint32_t group)
{
	const double tol = 5.0;	/* meters; tolerance per axis */
	int i;

	if(!da)
	{
		fprintf(stderr, "Developer error: isDifxAntennaInGroup() called with null DifxAntenna pointer\n");

		return 0;
	}
	for(i = 0; antennaDBEntries[i].name[0]; ++i)
	{
		if((group & antennaDBEntries[i].group) == 0)
		{
			continue;
		}

		if(fabs(antennaDBEntries[i].x - da->X) < tol &&
		   fabs(antennaDBEntries[i].y - da->Y) < tol &&
		   fabs(antennaDBEntries[i].z - da->Z) < tol)
		{
			return 1;
		}
		
	}

	return 0;
}
