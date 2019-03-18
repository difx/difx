#ifndef __VMF_H__
#define __VMF_H__

#define VMF_ANTENNA_NAME_LENGTH		12

/* these map directly to columns of data from http://vmf.geo.tuwien.ac.at/trop_products/VLBI/VMF3/VMF3_OP/daily/2019/2019074.vmf3_r */
typedef struct
{
	char antennaName[VMF_ANTENNA_NAME_LENGTH];
	double mjd;
	double a_hydrostatic;
	double a_wet;
	double zd_hydrostatic;	/* [m] "dry" zenith delay */
	double zd_wet;		/* [m] wet zenith delay */
	double pressure;	/* [hPa] */
	double temperature;	/* [C] */
	double pressure_wv;	/* [hPa] water vapor pressure */
} VMFData;

int loadVMFData(VMFData *data, int maxRows, int mjdStart, int nDay, int verbose);

int selectVMDData(const char *antennaName, VMFData **antennaData, int maxOut, VMFData *vmfData, int nData);

int calculateVMFDifxInput(DifxInput *D, const VMFData *vmfData, int vmfRows, int verbose);

#endif
