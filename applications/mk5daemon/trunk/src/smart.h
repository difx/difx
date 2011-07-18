#ifndef __SMART_H__
#define __SMART_H__

#include <xlrapi.h>

#define N_SMART_DRIVES	8


typedef struct
{
	char model[XLR_MAX_DRIVENAME+1];
	char serial[XLR_MAX_DRIVESERIAL+1];
	char rev[XLR_MAX_DRIVEREV+1];
	int smartCapable;
	long long capacity;	/* in bytes */
} DriveInformation;

typedef struct
{
	double mjd;
	char vsn[10];
	S_SMARTVALUES smartXLR[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
	int id[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
	long long value[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
	int nValue[N_SMART_DRIVES];

	DriveInformation drive[N_SMART_DRIVES];
} Mk5Smart;

typedef struct
{
	int id;
	int critical;
	char desc[32];
} SmartDescription;

extern const SmartDescription smartDescriptions[];

const char *getSmartDescription(int smartId);



#endif
