#ifndef __RADIOASTRON_H__
#define __RADIOASTRON_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
	double Delta_t;		/* the difference in time between the spacecraft and TT time frames, in seconds,
				   such that the spacecraft clock reads TT + \Delta t seconds at TT MJD time
				   mjd.fracDay.  T_{SC} = TT + \Delta t (s) */
	double dtdtau;		/* The rate of \Delta (s/s) */
} RadioastronTimeFrameOffset;

typedef struct
{
	double X[3];		/* unit vector for X axis (usually up or North)*/
	double Y[3];		/* unit vector for Y axis */
	double Z[3];		/* unit vector for Z axis (toward source) */
} RadioastronAxisVectors;

#ifdef __cplusplus
}
#endif

#endif
