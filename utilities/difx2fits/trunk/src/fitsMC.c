#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsMC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char bandFormFloat[4];

	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "Time of center of interval", "DAYS"},
		{"SOURCE_ID", "1J", "source id from sources tbl", 0},
		{"ANTENNA_NO", "1J", "antenna id from antennas tbl", 0},
		{"ARRAY", "1J", "array id number", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"ATMOS", "1D", "atmospheric group delay", "SECONDS"},
		{"DATMOS", "1D", "atmospheric group delay rate", "SEC/SEC"},
		{"GDELAY", "1D", "CALC geometric delay", "SECONDS"},
		{"GRATE", "1D", "CALC geometric delay rate", "SEC/SEC"},
		{"CLOCK_1", "1D", "electronic delay", "SECONDS"},
		{"DCLOCK_1", "1D", "electronic delay rate", "SEC/SEC"},
		{"LO_OFFSET_1", bandFormFloat, 
			"station lo_offset for polar. 1", "HZ"},
		{"DLO_OFFSET_1", bandFormFloat, 
			"station lo_offset rate for polar. 1", "HZ/SEC"},
		{"DISP_1", "1E", "dispersive delay", "SECONDS"},
		{"DDISP_1", "1E", "dispersive delay rate", "SEC/SEC"},
		{"CLOCK_2", "1D", "electronic delay", "SECONDS"},
		{"DCLOCK_2", "1D", "electronic delay rate", "SEC/SEC"},
		{"LO_OFFSET_2", bandFormFloat, 
			"station lo_offset for polar. 2", "HZ"},
		{"DLO_OFFSET_2", bandFormFloat, 
			"station lo_offset rate for polar. 2", "HZ/SEC"},
		{"DISP_2", "1E", "dispersive delay for polar 2", "SECONDS"},
		{"DDISP_2", "1E", "dispersive delay rate for polar 2", 
			"SEC/SEC"}
	};

	int nColumn;
 	int nRowBytes;
	char *pBuf, *fitsbuf;
	int nBand, nPol;
	int i, j, s, p, ant;
	int32_t stnId;
	int32_t arrayId;
	int32_t sourceId;
	int32_t freqId;
	double time;      
	double delay, drate;
	double atmos, arate;
	double clock, crate;

	if(D == 0)
	{
		return 0;
	}

	nBand = p_fits_keys->no_band;
	sprintf (bandFormFloat, "%1dE", nBand);  
  
	nPol = D->nPol;
	if(nPol == 2)
	{
		nColumn = NELEMENTS (columns);
	}
	else	/* don't populate last 6 columns if not full polar */
	{
		nColumn = NELEMENTS (columns) - 6;
	}
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS order */
	if ((fitsbuf = (char *)calloc (nRowBytes, 1)) == (char *)NULL)
	{
		return 0;
	}
  
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "MODEL_COMPS");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "FFT_SIZE", D->nOutChan*D->specAvg*2, "");
	fitsWriteInteger(out, "OVERSAMP", 0, "");
	fitsWriteInteger(out, "ZERO_PAD", 0, "");
	fitsWriteInteger(out, "FFT_TWID", 1, 
		"Version of FFT twiddle table used");
	fitsWriteString(out, "TAPER_FN", D->taperFunction, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	
	fitsWriteEnd(out);

	arrayId = 1;
	for(s = 0; s < D->nScan; s++)
	{
	   freqId = D->scan[s].configId + 1;
	   sourceId = D->scan[s].sourceId + 1;
	   for(p = 0; p < D->scan[s].nPoint; p++)
	   {
	      time = D->scan[s].mjdStart - (int)D->mjdStart + 
	      	D->modelInc*p/86400.0;
		
	      for(ant = 0; ant < D->nAntenna; ant++)
	      {
		stnId = ant + 1;
	        pBuf = fitsbuf;

		/* in general, convert from (us) to (sec) */
		atmos = D->scan[s].model[ant][p].a  * 1.0e-6;
		arate = D->scan[s].model[ant][p].da * 1.0e-6;
		/* here correct the sign of delay, and remove atmos portion of it. */
		delay = -D->scan[s].model[ant][p].t  * 1.0e-6 - atmos;
		drate = -D->scan[s].model[ant][p].dt * 1.0e-6 - arate;
		
		crate = D->antenna[ant].rate  * 1.0e-6;
		clock = D->antenna[ant].delay * 1.0e-6 + crate*D->modelInc*p;
          
	  	/* TIME */
		bcopy((char *)&time, pBuf, sizeof(time));
		pBuf += sizeof(time);
		
		/* SOURCE ID */
		bcopy((char *)&sourceId, pBuf, sizeof(sourceId));
		pBuf += sizeof(sourceId);
		
		/* STATION ID */
		bcopy((char *)&stnId, pBuf, sizeof(stnId));
		pBuf += sizeof(stnId);
		
		/* ARRAY ID */
		bcopy((char *)&arrayId, pBuf, sizeof(arrayId));
		pBuf += sizeof(arrayId);
		
		/* FREQ ID */
		bcopy((char *)&freqId, pBuf, sizeof(freqId));
		pBuf += sizeof(freqId);
		
		/* ATMOSPHERE DELAY */
		bcopy((char *)&atmos, pBuf, sizeof(atmos));
		pBuf += sizeof(atmos);
		
		/* ATMOSPHERE RATE */
		bcopy((char *)&arate, pBuf, sizeof(arate));
		pBuf += sizeof(arate);
		
		/* TOTAL DELAY */
		bcopy((char *)&delay, pBuf, sizeof(delay));
		pBuf += sizeof(delay);
		
		/* TOTAL RATE */
		bcopy((char *)&drate, pBuf, sizeof(drate));
		pBuf += sizeof(drate);
	  
		for (j = 0; j < nPol; j++)
                {
			/* CLOCK OFFSET */
			bcopy((char *)&clock, pBuf, sizeof(clock));
			pBuf += sizeof(clock);
              		
			/* CLOCK OFFSET RATE */
			bcopy((char *)&crate, pBuf, sizeof(crate));
			pBuf += sizeof(crate);
              
			/* LO OFFSET */
			for (i = 0; i < nBand; i++)
			{
				float zero = 0.0;
				bcopy((char *)&zero, pBuf, sizeof (zero));
				pBuf += sizeof (zero);
              		}
			
			/* LO OFFSET RATE */
			for (i = 0; i < nBand; i++)
			{
				float zero = 0.0;
				bcopy((char *)&zero, pBuf, sizeof (zero));
				pBuf += sizeof (zero);
              		}
			
			/* DISP  -- set to zero like VLBA correlator */
			{
				float zero = 0.0;
				bcopy((char *)&zero, pBuf, sizeof (zero));
				pBuf += sizeof (zero);
			}

			/* DDISP  -- set to zero like VLBA correlator */
			{
				float zero = 0.0;
				bcopy((char *)&zero, pBuf, sizeof (zero));
				pBuf += sizeof (zero);
			}
		} /* Polar loop */
      
		/* write buffer to output file */
#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
		fitsWriteBinRow(out, fitsbuf);
		
	     } /* Antenna loop */
	   } /* Intervals in scan loop */
	} /* Scan loop */
  
	/* release buffer space */
	free(fitsbuf);

	return D;
}
