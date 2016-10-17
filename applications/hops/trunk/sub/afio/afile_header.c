/************************************************************************/
/*									*/
/* Writes an informative header into an A-file.  The exact header	*/
/* written depends on the format version number and the data type. 	*/
/* The routine also identifies the program that wrote the file, and	*/
/* time-stamps it.							*/
/*									*/
/*	Inputs:		type		0, 1, 2, 3 or 4			*/
/*			version		Format version number		*/
/*			fp		open file stream		*/
/*									*/
/*	Output:		return value	0=OK, else error		*/
/*									*/
/* Created April 3 1995 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <time.h>
#include "mk4_afio.h"
#include "mk4_util.h"

					/* These are the header strings */

/************************************************************************/
/************************    Version 1    *******************************/
/************************************************************************/

char header10[] =
"*!NAME T X# SIZE EXP#   PROCDATE YR   RUN#   SOURCE   STATIONS    \
                                              ROOTCODE      ARCHV\n";

/************************************************************************/

char header11[] =
"*!NAME T X# SIZE EXP#   PROCDATE YR   RUN#   SOURCE   BSQ  STRT DUR\
 CSTRT/STP DRVS FQ           CLERR  CLDIFF ROOTCODE STATUS ARCHV\n";

/************************************************************************/

char header12[] =
"* NAME T X#  LEN EXP#   PROCDATE YR   RUN#   SOURCE   BSQ       \
TAPES       FM#C   AMP  SNR RPHAS SBDLY   MBDLY  DRATE ESDESP \
ARCHV EPOC TPHAS   TOTDRATE    TOTMBDELAY  SBRES  AMB X51 \
  PHASECALS     ROOTCODE\n\
*           (sec)                                               \
                            (deg) (usec)  (usec) (ps/s)       \
     (mmss)(deg)  (usec/sec)     (usec)   (usec) (usec) \n\
* \n";

/************************************************************************/
/************************    Version 2    *******************************/
/************************************************************************/

char header20[] =
"* ROOT   T X# SIZ EXP# PROCDATE   YR RUN#       SOURCE   STATIONS\n";

/************************************************************************/

char header21[] =
"* ROOT   T X# SIZ EXP# PROCDATE   YR RUN#       SOURCE   BSQ\
 DUR DRVS FQ CLERR   CLDIFF  STATUS\n";

/************************************************************************/

char header22[] =
"* ROOT   T X# LEN EXP# PROCDATE   YR RUN#       SOURCE   BSQ\
 FM#C   AMP   SNR  PH  TYP  SBDLY  MBDLY    AMB     DRATE  ELEVATION \
  AZIMUTH      U       V    ESDESP EPCH REF_FREQ TPHAS  TOTDRATE   \
  TOTMBDELAY  SBRES PARENT(S)\n\
*            (sec)                                          \
                  (deg)     (usec) (usec)  (usec)  (ps/s)   (deg)    \
   (deg)    (megalambda)          (mmss) (MHz)   (deg)  (usec/sec) \
  (usec)      (usec)\n";

/************************************************************************/

char header23[] =
"* EXP# T YR RUN#       SOURCE   FM TRI ROOTCODES            EXTENTS\
     LENGTHS        Q1Q2 ESDESP AMP         SNR  CPHS     CSBD\
   CMBD     AMBIG     CRATE   ELEVATIONS  AZIMUTHS      EPOCH  REFFREQ\n";

/************************************************************************/
/************************    Version 3    *******************************/
/************************************************************************/

char header30[] =
"* ROOT   T X# SIZ         EXP# PROCDATE   YR RUN#       SOURCE   STATIONS\n";

/************************************************************************/

char header31[] =
"* ROOT   T X# SIZ         EXP# PROCDATE   YR RUN#       SOURCE   BSQ\
 DUR DRVS FQ CLERR   CLDIFF  STATUS\n";

/************************************************************************/

char header32[] =
"* ROOT   T X# DUR LEN OFF EXP# PROCDATE   YR RUN#       SOURCE   BSQ\
 FM#C   AMP   SNR  PH   TYP  SBDLY  MBDLY    AMB    DRATE  ELEVATION \
  AZIMUTH      U       V    ESDESP EPCH REF_FREQ TPHAS  TOTDRATE   \
  TOTMBDELAY  SBRES PARENT(S)\n\
*            (    sec    )                                          \
                  (deg)     (usec) (usec)  (usec)  (ps/s)   (deg)    \
   (deg)    (megalambda)          (mmss) (MHz)   (deg)  (usec/sec) \
  (usec)      (usec)\n";

/************************************************************************/

char header33[] =
"* EXP# T YR RUN#       SOURCE   FM TRI ROOTCODES            EXTENTS\
     LENGTHS        DUR OFF Q1Q2 ESDESP AMP         SNR  CPHS     CSBD\
   CMBD     AMBIG     CRATE   ELEVATIONS  AZIMUTHS      EPOCH  REFFREQ\n";

/************************************************************************/

/************************************************************************/
/************************    Version 4    *******************************/
/************************************************************************/

char header40[] =
"* ROOT   T X# SIZ         EXP# PROCDATE     YR RUN#       SOURCE   STATIONS\n";

/************************************************************************/

char header41[] =
"* ROOT   T X# SIZ         EXP# PROCDATE     YR RUN#       SOURCE   BSQ\
 DUR LAG DRVS FQ CLERR   CLDIFF  STATUS\n";

/************************************************************************/

char header42[] =
"* ROOT   T X# DUR LEN OFF EXP# PROCDATE     YR RUN#           SOURCE   BSQ\
 FM#C LAGS   AMP    SNR  PH   SNR   TYP  SBDLY  MBDLY    AMB    DRATE  ELEVATION \
  AZIMUTH      U       V    ESDESP EPCH REF_FREQ TPHAS  TOTDRATE   \
  TOTMBDELAY  SBRES COHTIME PARENT(S)\n\
*            (    sec    )                                          \
                  (deg)     (usec) (usec)  (usec)  (ps/s)   (deg)    \
   (deg)    (megalambda)          (mmss) (MHz)   (deg)  (usec/sec) \
  (usec)      (usec) (sec)\n";

/************************************************************************/

char header43[] =
"* EXP# T YR RUN#           SOURCE   FM LAG TRI ROOTCODES            EXTENTS\
     LENGTHS        DUR OFF Q1Q2 ESDESP AMP         SNR  CPHS     CSBD\
   CMBD     AMBIG     CRATE   ELEVATIONS  AZIMUTHS      EPOCH  REFFREQ\n";

/************************************************************************/

/* gbc from correlator/prog/doc/file_formats/aformat.doc based on v4-> v5 */
/* corel run date used to be yyddd?hhmmss with ? supplying corel version */

/************************************************************************/
/************************    Version 5    *******************************/
/************************************************************************/

char header50[] =
"* ROOT   T X# SIZ         EXP# *************SCANID*************\
 PROCDATE     YEAR RUN#       SOURCE   STATIONS\n";

/************************************************************************/

char header51[] =
"* ROOT   T SIZ         EXP# *************SCANID*************\
 PROCDATE     YEAR RUN#       SOURCE   BS Q\
 DUR LAG DRVS FQ CLERR   CLDIFF  STATUS\n";

/************************************************************************/

char header52[] =
"* ROOT   T F# DUR  LEN  OFF  EXP# *************SCANID*************\
 PROCDATE     YEAR TIME*TAG OFF   SOURCE   BS Q\
 FM#C PL LAGS   AMP    SNR  PH   SNR   TYP  SBDLY  MBDLY    AMB   \
 DRATE  ELEVATION AZIMUTH      U       V    ESDESP\
 EPCH REF_FREQ TPHAS  TOTDRATE   TOTMBDELAY  TOTSBDMMBD COHTIMES\n\
*            (    sec    )                                          \
                  (deg)     (usec) (usec)  (usec)  (ps/s)   (deg)    \
   (deg)    (megalambda)          (mmss) (MHz)   (deg)  (usec/sec) \
  (usec)      (usec) (sec) *** NOT ALIGNED ***\n";

/************************************************************************/

char header53[] =
"* EXP# T *************SCANID*************\
 YEAR TIME*TAG OFF   SOURCE   FM LAGS TRI ROOTCODES\
 EXTENTS LENGTHS        DUR OFF Q1Q2 ESDESP AMP         SNR  CPHS     CSBD\
   CMBD     AMBIG     CRATE   ELEVATIONS  AZIMUTHS      EPOCH  REFFREQ\n";

/************************************************************************/


/* prototyping a version 6 */

char afile_com_char = '*';

/************************************************************************/
/************************    Version 6    *******************************/
/************************************************************************/

char header60[] =
"* ROOT   T X# SIZ         EXP# *************SCANID*************\
 PROCDATE     YEAR RUN#       SOURCE   STATIONS\n";

/************************************************************************/

char header61[] =
"* ROOT   T SIZ         EXP# *************SCANID*************\
 PROCDATE     YEAR RUN#       SOURCE   BS Q\
 DUR LAG DRVS FQ CLERR   CLDIFF  STATUS\n";

/************************************************************************/
/* aligned to write_fsumm.c:fformat_v6 */
char header62[] =
"%c col2 cl3 c4 cl5 cl6 cl7 col8     col9 \
         col10 cl11      col12 \
c13                            col14 15 16 \
c17 18 col19 \
        col20         col21       col22       col23 24 \
       col25        col26       col27 \
      col28 col29 \
col30  col31  col32   col33   col34  col35 \
cl36      col37      col38       col39         \
col40    col41    col42    col43      col44      col45         col46\n\
%c ROOT   T F# DUR LEN OFF EXP# SCANNAME PROCESSINGDATE YEAR DOY-HHMMSS \
OFF                       SOURCENAME BS QE\
 F#C PL #LAGS \
          AMP           SNR       PHASE        PSNR TY   \
     SBDLY        MBDLY         AMB     \
  DRATE ELref ELrem  AZref  AZrem       U       V ESDESP\
 EPCH   REF_FREQ   TOTPHASE    TOTDRATE   \
 TOTMBDELAY  TSBD-MBD SRCH-COH LOSS-COH         RA       DECL RESIDUALDELAY\n\
%c base64 2  . (s) (s) (s)    .        .              .    .          . \
  .                                .  . ..\
   .  .     . \
      (x10^4)             .       (deg)           . ..   \
    (usec)       (usec)      (usec)     \
 (ps/s) (deg) (deg)  (deg)  (deg)  (mega  lambda)      .\
 (ms)      (MHz)      (deg)  (usec/sec)   \
     (usec)    (usec)    (sec)    (sec)       (hr)      (deg)        (usec)\n";

/************************************************************************/

char header63[] = "\
%c col2 3     col4 col5       col6 cl7                             col8\
 c9 cl10 c11                col12\
       col13          col14\
 c16 c17 18 19  col20\
      col21    col22    col23 24   col25\
    col26     col27     col28\
          col29       col30 col31      col32   col33\n\
%c EXP# T SCANNAME YEAR PROCESSING OFF                           SOURCE\
 FM LAGS TRI RTCODE,RTCODE,RTCODE\
     EXTENTS        LENGTHS\
 DUR OFF Q1 Q2 ESDESP\
        AMP      SNR     CPHS DT    CSBD\
     CMBD     AMBIG     CRATE\
     ELEVATIONS    AZIMUTHS EPOCH    REFFREQ  COTIME\n\
%c    . .        .    .          .   .                                .\
  .    .   .                    .\
         (i)         (secs)\
 (s) (s)  .  .      .\
          .        .        .  .       .\
        .         .         .\
              .           .     .          .     (s)\n";

/*
 * Allow arbitrary comment character (version 6 and later)
 */
void set_afile_com_char(int star)
    {
    afile_com_char = star;
    }

/*
 * Identifier of comment lines for general use.
 * If it is a comment, return the comment character.
 */
int
afile_comment(char *line)
    {
    switch(line[0])
        {
        case '*': return('*'); break;   /* a traditional comment */
        case '#': return('#'); break;   /* modern practice comment */
        default: if (*line == afile_com_char) return(1); /* user-defined */
                 break;
        }
    return(0);  /* not a comment */
    }

/*
 * Writes the 3-line header of the requested version and type
 */
int
afile_header(int version, int type, FILE* fp)
    {
    int vertype, ret;
    time_t now;
    extern char progname[];
					/* Start with a time-stamp */
    now = time (NULL);
    fprintf(fp,"%c This file processed by %s, %s",
        afile_com_char, progname, ctime (&now));

					/* switch to appropriate header */
    vertype = version*10 + type;
    switch (vertype)
	{
					/* Fall through for version 0 */
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	    ret = fprintf (fp, 
		"* Data are of unknown version, so no header generated\n");
	    break;

	case 10:
	    ret = fprintf (fp, header10);
	    break;
	case 11:
	    ret = fprintf (fp, header11);
	    break;
	case 12:
	    ret = fprintf (fp, header12);
	    break;
	case 13:
	case 14:
	    break;

	case 20:
	    ret = fprintf (fp, header20);
	    break;
	case 21:
	    ret = fprintf (fp, header21);
	    break;
	case 22:
	    ret = fprintf (fp, header22);
	    break;
	case 23:
	    ret = fprintf (fp, header23);
	    break;
	case 24:
	    break;

	case 30:
	    ret = fprintf (fp, header30);
	    break;
	case 31:
	    ret = fprintf (fp, header31);
	    break;
	case 32:
	    ret = fprintf (fp, header32);
	    break;
	case 33:
	    ret = fprintf (fp, header33);
	    break;
	case 34:
	    break;

	case 40:
	    ret = fprintf (fp, header40);
	    break;
	case 41:
	    ret = fprintf (fp, header41);
	    break;
	case 42:
	    ret = fprintf (fp, header42);
	    break;
	case 43:
	    ret = fprintf (fp, header43);
	    break;
	case 44:
	    break;

	case 50:
	    ret = fprintf (fp, header50);
	    break;
	case 51:
	    ret = fprintf (fp, header51);
	    break;
	case 52:
	    ret = fprintf (fp, header52);
	    break;
	case 53:
	    ret = fprintf (fp, header53);
	    break;
	case 54:
	    break;

	case 60:
	    ret = fprintf (fp, header60);
	    break;
	case 61:
	    ret = fprintf (fp, header61);
	    break;
	case 62:
	    ret = fprintf (fp, header62,
                afile_com_char, afile_com_char, afile_com_char);
	    break;
	case 63:
	    ret = fprintf (fp, header63,
                afile_com_char, afile_com_char, afile_com_char);
	    break;
	case 64:
	    break;

	default:
	    ret = fprintf (fp, 
		"* Invalid afile_header() call, so no header generated\n");
	    break;
	}

    fflush (fp);

    if (ret < 0) return (1);
    else return (0);
    }
