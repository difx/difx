#ifndef ADUMP_H
#define ADUMP_H

					/* BASELINE DATA */
					/* Quantities directly in A-file */
#define		VERSION		1
#define		ROOTCODE	2
#define		TYPE		3
#define		EXTENT		4
#define		DURATION	5
#define		LENGTH		6
#define		OFFSET		7
#define		EXPT_NO		8
#define		PROCDATE	9
#define		SCANYEAR	10
#define		TIMETAG		11
#define		SOURCE		12
#define		BASELINE	13
#define		QUALITY		14
#define		FREQ_CODE	15
#define		MODE		16
#define		NFREQ		17
#define		AMP		18
#define		SNR		19
#define		PHASE		20
#define		DATATYPE	21
#define		SBDELAY		22
#define		MBDELAY		23
#define		AMBIGUITY	24
#define		DRATE		25
#define		REF_ELEVATION	26
#define		REM_ELEVATION	27
#define		REF_AZIMUTH	28
#define		REM_AZIMUTH	29
#define		U		30
#define		V		31
#define		ESDESP		32
#define		EPOCH		33
#define		REF_FREQ	34
#define		TOT_PHASE	35
#define		TOT_DRATE	36
#define		TOT_MBDELAY	37
#define		TOT_MBDSBD	38
#define		SCOTIME		39
#define		NCOTIME		40
#define		PARENTS		41
					/* Quantities needing a bit of */
					/* derivation */
#define		PROCDAY		50
#define		SCANDAY		51
#define		EPOCHDAY	52

					/* TRIANGLE DATA */
					/* Quantities directly in A-file */
#define		T_VERSION	101
#define		T_EXPT_NO	102
#define		T_TYPE		103
#define		T_SCANYEAR	104
#define		T_TIMETAG	105
#define		T_SOURCE	106
#define		T_FREQ_CODE	107
#define		T_MODE		108
#define		T_TRIANGLE	109
#define		T_ROOT		110
#define		T_EXTENT	111
#define		T_LENGTH	112
#define		T_DURATION	113
#define		T_OFFSET	114
#define		T_SQUALITY	115
#define		T_DQUALITY	116
#define		T_ESDESP	117
#define		T_BISAMP	118
#define		T_BISSNR	119
#define		T_BISPHASE	120
#define		T_DATATYPE	121
#define		T_CSBDELAY	122
#define		T_CMBDELAY	123
#define		T_AMBIGUITY	124
#define		T_CDRATE	125
#define		T_ELEVATION	126
#define		T_AZIMUTH	127
#define		T_EPOCH		128
#define		T_REF_FREQ	129
					/* Quantities needing a bit of */
					/* derivation */
#define		T_SCANDAY	150
#define		T_EPOCHDAY	151

#define MAXFIELDS 50
#define TRUE 1
#define FALSE 0

struct flist
    {
    char	name[20];
    int		id;
    char	format[10];
    };

extern int  parse_bfields (int, char **, struct flist []);
extern int  parse_cmdline (int, char **, FILE **, FILE **, int *, int *,
                           struct flist []);
extern int  parse_tfields (int, char **, struct flist []);
extern int  strip_bline (char *, struct flist [], char *);
extern int  strip_tline (char *, struct flist [], char *);
extern int  write_header (FILE *, struct flist []);

#endif
