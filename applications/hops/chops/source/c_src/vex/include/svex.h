#ifndef SVEX_H
#define SVEX_H
/* svex.h
Thu Feb  4 15:47:28 EST 1999
*/

#define PCAL_SIN 0
#define PCAL_COS 1

struct SU_chan_out_struct
	{
	char	chan_ID[MAX_NAMESIZE];
	float	freq_a;			/* use -1 for unused frequency	*/
	float	freq_b;
	float	freq_c;
	float	freq_d;
	};

struct pcm_cfg_struct
	{
	char	freq[MAX_NAMESIZE];
	int	type;			/* PCAL_SIN or PCAL_COS		*/
	float	counter_coeff[9];	/* use 0 for unused coefficient	*/
	};

struct pcm_tab_struct
	{
	double	delta_phase;		/* Divides evenly into 360 degrees */
	int	nval;			/* Number of tabular values */
	short	sin_value[4][MAXNVAL];	/* [SampleValue][angle increment] */
	short	cos_value[4][MAXNVAL];	/* [SampleValue][angle increment] */
	};

struct svex_struct
	{
	struct	SU_chan_out_struct	SU_chan_out[16];
	struct	pcm_cfg_struct		pcm_cfg[8];	/* maximum of 8 freqs */
	struct	pcm_tab_struct 		tables;
	};

#endif
