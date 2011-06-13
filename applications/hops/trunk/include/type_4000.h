#ifndef TYPE_4000_H
#define TYPE_4000_H

struct type_4000 {
	short		record_id;		/* record identifier */
	short		baseline_no;		/* baseline # as defined in root */
	char		baseline_id[2];		/* 2-letter baseline code */
	short		spare[3];		/*  */
	struct datef	utc_tag;		/* UTC time tag EPOCH for delay/rate */
	struct datef	utc_frnge;		/* UTC time of FRNGE processing */
	char		archive[8];		/* Archive name (#Axxxx) ?? */
	short		sample_rate;		/* Sample rate in kbits/sec */
	short		framepp;		/* # frames/parameter period */
	char		passno[2];		/* Pass number */
	short		channels;		/* U/L sideband pairs in this proc. */
	struct sbands	accum_per[14];		/* #acc. periods by sband/channel */
	struct stsband	rectrack[14];		/* track# by stat/sband/channel */
	short		corel_vers;		/* Corel version number */
	struct datef	utc_central;		/* UTC time tag for central epoch */
	short		hplu;			/* LU of frnge plot print (obsolete) */
	short		ref_drive;		/* Reference tape drive number */
	short		rem_drive;		/* Remote tape drive number */
	short		special_opt;		/* FRNGE special options flags */
	short		frnge_special;		/* Another mysterious special mode */
	short		unused[6];
};

#endif	
