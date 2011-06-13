#ifndef TYPE_4100_H
#define TYPE_4100_H

struct pcinfo {
	short		refampl;		/* Amplitude 0-10000, -1=manual */
	short		refphase;		/* -18000 to +18000 */
	short		reffreq;		/* in kilohertz */
	short		remampl;		/* Amplitude 0-10000, -1=manual */
	short		remphase;		/* -18000 to +18000 */
	short		remfreq;		/* in kilohertz */
};

struct type_4100 {
	short		record_id;		/*  */
	short		baseline_no;		/*  */
	short		spare[4];		/*  */
	struct sbands	corel_extent[14];	/* Corel extent # by sband/channel */
	struct pcinfo	pcal_data[14];		/* Pcal amp,phas,frq by sband/chnl */
	short		unused[10];		/*  */
};
	
#endif
