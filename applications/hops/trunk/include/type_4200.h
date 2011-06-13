#ifndef TYPE_4200_H
#define TYPE_4200_H

struct type_4200 {
	short		record_id;		/* record identifier */
	short		baseline_no;		/* baseline # as defined in root */
	short		spare[4];		/*  */
	struct sbands	procutc[14];		/* Corel proc. UTC YDDD, sband/chan */
	struct stsband	errorate[14];		/* Tape error rate by stat/sband/ch */
	struct sbands	corel_index[14];	/* Corel index #s by sband/frnge ch# */
	short		frnge_error;		/* FRNGE error code, !=0 ==> bad */
	short		sbdoffset;		/* sbdelay offset flag, 1=yes */
	short		pcal_by_acc;		/* 1 ==> pcals by acc period */

					/* Added for fourfit CJL Dec 21 1993 */
					/* This was originally unused, but */
					/* these parameters may be valuable */
					/* with the new parameter extraction */
					/* capability in aedit */
	short		xperror;
	short		yperror;
	short		suppress;
	short		ppupdate;
	short		xslip;
	short		yslip;
	short		badsync;
};
	
#endif
