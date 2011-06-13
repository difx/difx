#ifndef TYPE_5000_H
#define TYPE_5000_H

struct accrec {
	short		amplitude;		/* Amplitude, 30000 = 100% */
	short		phase;			/* Encoded phase */
	struct istats	pcal_phase;		/* Phasecal phase by station */
};

struct type_5000 {
	short		record_id;		/* record identifier */
	short		cont_number;		/* Continuation number (usually) */
	struct sbands	corel_index;		/* Corel index # by sideband */
	float		start_hr;		/* Start of 1st accper sec since hr */
	float		acc_period;		/* Accumulation period sec */
	float		start_epoch;		/* Start of 1st accper sec since ep. */
	short		unused[18];		/*  */
	struct accrec	data[25];		/* Actual data, 25 accpers */
};
	
#endif
