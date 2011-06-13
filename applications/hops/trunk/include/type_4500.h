#ifndef TYPE_4500_H
#define TYPE_4500_H

struct type_4500 {
	short		record_id;		/* record identifier */
	short		baseline_no;		/* baseline # as defined in root */
	struct polar	ampphas[14];		/* Amp/phase by channel resid corel */
	struct fstats	pcalrate;		/* Phase cal rate by station */
	float		resid_delay;		/* Delay residual to corel apriori */
	float		delay_sigma;		/* Calculated delay error usec */
	float		resid_rate;		/* Delay rate resid to corel aprio */
	float		rate_error;		/* Delay rate error calc from data */
	float		corr_coeff;		/* Coherent multi-freq corr. coeff */
	float		totphase;		/* Total observed fringe phase deg */
	float		fr_asec_ew;		/* Fringes/arcsec, E-W */
	float		fr_asec_ns;		/* Fringes/arcsec, N-S */
	struct fstats	elevation;		/* Elevation at ref time by stat */
	float		aamplitude;		/* Fringe amplitude incoh chnl addn */
	float		ur_deriv;		/* U rate derivative mHz/arcsec */
	float		vr_deriv;		/* V rate derivative mHz/arcsec */
	float		search_parm[6];		/* Search parameters */
	float		sbd_resid;		/* Singleband delay residual usec */
	float		snr;			/* SNR in sigmas */
	float		prob_false;		/* Probability of false detection */
	float		amplitude;		/* Incoh. segmented fringe ampl */
	float		totphase_ec;		/* Total phase at earth center */
	float		resphase_ec;		/* Residual phase at earth center */
	float		start;			/* start time in sec past hour */
	float		stop;			/* stop time in sec past hour */
	float		epoch_offset;		/* epoch offset from run center sec */
	float		intg_time;		/* Effective run duration in sec */
	float		sbd_error;		/* Sbdelay error in usec */
	float		accept_ratio;		/* %Ratio min to max data accepted */
	float		discard;		/* % data discarded */
	float		totphase_cen;		/* Total phase at central epoch */
};
	
#endif
