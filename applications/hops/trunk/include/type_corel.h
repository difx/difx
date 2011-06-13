union pos7_8 {
	int		fphase;			/* MkIII rotator phase end of AP */
	short		frame_cnt[2];		/* MkIIIA #x/y discards/resyncs */
};

struct cor {
	int		coscor;			/* Cosine correlation */
	int		sincor;			/* Sine correlation */
};

struct pcals {
	int		cos;			/* pcal cosine correlation */
	int		sin;			/* pcal sine correlation */
	int		bits;			/* pcal bits correlated */
	int		err;			/* # bytes with parity error */
};

struct type_corel {	/* NOTE: this is 128 bytes, half a record */
	unsigned short	base_index;		/* baseline # and index # encoded */
	short		flags;			/* lots of flag bits */
	short		status;			/* General status */
	unsigned short	bitshift;		/* Bit shift rate (absolute) */
	int		time;			/* Time at end of last PP in AP */
	union pos7_8	x;			/* Fphase or frame_cnt */
	int		delayz;			/* Delay (bits) */
	float 		fbit;			/* fractional bitshift center of AP */
	struct cor	xcor[8];		/* actual correlation results */
	int		cosbits;		/* cosine bits correlated */
	int		sinbits;		/* sine bits correlated */
	struct pcals	xypcals[2];		/* Pcal info, X then Y */
};
