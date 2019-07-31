#ifndef gen_done

#define TRUE 1
#define FALSE 0


struct istats {
	short		ref;
	short		rem;
};

struct fstats {
	float		ref;
	float		rem;
};

struct dstats {
	double		ref;
	double		rem;
};

struct sbands {
	short		rsb;
	short		psb;
};

struct stsband {
	struct istats	rsb;
	struct istats	psb;
};

struct polar {
	float		ampl;
	float		phase;
};

struct datec {
	short		year;
	short		day_of_year;
	short		month;
	short		day_of_month;
	short		hour;
	short		minute;
	short		second;
};

struct datef {
	short		year;
	short		month;
	short		day;
	short		hour;
	short		minute;
	short		second;
};

struct lvpair {
	char		label[20];
	char		value[25];
};

#define gen_done

#endif
