#ifndef DATA_H
#define  DATA_H

#define MAXBASELINES 66
#define MAXACCPER 2048
#define MAXLAGS 128
#define MAX_T2000 100
#define ERROR 01
#define pi 3.141592654

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#include "root_records.h"
#include "fringe_records.h"
#include "type_corel.h"
#include "type_comp.h"

struct root_baseline {
	struct type_2000 t2000[MAX_T2000];
	struct type_2100 t2100;
	struct type_2200 t2200;
	struct type_2300 t2300;
	struct type_2400 t2400;
	struct type_2500 t2500;
	struct type_2600 t2600;
	struct type_2700 t2700;
	struct type_2800 t2800;
};

struct data_root {
	char filename[256];
	int modflag;
	int lo_offset;
	struct type_1000 t1000[10];
	struct root_baseline *base;
};

struct data_fringe {
	int n5000;
	int fplot_alloc;
	int alloc_5000;
	int padding;				/* Defuse data alignment */
						/* woes for doubles in 4400 */
	struct type_4000 t4000;
	struct type_4100 t4100;
	struct type_4200 t4200;
	struct type_4300 t4300;
	struct type_4400 t4400;
	struct type_4500 t4500;
	struct type_5000 *t5000;		/* To be allocated */
	char **fplot;				/* Allocated (128x120) */
};

#endif
