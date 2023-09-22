#ifndef EVEX_H
#define EVEX_H

/*************************************************************************/
/*                                                                       */
/* Defines a structure to hold the information in a def in the EVEX file */
/*                                                                       */
/* CJL, 1 December 1998                                                  */
/*                                                                       */
/*************************************************************************/

#define TM_RANDOM         1
#define TM_SEQUENTIAL     2
#define TM_SYNCHRONOUS    3
#define TM_CONTINUOUS     4

#define MIR_ALLOCATE      1<<0
#define MIR_NOALLOCATE    0

#define MIR_COMPARE       1<<1
#define MIR_NOCOMPARE     0

#define MIR_SAVEDIFFERENT 1<<2
#define MIR_SAVEALL       1<<3
#define MIR_NOSAVE        0

struct evex_struct
    {
    int		exper_num;
    char        ovex_name[MAX_PVALSIZE];
    char        lvex_name[MAX_PVALSIZE];
    char        cvex_name[MAX_PVALSIZE];
    char        svex_name[MAX_PVALSIZE];
    float	ap_length;
    float	speedup_factor;
    char	corr_config_key[MAX_NAMESIZE];
    int		nst;
    struct
        {
        char	station;
        char	su_config_key[MAX_NAMESIZE];
	} su_config[32];
    int         tape_mode;
    int		mirror;
    int realtime_latency;
    };

#endif
