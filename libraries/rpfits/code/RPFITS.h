/*--------------------------------------------------------------------------*/
/* RPFITS.h: Global enums and structs for RPFITS usage in C and C++.        */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  The Fortran standard requires that CHARACTER variables not be mixed     */
/*  with other types in COMMON and hence they have a separate COMMON block. */
/*                                                                          */
/*  DOUBLE PRECISION variables were also segregated into a separate COMMON  */
/*  block for reasons long forgotten.                                       */
/*                                                                          */
/* $Id: RPFITS.h,v 1.7 2008/09/11 06:42:01 cal103 Exp $            */
/*--------------------------------------------------------------------------*/

#ifndef ATNF_RPFITS_H
#define ATNF_RPFITS_H

#ifdef __cplusplus
extern "C" {
#endif

/* RPFITS Fortran subroutine arguments:                                     */
/*                                                                          */
/*   int   jstat, baseline, flag, bin, if_no, sourceno;                     */
/*   float ut, u, v, w, vis[2*nPol*nChan], weight[nPol*nChan];              */
/*                                                                          */
/* where nPol*nChan is the maximum product of polarizations times           */
/* channels for any single IF.                                              */

int rpfitsin_ (int *jstat, float *vis, float *wgt, int *baseline, float *ut,
               float *u, float *v, float *w, int *flag, int *bin, int *if_no,
               int *sourceno);

int rpfitsout_(int *jstat, float *vis, float *wgt, int *baseline, float *ut,
               float *u, float *v, float *w, int *flag, int *bin, int *if_no,
               int *sourceno);


/* Variables passed from COMMON blocks. */
enum {
  ant_max  = 15,
  max_card = 650,
  max_if   = 16,
  pol_max  = 8,
  max_su   = 500,
  max_fg   = 32,
  max_nx   = 256,
  max_mt   = 256,
  max_sc   = 16,
  max_cu   = 32
};


/* Error handling. */
extern struct __iostat_t {
  int rp_iostat;
  int errlun;
} iostat_;


/*    -------- Parameter values obtained from the RPFITS header --------    */
/*               (except intbase which is a random parameter)               */
/*                                                                          */
/* Miscellaneous parameters from RPFITS header cards (ncount, nfreq, nstok, */
/* freq, dfreq, ra, dec, and coord are defunct; use if_nfreq, if_nstok,     */
/* if_freq, if_bw, if_nfreq su_ra, and su_dec, instead and ignore ncount    */
/* and coord - now always J2000).                                           */
extern struct param_t {
  int nstok;
  int nfreq;
  int ncount;
  int intime;
  int nscan;
  int write_wt;
  int ncard;
  float intbase;
  int data_format;
} param_;


/* Ephemeris parameters from RPFITS header cards. */
extern struct ephem_t {
  int rp_defeat;
} ephem_;


/* Proper motion parameters from RPFITS header cards. */
extern struct proper_t {
  double pm_ra;
  double pm_dec;
  double pm_epoch;
} proper_;


/* Spectral line information from RPFITS header cards. */
extern struct spect_t {
  int ivelref;
} spect_;


/* Antenna table (embedded in the RPFITS header). */
extern struct anten_t {
  int nant;
  int ant_num[ant_max];
  int ant_mount[ant_max];
  int an_found;
} anten_;


/* Uncalibration table (embedded in the RPFITS header). */
extern struct cu_t {
  int n_cu;
  int cu_ant[max_cu];
  int cu_if[max_cu];
  int cu_ch1[max_cu];
  int cu_ch2[max_cu];
  int cu_found;
} cu_;


/* Flag table (embedded in the RPFITS header). */
extern struct fg_t {
  int n_fg;
  int fg_ant[2*max_fg];
  int fg_if[2*max_fg];
  int fg_chan[2*max_fg];
  int fg_stok[2*max_fg];
  int fg_found;
} fg_;


/* IF table (embedded in the RPFITS header). */
extern struct if_t {
  int n_if;
  int if_invert[max_if];
  int if_nfreq[max_if];
  int if_nstok[max_if];
  int if_sampl[max_if];
  int if_found;
  int if_num[max_if];
  int if_simul[max_if];
  int if_chain[max_if];
} if_;


/* Meteorological table (embedded in the RPFITS header). */
extern struct mt_t {
  int n_mt;
  int mt_ant[max_mt];
  int mt_found;
} mt_;


/* Index table (embedded in the RPFITS header). */
extern struct nx_t {
  int n_nx;
  int nx_rec[max_nx];
  int nx_found;
} nx_;


/* Source table (embedded in the RPFITS header). */
extern struct su_t {
  int n_su;
  int su_found;
  int su_num[max_su];
} su_;


extern struct doubles_t {
  double axis_offset[ant_max];			/* anten */
  double dec;					/* param */
  double dfreq;					/* param */
  double cu_cal1[max_cu];			/* cu    */
  double cu_cal2[max_cu];			/* cu    */
  double cu_ut[max_cu];				/* cu    */
  double feed_cal[ant_max*max_if*pol_max];	/* anten */
  double feed_pa[2*ant_max];			/* anten */
  double fg_ut[2*max_fg];			/* fg    */
  double freq;					/* param */
  double if_bw[max_if];				/* if    */
  double if_ref[max_if];			/* if    */
  double if_freq[max_if];			/* if    */
  double mt_humid[max_mt];			/* mt    */
  double mt_press[max_mt];			/* mt    */
  double mt_temp[max_mt];			/* mt    */
  double mt_ut[max_mt];				/* mt    */
  double nx_ut[max_nx];				/* nx    */
  double ra;					/* param */
  double rfreq;					/* spect */
  double rp_c[12];				/* ephem */
  double rp_djmrefp;				/* ephem */
  double rp_djmreft;				/* ephem */
  double rp_utcmtai;				/* ephem */
  double su_dec[max_su];			/* su    */
  double su_ra[max_su];				/* su    */
  double su_rad[max_su];			/* su    */
  double su_decd[max_su];			/* su    */
  double su_pra[max_su];			/* su    */
  double su_pdec[max_su];			/* su    */
  double su_prad[max_su];			/* su    */
  double su_pdecd[max_su];			/* su    */
  double vel1;					/* spect */
  double x[ant_max];				/* anten */
  double x_array;				/* anten */
  double y[ant_max];				/* anten */
  double y_array;				/* anten */
  double z[ant_max];				/* anten */
  double z_array;				/* anten */
} doubles_;


extern struct names_t {
  char object[16];				/* param */
  char instrument[16];				/* param */
  char cal[16];					/* param */
  char rp_observer[16];				/* param */
  char datobs[12];				/* param */
  char datwrit[12];				/* param */
  char file[256];				/* param */
  char datsys[8];				/* param */
  char version[20];				/* param */
  char coord[8];				/* param */
  char sta[ant_max*8];				/* anten */
  char feed_type[2*ant_max*2];			/* anten */
  char card[max_card*80];			/* param */
  char if_cstok[4*max_if*2];			/* if    */
  char su_name[max_su*16];			/* su    */
  char su_cal[max_su*4];			/* su    */
  char fg_reason[max_fg*24];			/* fg    */
  char nx_source[max_nx*16];			/* nx    */
  char nx_date[max_nx*12];			/* nx    */
  char rpfitsversion[20];			/* param */
  char bunit[16];				/* param */
  char obstype[16];				/* param */
  char errmsg[80];				/* iostat */
} names_;


/*     -------------- Values obtained from the RPFITS data --------------   */
/*                                                                          */
/* Syscal data. */
extern struct sc_t {
  float sc_ut;
  int sc_ant;
  int sc_if;
  int sc_q;
  float sc_cal[max_sc*max_if*ant_max];
  int sc_srcno;
} sc_;

#ifdef __cplusplus
}
#endif

#endif /* ATNF_RPFITS_H */
