
/* tapeDB.h 
 * For a tape-head offset database 
 * Revised:  2000 November 2, JAB */ 

#ifndef TAPEDB_VERSION 
#define TAPEDB_VERSION 2 

#define TAPEINFO "/correlator/tapelib/tapeinfo/" 
#define MAX_PASS 61 /* From mk4_ccfile.h */ 

#ifndef MAX_NAMESIZE 
#define MAX_NAMESIZE 32 
#endif 

#ifndef MAX_INDEX 
#define MAX_TAPE_LENGTH 19000 /* Feet */ 
#define POSITION_TOLERENCE 500 /* Feet */ 
#define MAX_INDEX (MAX_TAPE_LENGTH / POSITION_TOLERENCE) /* 38 */ 
#endif 

#ifndef ABS 
#define ABS(a)  (((a) < 0) ? -(a) : (a)) 
#endif 

#ifndef TRUE 
#define TRUE  1 
#define FALSE 0 
#endif 

struct head_pos { /* See also $SUMAN/su_db.h */ 
  int head_pos_offset[2]; /* Deci-microns */ 
      /* [0] = reverse pass, [1] = forward (from SU_db) */ 
  }; /* tape_footage[] */ 

struct tapeDB { /* Overall tape data, one per tape */ 
  char record_id[3]; /* To be "TDB" */ 
  char version_no[2]; /* From TAPEDB_VERSION above */ 
  char vsn[MAX_NAMESIZE]; /* VSN from sut */ 
  char station; /* From sut, same as mk4_site_id */ 
  short exper_num; /* From ovex */ 
  char exper_name[32]; /* From ovex */ 
  struct date start_time; /* From ovex (ignore HHMMSS) */ 
  char site_id[3]; /* From ovex st[] */ 
  /* (tape_id in ovex is usually null) */ 
  unsigned char sus[5]; /* Recently used SUs */ 
  int head[MAX_PASS]; /* Head number, 1 or 2, for this pass */ 
  int headpos[MAX_PASS]; /* Reference position for pass, deci-microns */ 
      /* [pass number], pass number is arbitrary */ 
  struct head_pos pass[MAX_PASS][MAX_INDEX]; /* Each pass pair */ 
      /* [pass number][footage/500] */ 
  }; /* *tapeinfo */ 

int tapehead(int thisSU, short Ondx, struct head_pos * * ptape_footage, 
    struct head_pos * * pphintsFootage); /* In tapehead.c */ 

#endif 

