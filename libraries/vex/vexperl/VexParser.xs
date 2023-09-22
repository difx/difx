
//#define YYSTYPE PERLSTYPE /* Remove these comments it you get compile problems */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
//#undef YYSTYPE /* Remove these comments it you get compile problems */

#define block vexblock
#include <vex.h>
#include <y.tab.h>
#undef block

typedef struct vex vex;

/* Not done */
/*void print_def_block(struct llist *items, void func());*/

/*void print_external(struct external *this);*/


/*  Export constants */

static double
constant(char *name, int arg) {
  errno = 0;
  switch (*name) {
  case 'B':
    if (strEQ(name, "B_ANTENNA"))
      return B_ANTENNA;
    if (strEQ(name, "B_BBC"))
      return B_BBC;
    if (strEQ(name, "B_CLOCK"))
      return B_CLOCK;
    if (strEQ(name, "B_CORR"))
      return B_CORR;
    if (strEQ(name, "B_DAS"))
      return B_DAS;
    if (strEQ(name, "B_EOP"))
      return B_EOP;
    if (strEQ(name, "B_EXPER"))
      return B_EXPER;
    if (strEQ(name, "B_FREQ"))
      return B_FREQ;
    if (strEQ(name, "B_GLOBAL"))
      return B_GLOBAL;
    if (strEQ(name, "B_HEAD_POS"))
      return B_HEAD_POS;
    if (strEQ(name, "B_IF"))
      return B_IF;
    if (strEQ(name, "B_MODE"))
      return B_MODE;
    if (strEQ(name, "B_PASS_ORDER"))
      return B_PASS_ORDER;
    if (strEQ(name, "B_PHASE_CAL_DETECT"))
      return B_PHASE_CAL_DETECT;
    if (strEQ(name, "B_PROCEDURES"))
      return B_PROCEDURES;
    if (strEQ(name, "B_ROLL"))
      return B_ROLL;
    if (strEQ(name, "B_SCHED"))
      return B_SCHED;
    if (strEQ(name, "B_SCHEDULING_PARAMS"))
      return B_SCHEDULING_PARAMS;
    if (strEQ(name, "B_SEFD"))
      return B_SEFD;
    if (strEQ(name, "B_SITE"))
      return B_SITE;
    if (strEQ(name, "B_SOURCE"))
      return B_SOURCE;
    if (strEQ(name, "B_STATION"))
      return B_STATION;
    if (strEQ(name, "B_TAPELOG_OBS"))
      return B_TAPELOG_OBS;
    if (strEQ(name, "B_TRACKS"))
      return B_TRACKS;
    break;
  case 'T':
    if (strEQ(name, "T_A1_TAI"))
      return T_A1_TAI;
    if (strEQ(name, "T_ANGLE"))
      return T_ANGLE;
    if (strEQ(name, "T_ANTENNA_DIAM"))
      return T_ANTENNA_DIAM;
    if (strEQ(name, "T_ANTENNA_MOTION"))
      return T_ANTENNA_MOTION;
    if (strEQ(name, "T_ARG_PERIGEE"))
      return T_ARG_PERIGEE;
    if (strEQ(name, "T_ASCENDING_NODE"))
      return T_ASCENDING_NODE;
    if (strEQ(name, "T_AXIS_OFFSET"))
      return T_AXIS_OFFSET;
    if (strEQ(name, "T_AXIS_TYPE"))
      return T_AXIS_TYPE;
    if (strEQ(name, "T_BBC_ASSIGN"))
      return T_BBC_ASSIGN;
    if (strEQ(name, "T_BITS_PER_SAMPLE"))
      return T_BITS_PER_SAMPLE;
    if (strEQ(name, "T_CHAN_DEF"))
      return T_CHAN_DEF;
    if (strEQ(name, "T_CLOCK_EARLY"))
      return T_CLOCK_EARLY;
    if (strEQ(name, "T_COMMENT"))
      return T_COMMENT;
    if (strEQ(name, "T_COMMENT_TRAILING"))
      return T_COMMENT_TRAILING;
    if (strEQ(name, "T_CONTACT_EMAIL"))
      return T_CONTACT_EMAIL;
    if (strEQ(name, "T_CONTACT_NAME"))
      return T_CONTACT_NAME;
    if (strEQ(name, "T_DATA_MODULATION"))
      return T_DATA_MODULATION;
    if (strEQ(name, "T_DEC"))
      return T_DEC;
    if (strEQ(name, "T_DEC_RATE"))
      return T_DEC_RATE;
    if (strEQ(name, "T_DEF"))
      return T_DEF;
    if (strEQ(name, "T_ECCENTRICITY"))
      return T_ECCENTRICITY;
    if (strEQ(name, "T_ELECTRONICS_RACK_TYPE"))
      return T_ELECTRONICS_RACK_TYPE;
    if (strEQ(name, "T_ENDDEF"))
      return T_ENDDEF;
    if (strEQ(name, "T_ENDSCAN"))
      return T_ENDSCAN;
    if (strEQ(name, "T_EOP_INTERVAL"))
      return T_EOP_INTERVAL;
    if (strEQ(name, "T_EOP_REF_EPOCH"))
      return T_EOP_REF_EPOCH;
    if (strEQ(name, "T_EXPER_DESCRIPTION"))
      return T_EXPER_DESCRIPTION;
    if (strEQ(name, "T_EXPER_NAME"))
      return T_EXPER_NAME;
    if (strEQ(name, "T_EXPER_NOMINAL_START"))
      return T_EXPER_NOMINAL_START;
    if (strEQ(name, "T_EXPER_NOMINAL_STOP"))
      return T_EXPER_NOMINAL_STOP;
    if (strEQ(name, "T_EXPER_NUM"))
      return T_EXPER_NUM;
    if (strEQ(name, "T_FANIN_DEF"))
      return T_FANIN_DEF;
    if (strEQ(name, "T_FANOUT_DEF"))
      return T_FANOUT_DEF;
    if (strEQ(name, "T_HEADSTACK"))
      return T_HEADSTACK;
    if (strEQ(name, "T_HEADSTACK_MOTION"))
      return T_HEADSTACK_MOTION;
    if (strEQ(name, "T_HEADSTACK_POS"))
      return T_HEADSTACK_POS;
    if (strEQ(name, "T_HORIZON_MAP_AZ"))
      return T_HORIZON_MAP_AZ;
    if (strEQ(name, "T_HORIZON_MAP_EL"))
      return T_HORIZON_MAP_EL;
    if (strEQ(name, "T_IAU_NAME"))
      return T_IAU_NAME;
    if (strEQ(name, "T_IF_DEF"))
      return T_IF_DEF;
    if (strEQ(name, "T_INCLINATION"))
      return T_INCLINATION;
    if (strEQ(name, "T_LINK"))
      return T_LINK;
    if (strEQ(name, "T_LITERAL"))
      return T_LITERAL;
    if (strEQ(name, "T_MEAN_ANOMALY"))
      return T_MEAN_ANOMALY;
    if (strEQ(name, "T_MEAN_MOTION"))
      return T_MEAN_MOTION;
    if (strEQ(name, "T_MIDOB_CAL"))
      return T_MIDOB_CAL;
    if (strEQ(name, "T_MODE"))
      return T_MODE;
    if (strEQ(name, "T_NAME"))
      return T_NAME;
    if (strEQ(name, "T_NEW_SOURCE_COMMAND"))
      return T_NEW_SOURCE_COMMAND;
    if (strEQ(name, "T_NEW_TAPE_SETUP"))
      return T_NEW_TAPE_SETUP;
    if (strEQ(name, "T_NUMBER_DRIVES"))
      return T_NUMBER_DRIVES;
    if (strEQ(name, "T_NUM_EOP_POINTS"))
      return T_NUM_EOP_POINTS;
    if (strEQ(name, "T_OCCUPATION_CODE"))
      return T_OCCUPATION_CODE;
    if (strEQ(name, "T_OCEAN_LOAD_HORIZ"))
      return T_OCEAN_LOAD_HORIZ;
    if (strEQ(name, "T_OCEAN_LOAD_VERT"))
      return T_OCEAN_LOAD_VERT;
    if (strEQ(name, "T_ORBIT_EPOCH"))
      return T_ORBIT_EPOCH;
    if (strEQ(name, "T_PARITY_CHECK"))
      return T_PARITY_CHECK;
    if (strEQ(name, "T_PASS_ORDER"))
      return T_PASS_ORDER;
    if (strEQ(name, "T_PHASE_CAL_DETECT"))
      return T_PHASE_CAL_DETECT;
    if (strEQ(name, "T_PI_EMAIL"))
      return T_PI_EMAIL;
    if (strEQ(name, "T_PI_NAME"))
      return T_PI_NAME;
    if (strEQ(name, "T_POINTING_SECTOR"))
      return T_POINTING_SECTOR;
    if (strEQ(name, "T_POSTOB_CAL"))
      return T_POSTOB_CAL;
    if (strEQ(name, "T_PREOB_CAL"))
      return T_PREOB_CAL;
    if (strEQ(name, "T_PROCEDURE_NAME_PREFIX"))
      return T_PROCEDURE_NAME_PREFIX;
    if (strEQ(name, "T_RA"))
      return T_RA;
    if (strEQ(name, "T_RA_RATE"))
      return T_RA_RATE;
    if (strEQ(name, "T_RECORDING_SYSTEM_ID"))
      return T_RECORDING_SYSTEM_ID;
    if (strEQ(name, "T_RECORD_DENSITY"))
      return T_RECORD_DENSITY;
    if (strEQ(name, "T_RECORD_TRANSPORT_TYPE"))
      return T_RECORD_TRANSPORT_TYPE;
    if (strEQ(name, "T_REF"))
      return T_REF;
    if (strEQ(name, "T_REF_COORD_FRAME"))
      return T_REF_COORD_FRAME;
    if (strEQ(name, "T_ROLL"))
      return T_ROLL;
    if (strEQ(name, "T_ROLL_DEF"))
      return T_ROLL_DEF;
    if (strEQ(name, "T_ROLL_INC_PERIOD"))
      return T_ROLL_INC_PERIOD;
    if (strEQ(name, "T_ROLL_REINIT_PERIOD"))
      return T_ROLL_REINIT_PERIOD;
    if (strEQ(name, "T_S2_DATA_SOURCE"))
      return T_S2_DATA_SOURCE;
    if (strEQ(name, "T_S2_GROUP_ORDER"))
      return T_S2_GROUP_ORDER;
    if (strEQ(name, "T_S2_RECORDING_MODE"))
      return T_S2_RECORDING_MODE;
    if (strEQ(name, "T_SAMPLE_RATE"))
      return T_SAMPLE_RATE;
    if (strEQ(name, "T_SCAN"))
      return T_SCAN;
    if (strEQ(name, "T_SCHEDULER_EMAIL"))
      return T_SCHEDULER_EMAIL;
    if (strEQ(name, "T_SCHEDULER_NAME"))
      return T_SCHEDULER_NAME;
    if (strEQ(name, "T_SEFD"))
      return T_SEFD;
    if (strEQ(name, "T_SEFD_MODEL"))
      return T_SEFD_MODEL;
    if (strEQ(name, "T_SEMI_MAJOR_AXIS"))
      return T_SEMI_MAJOR_AXIS;
    if (strEQ(name, "T_SETUP_ALWAYS"))
      return T_SETUP_ALWAYS;
    if (strEQ(name, "T_SITE_ID"))
      return T_SITE_ID;
    if (strEQ(name, "T_SITE_NAME"))
      return T_SITE_NAME;
    if (strEQ(name, "T_SITE_POSITION"))
      return T_SITE_POSITION;
    if (strEQ(name, "T_SITE_POSITION_EPOCH"))
      return T_SITE_POSITION_EPOCH;
    if (strEQ(name, "T_SITE_POSITION_REF"))
      return T_SITE_POSITION_REF;
    if (strEQ(name, "T_SITE_TYPE"))
      return T_SITE_TYPE;
    if (strEQ(name, "T_SITE_VELOCITY"))
      return T_SITE_VELOCITY;
    if (strEQ(name, "T_SOURCE"))
      return T_SOURCE;
    if (strEQ(name, "T_SOURCE_MODEL"))
      return T_SOURCE_MODEL;
    if (strEQ(name, "T_SOURCE_NAME"))
      return T_SOURCE_NAME;
    if (strEQ(name, "T_SOURCE_POSITION_EPOCH"))
      return T_SOURCE_POSITION_EPOCH;
    if (strEQ(name, "T_SOURCE_POSITION_REF"))
      return T_SOURCE_POSITION_REF;
    if (strEQ(name, "T_SOURCE_TYPE"))
      return T_SOURCE_TYPE;
    if (strEQ(name, "T_START"))
      return T_START;
    if (strEQ(name, "T_STATION"))
      return T_STATION;
    if (strEQ(name, "T_SWITCHING_CYCLE"))
      return T_SWITCHING_CYCLE;
    if (strEQ(name, "T_TAI_UTC"))
      return T_TAI_UTC;
    if (strEQ(name, "T_TAPE_CHANGE"))
      return T_TAPE_CHANGE;
    if (strEQ(name, "T_TAPE_CONTROL"))
      return T_TAPE_CONTROL;
    if (strEQ(name, "T_TAPE_LENGTH"))
      return T_TAPE_LENGTH;
    if (strEQ(name, "T_TAPE_MOTION"))
      return T_TAPE_MOTION;
    if (strEQ(name, "T_TAPE_PREPASS"))
      return T_TAPE_PREPASS;
    if (strEQ(name, "T_TARGET_CORRELATOR"))
      return T_TARGET_CORRELATOR;
    if (strEQ(name, "T_TRACK_FRAME_FORMAT"))
      return T_TRACK_FRAME_FORMAT;
    if (strEQ(name, "T_UT1_UTC"))
      return T_UT1_UTC;
    if (strEQ(name, "T_VELOCITY_WRT_LSR"))
      return T_VELOCITY_WRT_LSR;
    if (strEQ(name, "T_VEX_REV"))
      return T_VEX_REV;
    if (strEQ(name, "T_VLBA_FRMTR_SYS_TRK"))
      return T_VLBA_FRMTR_SYS_TRK;
    if (strEQ(name, "T_VLBA_TRNSPRT_SYS_TRK"))
      return T_VLBA_TRNSPRT_SYS_TRK;
    if (strEQ(name, "T_VSN"))
      return T_VSN;
    if (strEQ(name, "T_X_WOBBLE"))
      return T_X_WOBBLE;
    if (strEQ(name, "T_Y_WOBBLE"))
      return T_Y_WOBBLE;
    if (strEQ(name, "T_ZEN_ATMOS"))
      return T_ZEN_ATMOS;
    break;
  }
  errno = EINVAL;
  return 0;
}

MODULE = Astro::VexParser	PACKAGE = Astro::VexParser

int
vex_open(vexfile, vexptr)
  char *  vexfile
  vex *   vexptr = NO_INIT
 PROTOTYPE: $$
 CODE:
   RETVAL = vex_open(vexfile, &vexptr);
 OUTPUT:
   RETVAL
   vexptr

int
lowl2int(lowl)
  char *  lowl
 PROTOTYPE: $
 CODE:
  RETVAL = lowl2int(lowl);
 OUTPUT:
  RETVAL

int
block2int(block)
  char *  block
 PROTOTYPE: $
 CODE:
  RETVAL = block2int(block);
 OUTPUT:
  RETVAL

char *
int2lowl(lowl)
  int     lowl
 PROTOTYPE: $
 CODE:
  RETVAL = int2lowl(lowl);
 OUTPUT:
  RETVAL

void
print_vex(vexptr)
  vex *   vexptr
 PROTOTYPE: $
 CODE:
   print_vex(vexptr);

void 
print_vex_blocks(blocks)
  Llist * blocks

void 
print_block_name(block)
  int     block

void 
print_qref_block(items)
  Llist * items

void 
print_qualifiers(items)
  Llist * items

void
print_lowl(items)
  Llist * items

void
print_lowl_st(statement, ptr)
  int     statement
  void *  ptr

void
print_svalue(svalue)
  char *  svalue

void
print_literal_list(svalues)
  Llist * svalues

void
print_comment(comment)
  char *  comment

void
print_comment_trailing(comment_trailing)
  char * comment_trailing

char *
int2block(block)
  int     block
 PROTOTYPE: $
 CODE:
  RETVAL = int2block(block);
 OUTPUT:
  RETVAL

int
vex_field(statement, lowlptr, i, link, name, value, units)
  int     statement
  void *  lowlptr
  int     i
  int     link = NO_INIT
  int     name = NO_INIT
  char *  value = NO_INIT
  char *  units = NO_INIT
 PROTOTYPE: $$$$$$$
 CODE:
  RETVAL = vex_field(statement, lowlptr, i, &link, &name, &value, &units);
 OUTPUT:
  RETVAL
  link
  name
  value
  units

char *
get_source_def(vexptr)
  vex *   vexptr
 PROTOTYPE: $
 CODE:
  RETVAL = get_source_def(vexptr);
 OUTPUT:
  RETVAL

char *
get_source_def_next()
 PROTOTYPE: 
 CODE:
  RETVAL = get_source_def_next();
 OUTPUT:
  RETVAL

char *
get_mode_def(vexptr)
  vex *   vexptr
 PROTOTYPE: $
 CODE:
  RETVAL = get_mode_def(vexptr);
 OUTPUT:
  RETVAL

char *
get_mode_def_next()
 PROTOTYPE: 
 CODE:
  RETVAL = get_mode_def_next();
 OUTPUT:
  RETVAL

char *
get_station_def(vexptr)
  vex *   vexptr
 PROTOTYPE: $
 CODE:
  RETVAL = get_station_def(vexptr);
 OUTPUT:
  RETVAL

char *
get_station_def_next()
 PROTOTYPE: 
 CODE:
  RETVAL = get_station_def_next();
 OUTPUT:
  RETVAL

void *
get_all_lowl(station, mode, statement, primitive, vexptr)
  char *  station
  char *  mode
  int     statement
  int     primitive
  vex *   vexptr
 PROTOTYPE: $$$$$
 CODE:
  RETVAL = get_all_lowl(station, mode, statement, primitive, vexptr);
 OUTPUT:
  RETVAL

void *
get_all_lowl_next()
 PROTOTYPE:
 CODE:
  RETVAL = get_all_lowl_next();
 OUTPUT:
  RETVAL

void *
get_mode_lowl(station, mode, statement, primitive, vexptr)
  char *  station
  char *  mode
  int     statement
  int     primitive
  vex *   vexptr
 PROTOTYPE: $$$$$
 CODE:
  RETVAL = get_mode_lowl(station, mode, statement, primitive, vexptr);
 OUTPUT:
  RETVAL

void *
get_mode_lowl_next()
 PROTOTYPE:
 CODE:
  RETVAL = get_mode_lowl_next();
 OUTPUT:
  RETVAL

void *
get_station_lowl(station, statement, primitive, vexptr)
  char *  station
  int     statement
  int     primitive
  vex *   vexptr
 PROTOTYPE: $$$$
 CODE:
  RETVAL = get_station_lowl(station, statement, primitive, vexptr);
 OUTPUT:
  RETVAL

void *
get_station_lowl_next()
 PROTOTYPE:
 CODE:
  RETVAL = get_station_lowl_next();
 OUTPUT:
  RETVAL

void *
get_source_lowl(source, statement, vexptr)
  char *  source
  int     statement
  vex *   vexptr
 PROTOTYPE: $$$
 CODE:
  RETVAL = get_source_lowl(source, statement, vexptr);
 OUTPUT:
  RETVAL

void *
get_source_lowl_next()
 PROTOTYPE:
 CODE:
  RETVAL = get_source_lowl_next();
 OUTPUT:
  RETVAL

void *
get_global_lowl(primitive, statement, vexptr)
  int     primitive
  int     statement
  vex *   vexptr
 PROTOTYPE: $$$
 CODE:
  RETVAL = get_global_lowl(primitive, statement, vexptr);
 OUTPUT:
  RETVAL

void *
get_global_lowl_next()
 PROTOTYPE:
 CODE:
  RETVAL = get_global_lowl_next();
 OUTPUT:
  RETVAL

Llist *
find_block(block, vexptr)
  int     block
  vex *   vexptr
 PROTOTYPE: $$
 CODE:
  RETVAL = find_block(block, vexptr);
 OUTPUT:
  RETVAL

Llist *
find_def(defs, mode)
  Llist * defs
  char *  mode
 PROTOTYPE: $$
 CODE:
  RETVAL = find_def(defs, mode);
 OUTPUT:
  RETVAL

Llist *
find_next_def(defs)
  Llist * defs
 PROTOTYPE: $
 CODE:
  RETVAL = find_next_def(defs);
 OUTPUT:
  RETVAL

Llist *
find_next_scan(defs)
  Llist * defs
 PROTOTYPE: $
 CODE:
  RETVAL = find_next_scan(defs);
 OUTPUT:
  RETVAL

Llist *
find_lowl(lowls, statement)
  Llist * lowls
  int     statement
 PROTOTYPE: $$
 CODE:
  RETVAL = find_lowl(lowls, statement);
 OUTPUT:
  RETVAL

char *
get_scan_start(lowls)
  void * lowls
 PROTOTYPE: $
 CODE:
  RETVAL = (char *)get_scan_start(lowls);
 OUTPUT:
  RETVAL

char *
get_scan_mode(lowls)
  void * lowls
 PROTOTYPE: $
 CODE:
  RETVAL = (char *)get_scan_mode(lowls);
 OUTPUT:
  RETVAL

char *
get_scan_source(lowls_scan)
  void * lowls_scan
 PROTOTYPE: $
 CODE:
  RETVAL = (char *)get_scan_source(lowls_scan);
 OUTPUT:
  RETVAL

char *
get_scan_source_next()
 PROTOTYPE: 
 CODE:
  RETVAL = (char *)get_scan_source_next();
 OUTPUT:
  RETVAL

void *
get_scan_station(lowls_scan, scanid, station, vexptr)
  Llist * lowls_scan = NO_INIT
  char * scanid
  char *  station
  vex *   vexptr
 PROTOTYPE: $$$
 CODE:
  RETVAL = get_scan_station(&lowls_scan, &scanid, station, vexptr);
 OUTPUT:
  RETVAL
  lowls_scan

void *
get_scan_station_next(lowls_scan, scanid)
  char * scanid
  Llist * lowls_scan = NO_INIT
 PROTOTYPE: $
 CODE:
  RETVAL = get_scan_station_next(&lowls_scan, &scanid);
 OUTPUT:
  RETVAL
  lowls_scan


void *
get_scan(scanid, vexptr)
  char * scanid
  vex *   vexptr
 PROTOTYPE: $$
 CODE:
  RETVAL = get_scan(&scanid, vexptr);
 OUTPUT:
   RETVAL
   scanid

void *
get_scan_next(scanid)
  char * scanid
 PROTOTYPE: $
 CODE:
  RETVAL = get_scan_next(&scanid);
 OUTPUT:
   RETVAL
   scanid

void *
get_station_scan(lowls_in)
  void * lowls_in
 PROTOTYPE: $
 CODE:
   RETVAL = get_station_scan(lowls_in);
 OUTPUT:
  RETVAL

void *
get_station_scan_next()
 PROTOTYPE: 
 CODE:
   RETVAL = get_station_scan_next();
 OUTPUT:
  RETVAL

double
constant(name,arg)
  char *  name
  int     arg


#void *
#print_def_block(items)
#  Llist * items
# PROTOTYPE: $
# CODE:
#  print_def_block(items, print_lowl);
