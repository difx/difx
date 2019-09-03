%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vex.h"

#define YYDEBUG 1

/* globals */

struct vex *vex_ptr=NULL;
extern int lines;

int yyparse();
int yylex();
void yyerror(const char *s);
 
%}

%union
{
int                     ival;
char                   *sval;
struct llist           *llptr;
struct qref            *qrptr;
struct def             *dfptr;
struct block           *blptr;
struct lowl            *lwptr;
struct dvalue          *dvptr;
struct external        *exptr;

struct chan_def        *cdptr;
struct switching_cycle *scptr;

struct station         *snptr;
struct data_transfer   *dtptr;

struct axis_type       *atptr;
struct antenna_motion  *amptr;
struct pointing_sector *psptr;

struct bbc_assign      *baptr;

struct headstack       *hsptr;

struct clock_early     *ceptr;

struct tape_length     *tlptr;
struct tape_motion     *tmptr;

struct headstack_pos   *hpptr;

struct if_def          *ifptr;

struct phase_cal_detect *pdptr;

struct setup_always    *saptr;
struct parity_check    *pcptr;
struct tape_prepass    *tpptr;
struct preob_cal       *prptr;
struct midob_cal       *miptr;
struct postob_cal      *poptr;

struct sefd            *septr;

struct site_position   *spptr;
struct site_velocity   *svptr;
struct ocean_load_vert *ovptr;
struct ocean_load_horiz *ohptr;

struct source_model    *smptr;

struct vsn             *vsptr;

struct fanin_def	*fiptr;
struct fanout_def	*foptr;
struct vlba_frmtr_sys_trk	*fsptr;
struct s2_data_source  *dsptr;

}

%token <ival>	T_VEX_REV T_REF T_DEF T_ENDDEF T_SCAN T_ENDSCAN

%token <ival>	T_CHAN_DEF T_SAMPLE_RATE T_BITS_PER_SAMPLE T_SWITCHING_CYCLE

%token <ival>	T_START T_SOURCE T_MODE T_STATION T_DATA_TRANSFER

%token <ival>	T_ANTENNA_DIAM T_AXIS_OFFSET T_ANTENNA_MOTION T_POINTING_SECTOR
%token <ival>   T_AXIS_TYPE

%token <ival>   T_BBC_ASSIGN

%token <ival>	T_CLOCK_EARLY

%token <ival>   T_RECORD_TRANSPORT_TYPE T_ELECTRONICS_RACK_TYPE T_NUMBER_DRIVES
%token <ival>   T_HEADSTACK T_RECORD_DENSITY T_TAPE_LENGTH
%token <ival>   T_RECORDING_SYSTEM_ID T_TAPE_MOTION T_TAPE_CONTROL

%token <ival>   T_TAI_UTC T_A1_TAI T_EOP_REF_EPOCH T_NUM_EOP_POINTS
%token <ival>   T_EOP_INTERVAL T_UT1_UTC T_X_WOBBLE T_Y_WOBBLE
%token <ival>   T_NUT_REF_EPOCH T_NUM_NUT_POINTS T_NUT_INTERVAL T_DELTA_PSI
%token <ival>   T_DELTA_EPS T_NUT_MODEL

%token <ival>   T_EXPER_NUM T_EXPER_NAME T_EXPER_NOMINAL_START 
%token <ival>   T_EXPER_NOMINAL_STOP T_PI_NAME T_PI_EMAIL T_CONTACT_NAME 
%token <ival>   T_CONTACT_EMAIL T_SCHEDULER_NAME T_SCHEDULER_EMAIL 
%token <ival>   T_TARGET_CORRELATOR T_EXPER_DESCRIPTION

%token <ival>   T_HEADSTACK_POS

%token <ival>   T_IF_DEF

%token <ival>   T_PASS_ORDER
%token <ival>   T_S2_GROUP_ORDER

%token <ival>   T_PHASE_CAL_DETECT

%token <ival>   T_TAPE_CHANGE T_NEW_SOURCE_COMMAND T_NEW_TAPE_SETUP 
%token <ival>   T_SETUP_ALWAYS T_PARITY_CHECK T_TAPE_PREPASS T_PREOB_CAL
%token <ival>	T_MIDOB_CAL T_POSTOB_CAL T_HEADSTACK_MOTION
%token <ival>   T_PROCEDURE_NAME_PREFIX

%token <ival>	T_ROLL_REINIT_PERIOD T_ROLL_INC_PERIOD T_ROLL T_ROLL_DEF

%token <ival>   T_SEFD_MODEL T_SEFD

%token <ival>   T_SITE_TYPE T_SITE_NAME T_SITE_ID T_SITE_POSITION
%token <ival>	T_SITE_POSITION_EPOCH T_SITE_POSITION_REF T_SITE_VELOCITY
%token <ival>   T_HORIZON_MAP_AZ T_HORIZON_MAP_EL T_ZEN_ATMOS
%token <ival>   T_OCEAN_LOAD_VERT T_OCEAN_LOAD_HORIZ T_OCCUPATION_CODE
%token <ival>   T_INCLINATION T_ECCENTRICITY T_ARG_PERIGEE T_ASCENDING_NODE
%token <ival>   T_MEAN_ANOMALY T_SEMI_MAJOR_AXIS T_MEAN_MOTION T_ORBIT_EPOCH

%token <ival>   T_SOURCE_TYPE T_SOURCE_NAME T_IAU_NAME T_RA T_DEC
%token <ival>   T_SOURCE_POSITION_REF T_RA_RATE T_DEC_RATE
%token <ival>   T_SOURCE_POSITION_EPOCH T_REF_COORD_FRAME
%token <ival>   T_VELOCITY_WRT_LSR T_SOURCE_MODEL

%token <ival>	T_VSN

%token <ival>   T_FANIN_DEF T_FANOUT_DEF T_TRACK_FRAME_FORMAT T_DATA_MODULATION
%token <ival>   T_VLBA_FRMTR_SYS_TRK T_VLBA_TRNSPRT_SYS_TRK
%token <ival>   T_S2_RECORDING_MODE T_S2_DATA_SOURCE

%token <ival>	B_GLOBAL B_STATION B_MODE B_SCHED
%token <ival>	B_EXPER B_SCHEDULING_PARAMS B_PROCEDURES B_EOP B_FREQ B_CLOCK
%token <ival>	B_ANTENNA B_BBC B_CORR B_DAS B_HEAD_POS B_PASS_ORDER
%token <ival>	B_PHASE_CAL_DETECT B_ROLL B_IF B_SEFD B_SITE B_SOURCE B_TRACKS
%token <ival>   B_TAPELOG_OBS

%token <llptr>	T_LITERAL

%token <sval>	T_NAME T_LINK T_ANGLE
%token <sval>   T_COMMENT T_COMMENT_TRAILING

%type  <dvptr>  version
%type  <llptr>  version_lowls
%type  <lwptr>  version_lowl

%type  <blptr>	block
%type  <llptr>  blocks

%type  <llptr>	global_block

%type  <llptr>	station_block station_defs
%type  <dfptr>	station_def
%type  <lwptr>  station_defx

%type  <llptr>	mode_block mode_defs
%type  <dfptr>	mode_def
%type  <lwptr>  mode_defx

%type  <llptr>  qrefs refs qualifiers
%type  <ival>	primitive
%type  <qrptr>	qref ref
%type  <lwptr>  refx qrefx

%type  <llptr>  sched_block sched_defs sched_lowls
%type  <dfptr>	sched_def
%type  <lwptr>  sched_lowl sched_defx
%type  <sval>	start source mode
%type  <dvptr>	start_position
%type  <snptr>	station
%type  <llptr>  drives
%type  <sval>	pass sector
%type  <dtptr>	data_transfer
%type  <sval>	scan_id method destination options
%type  <dvptr>  unit_value2

%type  <llptr>  antenna_block antenna_defs antenna_lowls 
%type  <dfptr>	antenna_def
%type  <lwptr>  antenna_lowl antenna_defx
%type  <dvptr>  antenna_diam axis_offset
%type  <atptr>  axis_type
%type  <amptr>  antenna_motion
%type  <psptr>  pointing_sector

%type  <llptr>  bbc_block bbc_defs bbc_lowls 
%type  <dfptr>	bbc_def
%type  <lwptr>  bbc_lowl bbc_defx
%type  <baptr>  bbc_assign

%type  <llptr>  clock_block clock_defs clock_lowls 
%type  <dfptr>	clock_def
%type  <lwptr>  clock_lowl clock_defx
%type  <ceptr>  clock_early

%type  <llptr>  das_block das_defs das_lowls 
%type  <dfptr>	das_def
%type  <lwptr>  das_lowl das_defx
%type  <sval>	record_transport_type electronics_rack_type tape_control
%type  <dvptr>  number_drives
%type  <dvptr>  record_density recording_system_id
%type  <hsptr>  headstack
%type  <tlptr>  tape_length
%type  <tmptr>  tape_motion

%type  <llptr>  eop_block eop_defs eop_lowls 
%type  <dfptr>  eop_def
%type  <lwptr>  eop_lowl eop_defx
%type  <dvptr>  tai_utc a1_tai num_eop_points eop_interval
%type  <sval>   eop_ref_epoch
%type  <llptr>  ut1_utc x_wobble y_wobble
%type  <sval>   nut_ref_epoch
%type  <dvptr>  num_nut_points nut_interval
%type  <llptr>  delta_psi delta_eps
%type  <sval>   nut_model

%type  <llptr>  exper_block exper_defs exper_lowls 
%type  <dfptr>  exper_def
%type  <lwptr>  exper_lowl exper_defx
%type  <dvptr>  exper_num
%type  <sval>   exper_name exper_nominal_start exper_description
%type  <sval>   exper_nominal_stop pi_name pi_email contact_name 
%type  <sval>   contact_email scheduler_name scheduler_email 
%type  <sval>   target_correlator

%type  <llptr>  freq_block freq_defs freq_lowls
%type  <dfptr>	freq_def
%type  <lwptr>	freq_lowl freq_defx
%type  <cdptr>	chan_def
%type  <dvptr>	sample_rate bits_per_sample switch_state
%type  <llptr>  switch_states
%type  <scptr>  switching_cycle

%type  <llptr>  head_pos_block head_pos_defs head_pos_lowls 
%type  <dfptr>  head_pos_def
%type  <lwptr>  head_pos_lowl head_pos_defx
%type  <hpptr>  headstack_pos

%type  <llptr>  if_block if_defs if_lowls 
%type  <dfptr>  if_def
%type  <lwptr>  if_lowl if_defx
%type  <ifptr>  if_def_st

%type  <llptr>  pass_order_block pass_order_defs pass_order_lowls 
%type  <dfptr>  pass_order_def
%type  <lwptr>  pass_order_lowl pass_order_defx
%type  <llptr>  pass_order s2_group_order

%type  <llptr>  phase_cal_detect_block phase_cal_detect_defs
%type  <llptr>  phase_cal_detect_lowls 
%type  <dfptr>  phase_cal_detect_def
%type  <lwptr>  phase_cal_detect_lowl phase_cal_detect_defx
%type  <pdptr>  phase_cal_detect

%type  <llptr>  procedures_block procedures_defs procedures_lowls 
%type  <dfptr>  procedures_def
%type  <lwptr>  procedures_lowl procedures_defx
%type  <dvptr>  tape_change headstack_motion new_source_command new_tape_setup
%type  <saptr>  setup_always
%type  <pcptr>  parity_check
%type  <tpptr>  tape_prepass
%type  <prptr>  preob_cal
%type  <miptr>  midob_cal
%type  <poptr>  postob_cal
%type  <sval>   procedure_name_prefix

%type  <llptr>  roll_block roll_defs roll_lowls 
%type  <dfptr>  roll_def
%type  <lwptr>  roll_lowl roll_defx
%type  <dvptr>  roll_reinit_period roll_inc_period
%type  <sval>	roll
%type  <llptr>  roll_def_st

%type  <llptr>  scheduling_params_block scheduling_params_defs
%type  <llptr>  scheduling_params_lowls 
%type  <dfptr>  scheduling_params_def
%type  <lwptr>  scheduling_params_lowl scheduling_params_defx

%type  <llptr>  sefd_block sefd_defs sefd_lowls 
%type  <dfptr>  sefd_def
%type  <lwptr>  sefd_lowl sefd_defx
%type  <sval>   sefd_model
%type  <septr>  sefd

%type  <llptr>  site_block site_defs site_lowls 
%type  <dfptr>  site_def
%type  <lwptr>  site_lowl site_defx
%type  <sval>   site_type site_name site_id occupation_code orbit_epoch
%type  <sval>	site_position_epoch site_position_ref
%type  <spptr>  site_position
%type  <svptr>  site_velocity
%type  <llptr>  horizon_map_az horizon_map_el
%type  <ovptr>  ocean_load_vert
%type  <ohptr>  ocean_load_horiz
%type  <dvptr>  zen_atmos inclination eccentricity arg_perigee ascending_node
%type  <dvptr>  mean_anomaly semi_major_axis mean_motion

%type  <llptr>  source_block source_defs source_lowls 
%type  <dfptr>  source_def
%type  <lwptr>  source_lowl source_defx
%type  <llptr>  source_type
%type  <sval>   source_name iau_name ra dec source_position_ref ref_coord_frame
%type  <sval>	source_position_epoch
%type  <dvptr>  ra_rate dec_rate velocity_wrt_lsr
%type  <smptr>  source_model

%type  <llptr>  tapelog_obs_block tapelog_obs_defs tapelog_obs_lowls 
%type  <dfptr>  tapelog_obs_def
%type  <lwptr>  tapelog_obs_lowl tapelog_obs_defx
%type  <vsptr>  vsn

%type  <llptr>  tracks_block tracks_defs tracks_lowls 
%type  <dfptr>  tracks_def
%type  <lwptr>  tracks_lowl tracks_defx
%type  <fiptr>  fanin_def
%type  <foptr>  fanout_def
%type  <sval>   track_frame_format data_modulation s2_recording_mode
%type  <fsptr>  vlba_frmtr_sys_trk
%type  <llptr>  bit_stream_list vlba_trnsprt_sys_trk
%type  <dsptr>  s2_data_source

%type  <exptr>  external_ref
%type  <llptr>  literal
%type  <llptr>  unit_list name_list value_list
%type  <llptr>  unit_more
%type  <dvptr>  unit_value value
%type  <dvptr>  unit_option
%type  <sval>   name_value

%%

/* start rule */

vex:	version_lowls blocks		{vex_ptr=make_vex($1,$2);}
	| version_lowls			{vex_ptr=make_vex($1,NULL);}
;
version_lowls:	version_lowls version_lowl	{$$=add_list($1,$2);}
		| version_lowl			{$$=add_list(NULL,$1);}
;
version_lowl:	version			{$$=make_lowl(T_VEX_REV,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
/* version number */

version:	T_VEX_REV '=' value ';'	{$$=$3;}
;

/* blocks */

blocks:	blocks block			{$$=add_list($1,$2);}
	| block				{$$=add_list(NULL,$1);}
;
block:	global_block			{$$=make_block(B_GLOBAL,$1);}
	| station_block			{$$=make_block(B_STATION,$1);}
	| mode_block			{$$=make_block(B_MODE,$1);}
	| freq_block			{$$=make_block(B_FREQ,$1);}
	| sched_block			{$$=make_block(B_SCHED,$1);}
 	| antenna_block			{$$=make_block(B_ANTENNA,$1);}
 	| bbc_block			{$$=make_block(B_BBC,$1);}
 	| clock_block			{$$=make_block(B_CLOCK,$1);}
 	| das_block			{$$=make_block(B_DAS,$1);}
 	| eop_block			{$$=make_block(B_EOP,$1);}
 	| exper_block			{$$=make_block(B_EXPER,$1);}
 	| head_pos_block		{$$=make_block(B_HEAD_POS,$1);}
 	| if_block			{$$=make_block(B_IF,$1);}
 	| pass_order_block		{$$=make_block(B_PASS_ORDER,$1);}
 	| phase_cal_detect_block	{$$=make_block(B_PHASE_CAL_DETECT,$1);}
 	| procedures_block		{$$=make_block(B_PROCEDURES,$1);}
 	| roll_block			{$$=make_block(B_ROLL,$1);}
 	| scheduling_params_block
				{$$=make_block(B_SCHEDULING_PARAMS,$1);}
 	| sefd_block			{$$=make_block(B_SEFD,$1);}
 	| site_block			{$$=make_block(B_SITE,$1);}
 	| source_block			{$$=make_block(B_SOURCE,$1);}
 	| tapelog_obs_block		{$$=make_block(B_TAPELOG_OBS,$1);}
 	| tracks_block			{$$=make_block(B_TRACKS,$1);}
;
/* $GLOBAL block */

global_block:	B_GLOBAL ';' refs	{$$=$3;}
		| B_GLOBAL ';'		{$$=NULL;}
;
/* $STATION block */

station_block:	B_STATION ';' station_defs	{$$=$3;}
		| B_STATION ';'			{$$=NULL;}
;
station_defs:	station_defs station_defx	{$$=add_list($1,$2);}
		| station_defx			{$$=add_list(NULL,$1);}
;
station_defx:	station_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
station_def:	T_DEF T_NAME ';' refs T_ENDDEF ';'	{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'		{$$=make_def($2,NULL);}
;
/* $MODE block */

mode_block:	B_MODE ';' mode_defs	{$$=$3;}
		| B_MODE ';' 		{$$=NULL;}
;
mode_defs:	mode_defs mode_defx	{$$=add_list($1,$2);}
		| mode_defx		{$$=add_list(NULL,$1);}
;
mode_defx:	mode_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
mode_def:	T_DEF T_NAME ';' qrefs T_ENDDEF ';'	{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;		
/* refs utility rules */

refs:	refs refx			{$$=add_list($1,$2);}
	| refx				{$$=add_list(NULL,$1);}
;
refx:	ref			{$$=make_lowl(T_REF,$1);}
	| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
	| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
ref:	T_REF primitive '=' T_NAME ';'	{$$=make_qref($2,$4,NULL);}
;
primitive:	B_EXPER			{$$=B_EXPER;}
		| B_SCHEDULING_PARAMS	{$$=B_SCHEDULING_PARAMS;}
		| B_PROCEDURES		{$$=B_PROCEDURES;}
		| B_EOP			{$$=B_EOP;}
		| B_FREQ		{$$=B_FREQ;}
		| B_ANTENNA		{$$=B_ANTENNA;}
		| B_BBC			{$$=B_BBC;}
		| B_CLOCK		{$$=B_CLOCK;}
		| B_CORR		{$$=B_CORR;}
		| B_DAS			{$$=B_DAS;}
		| B_HEAD_POS		{$$=B_HEAD_POS;}
		| B_PASS_ORDER		{$$=B_PASS_ORDER;}
		| B_PHASE_CAL_DETECT	{$$=B_PHASE_CAL_DETECT;}
		| B_ROLL		{$$=B_ROLL;}
		| B_IF			{$$=B_IF;}
		| B_SEFD		{$$=B_SEFD;}
		| B_SITE		{$$=B_SITE;}
		| B_SOURCE		{$$=B_SOURCE;}
		| B_TRACKS		{$$=B_TRACKS;}
		| B_TAPELOG_OBS		{$$=B_TAPELOG_OBS;}
;
qrefs:	qrefs qrefx			{$$=add_list($1,$2);}
	| qrefx				{$$=add_list(NULL,$1);}
;
qrefx:	qref			{$$=make_lowl(T_REF,$1);}
	| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
	| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
qref:	T_REF primitive '=' T_NAME qualifiers ';' {$$=make_qref($2,$4,$5);}
	| T_REF primitive '=' T_NAME ';' 	{$$=make_qref($2,$4,NULL);}
;
qualifiers:	qualifiers ':' T_NAME	{$$=add_list($1,$3);}
		| ':' T_NAME		{$$=add_list(NULL,$2);}
;
/* $SCHED block */

sched_block:	B_SCHED ';' sched_defs	{$$=$3;}
		| B_SCHED ';'		{$$=NULL;}
;
sched_defs:	sched_defs sched_defx	{$$=add_list($1,$2);}
		| sched_defx		{$$=add_list(NULL,$1);}
;
sched_defx:	sched_def		{$$=make_lowl(T_SCAN,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
sched_def:	T_SCAN T_NAME ';' sched_lowls T_ENDSCAN ';'
						{$$=make_def($2,$4);}
		| T_SCAN T_NAME ';' T_ENDSCAN ';'	{$$=make_def($2,NULL);}
;
sched_lowls:	sched_lowls sched_lowl	{$$=add_list($1,$2);}
		| sched_lowl		{$$=add_list(NULL,$1);}
;
sched_lowl:	start			{$$=make_lowl(T_START,$1);}
		| mode			{$$=make_lowl(T_MODE,$1);}
		| source		{$$=make_lowl(T_SOURCE,$1);}
		| station		{$$=make_lowl(T_STATION,$1);}
		| data_transfer		{$$=make_lowl(T_DATA_TRANSFER,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
start:		T_START '=' T_NAME ';'	{$$=$3;}
;
mode:		T_MODE '=' T_NAME ';'	{$$=$3;}
;
source:		T_SOURCE '=' T_NAME ';'	{$$=$3;}
;
station:	T_STATION '=' T_NAME ':'	/* name */
		unit_value ':'			/* data start */
		unit_value ':'			/* data stop */
		start_position ':'
		pass ':'		
		sector ':'				/* pointing sector */
		drives ';'
		{$$=make_station($3,$5,$7,$9,$11,$13,$15);}
;
data_transfer:	T_DATA_TRANSFER '=' scan_id ':' /* name */
                method ':'                     /* method disk2file or in2net */
                destination ':'		       /* filename or blank */
		unit_value2 ':'		       /* data start */
		unit_value2 ':'		       /* data stop */
		options ';'                    /* future use, empty now */
		{$$=make_data_transfer($3,$5,$7,$9,$11,$13);}
                | T_DATA_TRANSFER '=' scan_id ':' /* name */
                method ':'                     /* method disk2file or in2net */
                destination ':'                /* filename or blank */
                unit_value2 ':'                /* data start */
                unit_value2 ';'                /* data stop */
                {$$=make_data_transfer($3,$5,$7,$9,$11,NULL);}
;
start_position:	/* empty */			{$$=NULL;}
		| unit_value			{$$=$1;}
;
pass:		/* empty */			{$$=NULL;}
		| T_NAME			{$$=$1;}
;
sector:		/* empty */			{$$=NULL;}
		| T_LINK			{$$=$1;}
;
drives:		/* empty */			{$$=NULL;}
		| value				{$$=add_list(NULL,$1);}
		| value ':' value	{$$=add_list(add_list(NULL,$1),$3);}
;
scan_id:	/* empty */			{$$=NULL;}
		| T_NAME			{$$=$1;}
;
method:		/* empty */			{$$=NULL;}
		| T_NAME			{$$=$1;}
;
destination:	/* empty */			{$$=NULL;}
		| T_NAME			{$$=$1;}
;
unit_value2:	/* empty */ 			{$$=make_dvalue(NULL,NULL);}
		| T_NAME T_NAME 		{$$=make_dvalue($1,$2);}
;
options:	/* empty */			{$$=NULL;}
		| T_NAME			{$$=$1;}
;

/* $ANTENNA block */

antenna_block:	B_ANTENNA ';' antenna_defs	{$$=$3;}
		| B_ANTENNA ';'			{$$=NULL;}
;
antenna_defs:	antenna_defs antenna_defx	{$$=add_list($1,$2);}
		| antenna_defx			{$$=add_list(NULL,$1);}
;
antenna_defx:	antenna_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
antenna_def:	T_DEF T_NAME ';' antenna_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'	{$$=make_def($2,NULL);}
;
antenna_lowls:	antenna_lowls antenna_lowl	{$$=add_list($1,$2);}
		| antenna_lowl		{$$=add_list(NULL,$1);}
;
antenna_lowl:	antenna_diam		{$$=make_lowl(T_ANTENNA_DIAM,$1);}
		| axis_type		{$$=make_lowl(T_AXIS_TYPE,$1);}
		| axis_offset		{$$=make_lowl(T_AXIS_OFFSET,$1);}
		| antenna_motion	{$$=make_lowl(T_ANTENNA_MOTION,$1);}
		| pointing_sector	{$$=make_lowl(T_POINTING_SECTOR,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
antenna_diam:	T_ANTENNA_DIAM '=' unit_value ';'		{$$=$3;}
;
axis_type:	T_AXIS_TYPE '=' T_NAME ':' T_NAME ';'
		{$$=make_axis_type($3,$5);}
;
axis_offset:	T_AXIS_OFFSET '=' unit_value ';'	{$$=$3;}
;
antenna_motion:	T_ANTENNA_MOTION '=' T_NAME ':'
		unit_value ':'
		unit_value ';'
		{$$=make_antenna_motion($3,$5,$7);}
;
pointing_sector:	T_POINTING_SECTOR '=' T_LINK ':'
			T_NAME ':'
			unit_value ':'
			unit_value ':'
			T_NAME ':'
			unit_value ':'
			unit_value ';'
			{$$=make_pointing_sector($3,$5,$7,$9,$11,$13,$15);}
;
/* $BBC block */

bbc_block:	B_BBC ';' bbc_defs	{$$=$3;}
		| B_BBC ';'		{$$=NULL;}
;
bbc_defs:	bbc_defs bbc_defx	{$$=add_list($1,$2);}
		| bbc_defx		{$$=add_list(NULL,$1);}
;
bbc_defx:	bbc_def			{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
bbc_def:	T_DEF T_NAME ';' bbc_lowls T_ENDDEF ';' {$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
bbc_lowls:	bbc_lowls bbc_lowl	{$$=add_list($1,$2);}
		| bbc_lowl		{$$=add_list(NULL,$1);}
;
bbc_lowl:	bbc_assign		{$$=make_lowl(T_BBC_ASSIGN,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
bbc_assign:	T_BBC_ASSIGN '=' T_LINK ':' value ':' T_LINK ';'
		{$$=make_bbc_assign($3,$5,$7);}
;
/* $CLOCK block */

clock_block:	B_CLOCK ';' clock_defs	{$$=$3;}
		| B_CLOCK ';'		{$$=NULL;}
;
clock_defs:	clock_defs clock_defx	{$$=add_list($1,$2);}
		| clock_defx		{$$=add_list(NULL,$1);}
;
clock_defx:	clock_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
clock_def:	T_DEF T_NAME ';' clock_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
clock_lowls:	clock_lowls clock_lowl	{$$=add_list($1,$2);}
		| clock_lowl		{$$=add_list(NULL,$1);}
;
clock_lowl:	clock_early		{$$=make_lowl(T_CLOCK_EARLY,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
clock_early:	T_CLOCK_EARLY '=' ':' unit_value ';'
				{$$=make_clock_early(NULL,$4,NULL,NULL);}
		| T_CLOCK_EARLY '=' T_NAME ':' unit_value ';'
				{$$=make_clock_early($3,$5,NULL,NULL);}
	| T_CLOCK_EARLY '=' T_NAME ':' unit_value ':' T_NAME ':' value ';'
				{$$=make_clock_early($3,$5,$7,$9);}
	| T_CLOCK_EARLY '=' ':' unit_value ':' T_NAME ':' value ';'
				{$$=make_clock_early(NULL,$4,$6,$8);}
;
/* $DAS block */

das_block:	B_DAS ';' das_defs	{$$=$3;}
		| B_DAS ';'		{$$=NULL;}
;
das_defs:	das_defs das_defx	{$$=add_list($1,$2);}
		| das_defx		{$$=add_list(NULL,$1);}
;
das_defx:	das_def			{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
das_def:	T_DEF T_NAME ';' das_lowls T_ENDDEF ';' {$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
das_lowls:	das_lowls das_lowl	{$$=add_list($1,$2);}
		| das_lowl		{$$=add_list(NULL,$1);}
;
das_lowl:	record_transport_type {$$=make_lowl(T_RECORD_TRANSPORT_TYPE,$1);}
 		| electronics_rack_type {$$=make_lowl(T_ELECTRONICS_RACK_TYPE,$1);}
		| number_drives		{$$=make_lowl(T_NUMBER_DRIVES,$1);}
		| headstack		{$$=make_lowl(T_HEADSTACK,$1);}
		| record_density	{$$=make_lowl(T_RECORD_DENSITY,$1);}
		| tape_length		{$$=make_lowl(T_TAPE_LENGTH,$1);}
		| recording_system_id
				{$$=make_lowl(T_RECORDING_SYSTEM_ID,$1);}
		| tape_motion		{$$=make_lowl(T_TAPE_MOTION,$1);}
		| tape_control		{$$=make_lowl(T_TAPE_CONTROL,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
record_transport_type:	T_RECORD_TRANSPORT_TYPE '=' T_NAME ';' 	{$$=$3;}
;
electronics_rack_type:	T_ELECTRONICS_RACK_TYPE '=' T_NAME ';' 	{$$=$3;}
;
number_drives:		T_NUMBER_DRIVES '=' value ';'		{$$=$3;}
;
headstack:	T_HEADSTACK '=' value ':' T_NAME ':' value ';'
					{$$=make_headstack($3,$5,$7);}
        	| T_HEADSTACK '=' value ':' ':' value ';'
					{$$=make_headstack($3,NULL,$6);}
;
record_density:	T_RECORD_DENSITY '=' T_NAME T_NAME ';'
						{$$=make_dvalue($3,$4);}
;
tape_length:	T_TAPE_LENGTH '=' unit_value ';'
				{$$=make_tape_length($3,NULL,NULL);}
		| T_TAPE_LENGTH '=' unit_value ':' T_NAME ':' value ';'
				{$$=make_tape_length($3,$5,$7);}
;
recording_system_id:	T_RECORDING_SYSTEM_ID '=' value ';' 	{$$=$3;}
;
tape_motion:	T_TAPE_MOTION '=' T_NAME ';'
				{$$=make_tape_motion($3,NULL,NULL,NULL);}
		| T_TAPE_MOTION '=' T_NAME ':' unit_value ';'
				{$$=make_tape_motion($3,$5,NULL,NULL);}
		| T_TAPE_MOTION '=' T_NAME ':' unit_value
					':' unit_value ':' unit_value ';'
				{$$=make_tape_motion($3,$5,$7,$9);}
;
tape_control:	T_TAPE_CONTROL '=' T_NAME ';' {$$=$3;}
;
/* $EOP block */

eop_block:	B_EOP ';' eop_defs	{$$=$3;}
		| B_EOP ';'		{$$=NULL;}
;
eop_defs:	eop_defs eop_defx	{$$=add_list($1,$2);}
		| eop_defx		{$$=add_list(NULL,$1);}
;
eop_defx:	eop_def			{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
eop_def:	T_DEF T_NAME ';' eop_lowls T_ENDDEF ';' {$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
eop_lowls:	eop_lowls eop_lowl	{$$=add_list($1,$2);}
		| eop_lowl		{$$=add_list(NULL,$1);}
;
eop_lowl:	tai_utc			{$$=make_lowl(T_TAI_UTC,$1);}
 		| a1_tai		{$$=make_lowl(T_A1_TAI,$1);}
		| eop_ref_epoch		{$$=make_lowl(T_EOP_REF_EPOCH,$1);}
		| num_eop_points	{$$=make_lowl(T_NUM_EOP_POINTS,$1);}
		| eop_interval		{$$=make_lowl(T_EOP_INTERVAL,$1);}
		| ut1_utc		{$$=make_lowl(T_UT1_UTC,$1);}
		| x_wobble		{$$=make_lowl(T_X_WOBBLE,$1);}
		| y_wobble		{$$=make_lowl(T_Y_WOBBLE,$1);}
		| nut_ref_epoch		{$$=make_lowl(T_NUT_REF_EPOCH,$1);}
		| num_nut_points	{$$=make_lowl(T_NUM_NUT_POINTS,$1);}
		| nut_interval  	{$$=make_lowl(T_NUT_INTERVAL,$1);}
		| delta_psi          	{$$=make_lowl(T_DELTA_PSI,$1);}
		| delta_eps          	{$$=make_lowl(T_DELTA_EPS,$1);}
		| nut_model     	{$$=make_lowl(T_NUT_MODEL,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
tai_utc:	T_TAI_UTC '=' unit_value ';'	{$$=$3;}
;
a1_tai:		T_A1_TAI '=' unit_value ';'	{$$=$3;}
;
eop_ref_epoch:	T_EOP_REF_EPOCH '=' T_NAME ';'	{$$=$3;}
;
num_eop_points:	T_NUM_EOP_POINTS '=' value ';'	{$$=$3;}
;
eop_interval:	T_EOP_INTERVAL '=' unit_value ';'	{$$=$3;}
;
ut1_utc:	T_UT1_UTC '=' unit_list ';'	{$$=$3;}
		| T_UT1_UTC '=' ';'		{$$=NULL;}
;
x_wobble:	T_X_WOBBLE '=' unit_list ';'	{$$=$3;}
		| T_X_WOBBLE '=' ';'		{$$=NULL;}
;
y_wobble:	T_Y_WOBBLE '=' unit_list ';'	{$$=$3;}
		| T_Y_WOBBLE '=' ';'		{$$=NULL;}
;
nut_ref_epoch:	T_NUT_REF_EPOCH '=' T_NAME ';'	{$$=$3;}
;
num_nut_points:	T_NUM_NUT_POINTS '=' value ';'	{$$=$3;}
;
nut_interval:	T_NUT_INTERVAL '=' unit_value ';'	{$$=$3;}
;
delta_psi:      T_DELTA_PSI '=' unit_list ';'	{$$=$3;}
		| T_DELTA_PSI '=' ';'		{$$=NULL;}
;
delta_eps:      T_DELTA_EPS '=' unit_list ';'	{$$=$3;}
		| T_DELTA_EPS '=' ';'		{$$=NULL;}
;
nut_model:	T_NUT_MODEL '=' T_NAME ';'	{$$=$3;}
;
/* $EXPER block */

exper_block:	B_EXPER ';' exper_defs	{$$=$3;}
		| B_EXPER ';'		{$$=NULL;}
;
exper_defs:	exper_defs exper_defx	{$$=add_list($1,$2);}
		| exper_defx		{$$=add_list(NULL,$1);}
;
exper_defx:	exper_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
exper_def:	T_DEF T_NAME ';' exper_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'	{$$=make_def($2,NULL);}
;
exper_lowls:	exper_lowls exper_lowl	{$$=add_list($1,$2);}
		| exper_lowl		{$$=add_list(NULL,$1);}
;
exper_lowl:	exper_num		{$$=make_lowl(T_EXPER_NUM,$1);}
   		| exper_name		{$$=make_lowl(T_EXPER_NAME,$1);}
   		| exper_description	{$$=make_lowl(T_EXPER_DESCRIPTION,$1);}
   		| exper_nominal_start
				{$$=make_lowl(T_EXPER_NOMINAL_START,$1);}
   		| exper_nominal_stop
				{$$=make_lowl(T_EXPER_NOMINAL_STOP,$1);}
   		| pi_name		{$$=make_lowl(T_PI_NAME,$1);}
   		| pi_email		{$$=make_lowl(T_PI_EMAIL,$1);}
   		| contact_name		{$$=make_lowl(T_CONTACT_NAME,$1);}
   		| contact_email		{$$=make_lowl(T_CONTACT_EMAIL,$1);}
   		| scheduler_name	{$$=make_lowl(T_SCHEDULER_NAME,$1);}
   		| scheduler_email	{$$=make_lowl(T_SCHEDULER_EMAIL,$1);}
   		| target_correlator
				{$$=make_lowl(T_TARGET_CORRELATOR,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
exper_num:	T_EXPER_NUM '=' value ';' {$$=$3;}
;
exper_name:	T_EXPER_NAME '=' T_NAME ';'	{$$=$3;}
;
exper_description:	T_EXPER_DESCRIPTION '=' T_NAME ';'	{$$=$3;}
;
exper_nominal_start:	T_EXPER_NOMINAL_START '=' T_NAME ';'	{$$=$3;}
;
exper_nominal_stop:	T_EXPER_NOMINAL_STOP '=' T_NAME ';'	{$$=$3;}
;
pi_name:	T_PI_NAME '=' T_NAME ';'	{$$=$3;}
;
pi_email:	T_PI_EMAIL '=' T_NAME ';'	{$$=$3;}
;
contact_name:	T_CONTACT_NAME '=' T_NAME ';'	{$$=$3;}
;
contact_email:	T_CONTACT_EMAIL '=' T_NAME ';'	{$$=$3;}
;
scheduler_name:	T_SCHEDULER_NAME '=' T_NAME ';'	{$$=$3;}
;
scheduler_email:	T_SCHEDULER_EMAIL '=' T_NAME ';'	{$$=$3;}
;
target_correlator:	T_TARGET_CORRELATOR '=' T_NAME ';'	{$$=$3;}
;
/* $FREQ block */

freq_block:	B_FREQ ';' freq_defs	{$$=$3;}
		| B_FREQ ';'		{$$=NULL;}
;
freq_defs:	freq_defs freq_defx	{$$=add_list($1,$2);}
		| freq_defx		{$$=add_list(NULL,$1);}
;
freq_defx:	freq_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
freq_def:	T_DEF T_NAME ';' freq_lowls T_ENDDEF ';' {$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
freq_lowls:	freq_lowls freq_lowl	{$$=add_list($1,$2);}
		| freq_lowl		{$$=add_list(NULL,$1);}
;
freq_lowl:	chan_def		{$$=make_lowl(T_CHAN_DEF,$1);}
		| sample_rate		{$$=make_lowl(T_SAMPLE_RATE,$1);}
		| bits_per_sample	{$$=make_lowl(T_BITS_PER_SAMPLE,$1);}
		| switching_cycle	{$$=make_lowl(T_SWITCHING_CYCLE,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
chan_def:	T_CHAN_DEF '=' T_LINK		/* band_id */
		':' unit_value			/* sky frequency */
		':' T_NAME			/* net sb */
		':' unit_value			/* channel BW */
		':' T_LINK			/* chan ID */
		':' T_LINK			/* BBC ID */
		':' T_LINK ';'	/* phase-cal ID */
		{$$=make_chan_def($3,$5,$7,$9,$11,$13,$15,NULL);}
		| T_CHAN_DEF '=' T_LINK		/* band_id */
		':' unit_value			/* sky frequency */
		':' T_NAME			/* net sb */
		':' unit_value			/* channel BW */
		':' T_LINK			/* chan ID */
		':' T_LINK			/* BBC ID */
		':' T_LINK switch_states ';'	/* phase-cal ID */
		{$$=make_chan_def($3,$5,$7,$9,$11,$13,$15,$16);}
		| T_CHAN_DEF '='		/* band_id */
		':' unit_value			/* sky frequency */
		':' T_NAME			/* net sb */
		':' unit_value			/* channel BW */
		':' T_LINK			/* chan ID */
		':' T_LINK			/* BBC ID */
		':' T_LINK ';'	/* phase-cal ID */
		{$$=make_chan_def(NULL,$4,$6,$8,$10,$12,$14,NULL);}
		| T_CHAN_DEF '='		/* band_id */
		':' unit_value			/* sky frequency */
		':' T_NAME			/* net sb */
		':' unit_value			/* channel BW */
		':' T_LINK			/* chan ID */
		':' T_LINK			/* BBC ID */
		':' T_LINK switch_states ';'	/* phase-cal ID */
		{$$=make_chan_def(NULL,$4,$6,$8,$10,$12,$14,$15);}
;
switch_states:	switch_states switch_state	{$$=add_list($1,$2);}
		| switch_state			{$$=add_list(NULL,$1);}
;
switch_state:	':' value			{$$=$2;}
;
sample_rate:	T_SAMPLE_RATE '=' unit_value ';'		{$$=$3;}
;
bits_per_sample:	T_BITS_PER_SAMPLE '=' value ';'		{$$=$3;}
;
switching_cycle:	T_SWITCHING_CYCLE '=' T_NAME ':' unit_list ';' 
				{$$=make_switching_cycle($3,$5);}
;
/* $HEAD_POS block */
	
head_pos_block:	B_HEAD_POS ';' head_pos_defs	{$$=$3;}
		| B_HEAD_POS ';'		{$$=NULL;}
;
head_pos_defs:	head_pos_defs head_pos_defx	{$$=add_list($1,$2);}
		| head_pos_defx			{$$=add_list(NULL,$1);}
;
head_pos_defx:	head_pos_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
head_pos_def:	T_DEF T_NAME ';' head_pos_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
head_pos_lowls:	head_pos_lowls head_pos_lowl	{$$=add_list($1,$2);}
		| head_pos_lowl			{$$=add_list(NULL,$1);}
;
head_pos_lowl:	headstack_pos		{$$=make_lowl(T_HEADSTACK_POS,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
headstack_pos:	T_HEADSTACK_POS '=' value ':' unit_list ';'
					{$$=make_headstack_pos($3,$5);}
;
/* $IF block */

if_block:	B_IF ';' if_defs	{$$=$3;}
		| B_IF ';'		{$$=NULL;}
;
if_defs:	if_defs if_defx			{$$=add_list($1,$2);}
		| if_defx			{$$=add_list(NULL,$1);}
;
if_defx:	if_def			{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
if_def:		T_DEF T_NAME ';' if_lowls T_ENDDEF ';'	{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
if_lowls:	if_lowls if_lowl		{$$=add_list($1,$2);}
		| if_lowl			{$$=add_list(NULL,$1);}
;
if_lowl:	if_def_st		{$$=make_lowl(T_IF_DEF,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
if_def_st:	T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' unit_value ':' unit_value ';'
			{$$=make_if_def($3,$5,$7,$9,$11,$13,$15,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ';'
			{$$=make_if_def($3,$5,$7,$9,$11,NULL,NULL,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' ':' ';'
			{$$=make_if_def($3,$5,$7,$9,$11,NULL,NULL,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' ';'
			{$$=make_if_def($3,$5,$7,$9,$11,NULL,NULL,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' unit_value ';'
			{$$=make_if_def($3,$5,$7,$9,$11,$13,NULL,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' unit_value ':' ';'
			{$$=make_if_def($3,$5,$7,$9,$11,$13,NULL,NULL);}
		| T_IF_DEF '=' T_LINK ':' T_NAME ':' T_NAME ':' unit_value ':' T_NAME ':' unit_value ':' unit_value ';' '*' T_NAME
			{$$=make_if_def($3,$5,$7,$9,$11,$13,$15,$18);}
;
/* $PASS_ORDER block */

pass_order_block:	B_PASS_ORDER ';' pass_order_defs	{$$=$3;}
			| B_PASS_ORDER ';'			{$$=NULL;}
;
pass_order_defs:	pass_order_defs pass_order_defx	{$$=add_list($1,$2);}
			| pass_order_defx
						{$$=add_list(NULL,$1);}
;
pass_order_defx:	pass_order_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
pass_order_def:	T_DEF T_NAME ';' pass_order_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
pass_order_lowls:	pass_order_lowls pass_order_lowl
						{$$=add_list($1,$2);}
			| pass_order_lowl	{$$=add_list(NULL,$1);}
;
pass_order_lowl:	pass_order	{$$=make_lowl(T_PASS_ORDER,$1);}
			| s2_group_order
					{$$=make_lowl(T_S2_GROUP_ORDER,$1);}
			| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
pass_order:	T_PASS_ORDER '=' name_list ';' {$$=$3;}
;
s2_group_order:	T_S2_GROUP_ORDER '=' value_list ';' {$$=$3;}
;
/* $PHASE_CAL_DETECT block */

phase_cal_detect_block:	B_PHASE_CAL_DETECT ';' phase_cal_detect_defs	{$$=$3;}
			| B_PHASE_CAL_DETECT ';'		{$$=NULL;}
;
phase_cal_detect_defs:	phase_cal_detect_defs phase_cal_detect_defx
						{$$=add_list($1,$2);}
			| phase_cal_detect_defx	{$$=add_list(NULL,$1);}
;
phase_cal_detect_defx:	phase_cal_detect_def	{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
phase_cal_detect_def:	T_DEF T_NAME ';' phase_cal_detect_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'	{$$=make_def($2,NULL);}
;
phase_cal_detect_lowls:	phase_cal_detect_lowls phase_cal_detect_lowl
						{$$=add_list($1,$2);}
			| phase_cal_detect_lowl	{$$=add_list(NULL,$1);}
;
phase_cal_detect_lowl:	phase_cal_detect {$$=make_lowl(T_PHASE_CAL_DETECT,$1);}
		| external_ref	{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
phase_cal_detect:	T_PHASE_CAL_DETECT '=' T_LINK ':' value_list ';'
		{$$=make_phase_cal_detect($3,$5);}
			| T_PHASE_CAL_DETECT '=' T_LINK ';'
		{$$=make_phase_cal_detect($3,NULL);}
;
/* $PROCEDURES block */

procedures_block:	B_PROCEDURES ';' procedures_defs	{$$=$3;}
			| B_PROCEDURES ';'			{$$=NULL;}
;
procedures_defs:	procedures_defs procedures_defx
							{$$=add_list($1,$2);}
			| procedures_defx	{$$=add_list(NULL,$1);}
;
procedures_defx:	procedures_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
procedures_def:	T_DEF T_NAME ';' procedures_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
			| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
procedures_lowls:	procedures_lowls procedures_lowl
							{$$=add_list($1,$2);}
			| procedures_lowl	{$$=add_list(NULL,$1);}
;
procedures_lowl:	tape_change
				 {$$=make_lowl(T_TAPE_CHANGE,$1);}
			| headstack_motion
				{$$=make_lowl(T_HEADSTACK_MOTION,$1);}
			| new_source_command
				{$$=make_lowl(T_NEW_SOURCE_COMMAND,$1);}
			| new_tape_setup
				{$$=make_lowl(T_NEW_TAPE_SETUP,$1);}
			| setup_always
				{$$=make_lowl(T_SETUP_ALWAYS,$1);}
			| parity_check
				{$$=make_lowl(T_PARITY_CHECK,$1);}
			| tape_prepass
				{$$=make_lowl(T_TAPE_PREPASS,$1);}
			| preob_cal
				{$$=make_lowl(T_PREOB_CAL,$1);}
			| midob_cal
				{$$=make_lowl(T_MIDOB_CAL,$1);}
			| postob_cal
				{$$=make_lowl(T_POSTOB_CAL,$1);}
	   		| procedure_name_prefix
				 {$$=make_lowl(T_PROCEDURE_NAME_PREFIX,$1);}
			| external_ref	{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
tape_change:	T_TAPE_CHANGE '=' unit_value ';'	{$$=$3;}
;
headstack_motion:	T_HEADSTACK_MOTION '=' unit_value ';'	{$$=$3;}
;
new_source_command:	T_NEW_SOURCE_COMMAND '=' unit_value ';'	{$$=$3;}
;
new_tape_setup:	T_NEW_TAPE_SETUP '=' unit_value ';'	{$$=$3;}
;
setup_always:	T_SETUP_ALWAYS '=' name_value ':' unit_value ';'
		{$$=make_setup_always($3,$5);}
;
parity_check:	T_PARITY_CHECK '=' name_value ':' unit_value ';'
		{$$=make_parity_check($3,$5);}
;
tape_prepass:	T_TAPE_PREPASS '=' name_value ':' unit_value ';'
		{$$=make_tape_prepass($3,$5);}
;
preob_cal:	T_PREOB_CAL '=' name_value ':' unit_value ':' name_value ';'
		{$$=make_preob_cal($3,$5,$7);}
;
midob_cal:	T_MIDOB_CAL '=' name_value ':' unit_value ':' name_value ';'
		{$$=make_midob_cal($3,$5,$7);}
;
postob_cal:	T_POSTOB_CAL '=' name_value ':' unit_value ':' name_value ';'
		{$$=make_postob_cal($3,$5,$7);}
;
procedure_name_prefix:	T_PROCEDURE_NAME_PREFIX '=' T_NAME ';'	{$$=$3;}
;
/* $ROLL block */

roll_block:	B_ROLL ';' roll_defs	{$$=$3;}
			| B_ROLL ';'		{$$=NULL;}
;
roll_defs:	roll_defs roll_defx	{$$=add_list($1,$2);}
		| roll_defx		{$$=add_list(NULL,$1);}
;
roll_defx:	roll_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
roll_def:	T_DEF T_NAME ';' roll_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
roll_lowls:	roll_lowls roll_lowl	{$$=add_list($1,$2);}
			| roll_lowl		{$$=add_list(NULL,$1);}
;
roll_lowl:	roll_reinit_period {$$=make_lowl(T_ROLL_REINIT_PERIOD,$1);}
		| roll_inc_period	{$$=make_lowl(T_ROLL_INC_PERIOD,$1);}
		| roll		{$$=make_lowl(T_ROLL,$1);}
		| roll_def_st	{$$=make_lowl(T_ROLL_DEF,$1);}
		| external_ref	{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
roll_reinit_period:	T_ROLL_REINIT_PERIOD '=' unit_value ';' {$$=$3;}
;
roll_inc_period:	T_ROLL_INC_PERIOD '=' value ';'	{$$=$3;}
;
roll:		T_ROLL '=' T_NAME ';'		{$$=$3;}
;
roll_def_st:	T_ROLL_DEF '=' value_list ';'	{$$=$3;}
;
/* $SCHEDULING_PARAMS block */

 scheduling_params_block:	B_SCHEDULING_PARAMS ';' scheduling_params_defs
								{$$=$3;}
			| B_SCHEDULING_PARAMS ';'		{$$=NULL;}
;
scheduling_params_defs:	scheduling_params_defs scheduling_params_defx
							{$$=add_list($1,$2);}
			| scheduling_params_defx
						{$$=add_list(NULL,$1);}
;
scheduling_params_defx:	scheduling_params_def	{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
scheduling_params_def:	T_DEF T_NAME ';' scheduling_params_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
scheduling_params_lowls:	scheduling_params_lowls scheduling_params_lowl
							{$$=add_list($1,$2);}
			| scheduling_params_lowl
						{$$=add_list(NULL,$1);}
;
scheduling_params_lowl:	external_ref	{$$=make_lowl(T_REF,$1);}
			|literal	{$$=make_lowl(T_LITERAL,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
/* $SEFD block */

sefd_block:	B_SEFD ';' sefd_defs	{$$=$3;}
		| B_SEFD ';'		{$$=NULL;}
;
sefd_defs:	sefd_defs sefd_defx	{$$=add_list($1,$2);}
		| sefd_defx		{$$=add_list(NULL,$1);}
;
sefd_defx:	sefd_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
sefd_def:	T_DEF T_NAME ';' sefd_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
sefd_lowls:	sefd_lowls sefd_lowl	{$$=add_list($1,$2);}
		| sefd_lowl		{$$=add_list(NULL,$1);}
;
sefd_lowl:	sefd_model	{$$=make_lowl(T_SEFD_MODEL,$1);}
		| sefd		{$$=make_lowl(T_SEFD,$1);}
		| external_ref	{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
sefd_model:	T_SEFD_MODEL '=' T_NAME ';' {$$=$3;}
;
sefd:		T_SEFD '=' T_LINK ':' unit_value ':' value_list ';'
						{$$=make_sefd($3,$5,$7);}
;
/* $SITE block */

site_block:	B_SITE ';' site_defs	{$$=$3;}
		| B_SITE ';'		{$$=NULL;}
;
site_defs:	site_defs site_defx	{$$=add_list($1,$2);}
		| site_defx		{$$=add_list(NULL,$1);}
;
site_defx:	site_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
site_def:	T_DEF T_NAME ';' site_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'	{$$=make_def($2,NULL);}
;
site_lowls:	site_lowls site_lowl	{$$=add_list($1,$2);}
		| site_lowl		{$$=add_list(NULL,$1);}
;
site_lowl:	site_type	{$$=make_lowl(T_SITE_TYPE,$1);}
		| site_name	{$$=make_lowl(T_SITE_NAME,$1);}
		| site_id	{$$=make_lowl(T_SITE_ID,$1);}
		| site_position	{$$=make_lowl(T_SITE_POSITION,$1);}
		| site_position_epoch {$$=make_lowl(T_SITE_POSITION_EPOCH,$1);}
		| site_position_ref   {$$=make_lowl(T_SITE_POSITION_REF,$1);}
		| site_velocity	      {$$=make_lowl(T_SITE_VELOCITY,$1);}
		| horizon_map_az {$$=make_lowl(T_HORIZON_MAP_AZ,$1);}
		| horizon_map_el {$$=make_lowl(T_HORIZON_MAP_EL,$1);}
		| zen_atmos	{$$=make_lowl(T_ZEN_ATMOS,$1);}
		| ocean_load_vert	{$$=make_lowl(T_OCEAN_LOAD_VERT,$1);}
		| ocean_load_horiz	{$$=make_lowl(T_OCEAN_LOAD_HORIZ,$1);}
		| occupation_code	{$$=make_lowl(T_OCCUPATION_CODE,$1);}
		| inclination		{$$=make_lowl(T_INCLINATION,$1);}
		| eccentricity		{$$=make_lowl(T_ECCENTRICITY,$1);}
		| arg_perigee		{$$=make_lowl(T_ARG_PERIGEE,$1);}
		| ascending_node	{$$=make_lowl(T_ASCENDING_NODE,$1);}
		| mean_anomaly		{$$=make_lowl(T_MEAN_ANOMALY,$1);}
		| semi_major_axis	{$$=make_lowl(T_SEMI_MAJOR_AXIS,$1);}
		| mean_motion		{$$=make_lowl(T_MEAN_MOTION,$1);}
		| orbit_epoch		{$$=make_lowl(T_ORBIT_EPOCH,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
site_type:	T_SITE_TYPE '=' T_NAME ';' {$$=$3;}
;
site_name:	T_SITE_NAME '=' T_NAME ';' {$$=$3;}
;
site_id:	T_SITE_ID '=' T_NAME ';' {$$=$3;}
;
site_position:	T_SITE_POSITION '=' unit_value ':' unit_value ':'
		unit_value ';'
		{$$=make_site_position($3,$5,$7);}		
;
site_position_epoch:	T_SITE_POSITION_EPOCH '=' T_NAME ';' {$$=$3;}
;
site_position_ref:	T_SITE_POSITION_REF '=' T_NAME ';' {$$=$3;}
;
site_velocity:	T_SITE_VELOCITY '=' unit_value ':' unit_value ':'
		unit_value ';'
		{$$=make_site_velocity($3,$5,$7);}		
;
horizon_map_az:	T_HORIZON_MAP_AZ '=' unit_list ';'	{$$=$3;}
;
horizon_map_el:	T_HORIZON_MAP_EL '=' unit_list ';'	{$$=$3;}
;
zen_atmos:	T_ZEN_ATMOS '=' unit_value ';'		{$$=$3;}
;
ocean_load_vert:	T_OCEAN_LOAD_VERT '=' unit_value ':'
			unit_value ';'
				{$$=make_ocean_load_vert($3,$5);}
;
ocean_load_horiz:	T_OCEAN_LOAD_HORIZ '=' unit_value ':'
			unit_value ';'
				{$$=make_ocean_load_horiz($3,$5);}
;
occupation_code:	T_OCCUPATION_CODE '=' name_value ';' {$$=$3;}
;
inclination:	T_INCLINATION '=' unit_value ';'	{$$=$3;}
;
eccentricity:	T_ECCENTRICITY '=' value ';'		{$$=$3;}
;
arg_perigee:	T_ARG_PERIGEE '=' unit_value ';'	{$$=$3;}
;
ascending_node:	T_ASCENDING_NODE '=' unit_value ';'	{$$=$3;}
;
mean_anomaly:	T_MEAN_ANOMALY '=' unit_value ';'	{$$=$3;}
;
semi_major_axis:	T_SEMI_MAJOR_AXIS '=' unit_value ';' {$$=$3;}
;
mean_motion:	T_MEAN_MOTION '=' value ';'	{$$=$3;}
;
orbit_epoch:	T_ORBIT_EPOCH '=' T_NAME ';' {$$=$3;}
;
/* $SOURCE block */

source_block:	B_SOURCE ';' source_defs	{$$=$3;}
		| B_SOURCE ';'			{$$=NULL;}
;
source_defs:	source_defs source_defx	{$$=add_list($1,$2);}
		| source_defx		{$$=add_list(NULL,$1);}
;
source_defx:	source_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
source_def:	T_DEF T_NAME ';' source_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
source_lowls:	source_lowls source_lowl	{$$=add_list($1,$2);}
		| source_lowl			{$$=add_list(NULL,$1);}
;
source_lowl:	source_type		{$$=make_lowl(T_SOURCE_TYPE,$1);}
		| source_name		{$$=make_lowl(T_SOURCE_NAME,$1);}
		| iau_name		{$$=make_lowl(T_IAU_NAME,$1);}
		| ra			{$$=make_lowl(T_RA,$1);}
		| dec			{$$=make_lowl(T_DEC,$1);}
		| ref_coord_frame	{$$=make_lowl(T_REF_COORD_FRAME,$1);}
		| source_position_ref {$$=make_lowl(T_SOURCE_POSITION_REF,$1);}
	| source_position_epoch {$$=make_lowl(T_SOURCE_POSITION_EPOCH,$1);}
		| ra_rate		{$$=make_lowl(T_RA_RATE,$1);}
		| dec_rate		{$$=make_lowl(T_DEC_RATE,$1);}
		| velocity_wrt_lsr	{$$=make_lowl(T_VELOCITY_WRT_LSR,$1);}
		| source_model		{$$=make_lowl(T_SOURCE_MODEL,$1);}
		| inclination		{$$=make_lowl(T_INCLINATION,$1);}
		| eccentricity		{$$=make_lowl(T_ECCENTRICITY,$1);}
		| arg_perigee		{$$=make_lowl(T_ARG_PERIGEE,$1);}
		| ascending_node	{$$=make_lowl(T_ASCENDING_NODE,$1);}
		| mean_anomaly		{$$=make_lowl(T_MEAN_ANOMALY,$1);}
		| semi_major_axis	{$$=make_lowl(T_SEMI_MAJOR_AXIS,$1);}
		| mean_motion		{$$=make_lowl(T_MEAN_MOTION,$1);}
		| orbit_epoch		{$$=make_lowl(T_ORBIT_EPOCH,$1);}
		| external_ref		{$$=make_lowl(T_REF,$1);}
		| T_COMMENT   		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING	{$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
source_type:	T_SOURCE_TYPE '=' T_NAME ';'	{$$=add_list(NULL,$3);}
		| T_SOURCE_TYPE '=' T_NAME ':' T_NAME ';'
					{$$=add_list(add_list(NULL,$3),$5);}
;
source_name:	T_SOURCE_NAME '=' T_NAME ';'	{$$=$3;}
;
iau_name:	T_IAU_NAME '=' T_NAME ';'	{$$=$3;}
;
ra:		T_RA '=' T_NAME ';'		{$$=$3;}
;
dec:		T_DEC '=' T_ANGLE ';'		{$$=$3;}
;
ref_coord_frame:	T_REF_COORD_FRAME '=' T_NAME ';'	{$$=$3;}
;
source_position_ref:	T_SOURCE_POSITION_REF '=' T_NAME ';'	{$$=$3;}
;
source_position_epoch:	T_SOURCE_POSITION_EPOCH '=' T_NAME ';'	{$$=$3;}
;
ra_rate:	T_RA_RATE '=' unit_value ';'	{$$=$3;}
;
dec_rate:	T_DEC_RATE '=' unit_value ';'	{$$=$3;}
;
velocity_wrt_lsr:	T_VELOCITY_WRT_LSR '=' unit_value ';'
								{$$=$3;}
;
source_model:	T_SOURCE_MODEL '=' value ':'
		T_LINK ':'
		unit_value ':'
		unit_value ':'
		value ':'
		unit_value ':'
		unit_value ':'
		unit_value ';'
		{$$=make_source_model($3,$5,$7,$9,$11,$13,$15,$17);}
;
/* $TAPELOG_OBS block */

tapelog_obs_block:	B_TAPELOG_OBS ';' tapelog_obs_defs	{$$=$3;}
			| B_TAPELOG_OBS ';'			{$$=NULL;}
;
tapelog_obs_defs:	tapelog_obs_defs tapelog_obs_defx
						{$$=add_list($1,$2);}
			| tapelog_obs_defx	{$$=add_list(NULL,$1);}
;
tapelog_obs_defx:	tapelog_obs_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
		| T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;

tapelog_obs_def:	T_DEF T_NAME ';' tapelog_obs_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
			| T_DEF T_NAME ';' T_ENDDEF ';'
						{$$=make_def($2,NULL);}
;
tapelog_obs_lowls:	tapelog_obs_lowls tapelog_obs_lowl
						{$$=add_list($1,$2);}
		| tapelog_obs_lowl		{$$=add_list(NULL,$1);}
;
tapelog_obs_lowl:	vsn		{$$=make_lowl(T_VSN,$1);}
			| external_ref	{$$=make_lowl(T_REF,$1);}
			| T_COMMENT     {$$=make_lowl(T_COMMENT,$1);}
			| T_COMMENT_TRAILING
				 {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
vsn:		T_VSN '=' value ':' T_NAME ':' T_NAME ':' T_NAME ';'
		{$$=make_vsn($3,$5,$7,$9);}
;
/* $TRACKS */

tracks_block:	B_TRACKS ';' tracks_defs	{$$=$3;}
		| B_TRACKS ';'			{$$=NULL;}
;
tracks_defs:	tracks_defs tracks_defx	{$$=add_list($1,$2);}
                | tracks_defx		{$$=add_list(NULL,$1);}
;
tracks_defx:	tracks_def		{$$=make_lowl(T_DEF,$1);}
		| T_COMMENT		{$$=make_lowl(T_COMMENT,$1);}
                | T_COMMENT_TRAILING    {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
tracks_def:	T_DEF T_NAME ';' tracks_lowls T_ENDDEF ';'
							{$$=make_def($2,$4);}
		| T_DEF T_NAME ';' T_ENDDEF ';'
                                                       {$$=make_def($2,NULL);}
;
tracks_lowls:	tracks_lowls tracks_lowl	{$$=add_list($1,$2);}
                | tracks_lowl			{$$=add_list(NULL,$1);}
;
tracks_lowl:	fanin_def	{$$=make_lowl(T_FANIN_DEF,$1);}
		| fanout_def	{$$=make_lowl(T_FANOUT_DEF,$1);}
		| track_frame_format
				{$$=make_lowl(T_TRACK_FRAME_FORMAT,$1);}
		| data_modulation	{$$=make_lowl(T_DATA_MODULATION,$1);}
		| vlba_frmtr_sys_trk
				{$$=make_lowl(T_VLBA_FRMTR_SYS_TRK,$1);}
		| vlba_trnsprt_sys_trk
				{$$=make_lowl(T_VLBA_TRNSPRT_SYS_TRK,$1);}
		| s2_recording_mode	{$$=make_lowl(T_S2_RECORDING_MODE,$1);}
		| s2_data_source	{$$=make_lowl(T_S2_DATA_SOURCE,$1);}
		| external_ref	{$$=make_lowl(T_REF,$1);}
		| T_COMMENT     {$$=make_lowl(T_COMMENT,$1);}
                | T_COMMENT_TRAILING {$$=make_lowl(T_COMMENT_TRAILING,$1);}
;
fanin_def:	T_FANIN_DEF '=' T_NAME ':' value ':' value ':'
                bit_stream_list ';'	{$$=make_fanin_def($3,$5,$7,$9);}
;
fanout_def:	T_FANOUT_DEF '=' T_NAME ':' bit_stream_list ':'
		value ':' value_list ';'
                {$$=make_fanout_def($3,$5,$7,$9);}
                | T_FANOUT_DEF '=' ':' bit_stream_list ':'
		  value ':' value_list ';'
                  {$$=make_fanout_def(NULL,$4,$6,$8);}
;
track_frame_format:	T_TRACK_FRAME_FORMAT '=' T_NAME ';' {$$=$3;}
;
data_modulation:	T_DATA_MODULATION '=' T_NAME ';' {$$=$3;}
;
vlba_frmtr_sys_trk:	T_VLBA_FRMTR_SYS_TRK '=' value ':' T_NAME ':'
			value ':' value ';'
                        {$$=make_vlba_frmtr_sys_trk($3,$5,$7,$9);}
			| T_VLBA_FRMTR_SYS_TRK '=' value ':' T_NAME ':'
			value ';'
                        {$$=make_vlba_frmtr_sys_trk($3,$5,$7,NULL);}
;
vlba_trnsprt_sys_trk:	T_VLBA_TRNSPRT_SYS_TRK '=' value ':' value ';'
                        {$$=add_list(add_list(NULL,$3),$5);}
;
s2_recording_mode:	T_S2_RECORDING_MODE '=' T_NAME ';' {$$=$3;}
;
s2_data_source:	T_S2_DATA_SOURCE '=' T_NAME ':' T_LINK ':' T_LINK ';'
                {$$=make_s2_data_source($3,$5,$7);}
		| T_S2_DATA_SOURCE '=' T_NAME ';'
                {$$=make_s2_data_source($3,NULL,NULL);}
;
bit_stream_list:	bit_stream_list ':' T_LINK ':' T_NAME 
                                        {$$=add_list(add_list($1,$3),$5);}
			| T_LINK ':' T_NAME	
                                        {$$=add_list(add_list(NULL,$1),$3);}
;
/* utility rules */

external_ref:	T_REF T_NAME ':' primitive '=' T_NAME ';'
			                        {$$=make_external($2,$4,$6);}
;
literal:	T_LITERAL ';'		{$$=$1;}
;
unit_list:	unit_value ':' unit_more	{$$=ins_list($1,$3);}
                   | unit_value			{$$=add_list(NULL,$1);}
;
unit_more:	unit_more ':' unit_option	{$$=add_list($1,$3);}
                  | unit_option			{$$=add_list(NULL,$1);}
;
unit_option:	unit_value			{$$=$1;}
		| value				
;
unit_value:	T_NAME T_NAME		{$$=make_dvalue($1,$2);}
;
name_list:	name_list ':' name_value	{$$=add_list($1,$3);}
                | name_value			{$$=add_list(NULL,$1);}
;
name_value:	T_NAME				
;
value_list:	value_list ':' value		{$$=add_list($1,$3);}
                | value 			{$$=add_list(NULL,$1);}
;
value:		T_NAME  			{$$=make_dvalue($1,NULL);}
;
%%

void yyerror(const char *s) {
  fprintf(stderr,"%s at line %d\n",s,lines);
  exit(1);
}


