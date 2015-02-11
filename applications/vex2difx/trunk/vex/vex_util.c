#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vex.h"
#include "vex_parse.tab.h"

#define NEWSTRUCT(PTR,TYPE)	struct TYPE *PTR;\
			PTR=(struct TYPE *) malloc(sizeof(struct TYPE));\
			if(PTR == NULL) {\
			fprintf(stderr,"out of memory allocating TYPE\n");\
			exit(1);}

static int
get_chan_def_field(Chan_def *chan_def,int n,int *link,int *name, 
		   char **value, char **units);
static int
get_switching_cycle_field(Switching_cycle *switching_cycle,int n,int *link,
			  int *name, char **value, char **units);
static int
get_station_field(Station *station,int n,int *link, int * name,
		  char **value, char **units);
static int
get_data_transfer_field(Data_transfer *data_transfer,int n,int *link, 
			int * name,
			char **value,
			char **units);
static int
get_axis_type_field(Axis_type *axis_type,int n,int *link,
			  int *name, char **value, char **units);
static int
get_antenna_motion_field(Antenna_motion *antenna_motion,int n,int *link,
			  int *name, char **value, char **units);
static int
get_pointing_sector_field(Pointing_sector *pointing_sector,int n,int *link,
			  int *name, char **value, char **units);
static int
get_bbc_assign_field(Bbc_assign *bbc_assign,int n,int *link,
			  int *name, char **value, char **units);
static int
get_clock_early_field(Clock_early *clock_early,int n,int *link,
			  int *name, char **value, char **units);
static int
get_headstack_field(Headstack *headstack,int n,int *link,
			  int *name, char **value, char **units);
static int
get_tape_length_field(Tape_Length *tape_length,int n,int *link,
			  int *name, char **value, char **units);
static int
get_tape_motion_field(Tape_Motion *tape_motion,int n,int *link,
			  int *name, char **value, char **units);
static int
get_headstack_pos_field(Headstack_pos *headstack_pos,int n,int *link,
			  int *name, char **value, char **units);
static int
get_if_def_field(If_def *if_def,int n,int *link,
		 int *name, char **value, char **units);
static int
get_phase_cal_detect_field(Phase_cal_detect *phase_cal_detect,int n,int *link,
		 int *name, char **value, char **units);
static int
get_setup_always_field(Setup_always *setup_always,int n,int *link,
		 int *name, char **value, char **units);
static int
get_parity_check_field(Parity_check *parity_check,int n,int *link,
		 int *name, char **value, char **units);
static int
get_tape_prepass_field(Tape_prepass *tape_prepass,int n,int *link,
		 int *name, char **value, char **units);
static int
get_preob_cal_field(Preob_cal *preob_cal,int n,int *link,
		 int *name, char **value, char **units);
static int
get_midob_cal_field(Midob_cal *midob_cal,int n,int *link,
		 int *name, char **value, char **units);
static int
get_postob_cal_field(Postob_cal *postob_cal,int n,int *link,
		 int *name, char **value, char **units);
static int
get_sefd_field(Sefd *sefd,int n,int *link,
		 int *name, char **value, char **units);
static int
get_site_position_field(Site_position *site_position,int n,int *link,
		 int *name, char **value, char **units);
static int
get_site_velocity_field(Site_velocity *site_velocity,int n,int *link,
		 int *name, char **value, char **units);
static int
get_ocean_load_vert_field(Ocean_load_vert *ocean_load_vert,int n,int *link,
		 int *name, char **value, char **units);
static int
get_ocean_load_horiz_field(Ocean_load_horiz *ocean_load_horiz,int n,int *link,
		 int *name, char **value, char **units);
static int
get_source_model_field(Source_model *source_model,int n,int *link,
		 int *name, char **value, char **units);
static int
get_vsn_field(Vsn *vsn,int n,int *link,
	      int *name, char **value, char **units);
static int
get_fanin_def_field(Fanin_def *fanin_def,int n,int *link,
	      int *name, char **value, char **units);
static int
get_fanout_def_field(Fanout_def *fanout_def,int n,int *link,
	      int *name, char **value, char **units);
static int
get_bitstream_list_field(Llist *list,int n,int *link,int *name,
		      char **value, char **units);
static int
get_vlba_frmtr_sys_trk_field(Vlba_frmtr_sys_trk *vlba_frmtr_sys_trk,int n,
			     int *link, int *name, char **value, char **units);
static int
get_s2_data_source_field(S2_data_source *s2_data_source,int n,int *link,
			  int *name, char **value, char **units);
static int
get_dvalue_field(Dvalue *dvalue,int n,int *link,int *name, char **value,
		 char **units);
static int
get_dvalue_list_field(Llist *list,int n,int *link, int *name,
		      char **value, char **units);
static int
get_svalue_field(char *svalue,int n,int *link, int *name,
		 char **value, char **units);
static int
get_svalue_list_field(Llist *list,int n,int *link,int *name,
		      char **value, char **units);
static int
get_date_field(char *date,int n,int *link, int *name, char **value,
	       char **units);
static int
get_time_field(char *time,int n,int *link, int *name, char **value,
		   char **units);
static int
get_angle_field(char *angle,int n,int *link, int *name, char **value,
		   char **units);

static struct {
  char *str;
  int value;
} blocks[] = {
  {"GLOBAL", B_GLOBAL},
  {"STATION", B_STATION},
  {"MODE", B_MODE},
  {"SCHED", B_SCHED},
  {"EXPER", B_EXPER},
  {"SCHEDULING_PARAMS", B_SCHEDULING_PARAMS},
  {"PROCEDURES", B_PROCEDURES},
  {"EOP", B_EOP},
  {"FREQ", B_FREQ},
  {"CLOCK", B_CLOCK},
  {"ANTENNA", B_ANTENNA},
  {"BBC", B_BBC},
  {"CORR", B_CORR},
  {"DAS", B_DAS},
  {"HEAD_POS", B_HEAD_POS},
  {"PASS_ORDER", B_PASS_ORDER},
  {"PHASE_CAL_DETECT", B_PHASE_CAL_DETECT},
  {"ROLL", B_ROLL},
  {"IF", B_IF},
  {"SEFD", B_SEFD},
  {"SITE", B_SITE},
  {"SOURCE", B_SOURCE},
  {"TAPELOG_OBS", B_TAPELOG_OBS},
  {"TRACKS", B_TRACKS},
  {NULL, 0}
};
static  struct {
  char *str;
  int  value;
} lowls[]= {
  {"VEX_rev", T_VEX_REV},
  {"chan_def", T_CHAN_DEF},
  {"sample_rate", T_SAMPLE_RATE},
  
  {"bits_per_sample", T_BITS_PER_SAMPLE},
  {"switching_cycle", T_SWITCHING_CYCLE},
    
  {"start", T_START},
  {"source", T_SOURCE},
  {"mode", T_MODE},
  {"station", T_STATION},
  {"data_transfer", T_DATA_TRANSFER},
    
  {"antenna_diam", T_ANTENNA_DIAM},
  {"axis_type", T_AXIS_TYPE},
  {"axis_offset", T_AXIS_OFFSET},
  {"antenna_motion", T_ANTENNA_MOTION},
  {"pointing_sector", T_POINTING_SECTOR},
  
  {"BBC_assign", T_BBC_ASSIGN},
  
  {"clock_early", T_CLOCK_EARLY},
    
  {"record_transport_type", T_RECORD_TRANSPORT_TYPE},
  {"electronics_rack_type", T_ELECTRONICS_RACK_TYPE},
  {"number_drives", T_NUMBER_DRIVES},
  {"headstack", T_HEADSTACK},
  {"record_density", T_RECORD_DENSITY},
  {"tape_length", T_TAPE_LENGTH},
  {"recording_system_ID", T_RECORDING_SYSTEM_ID},
  {"tape_motion", T_TAPE_MOTION},
  {"tape_control", T_TAPE_CONTROL},
  
  {"TAI-UTC", T_TAI_UTC},
  {"A1-TAI", T_A1_TAI},
  {"eop_ref_epoch", T_EOP_REF_EPOCH},
  {"num_eop_points", T_NUM_EOP_POINTS},
  {"eop_interval", T_EOP_INTERVAL},
  {"ut1-utc", T_UT1_UTC},
  {"x_wobble", T_X_WOBBLE},
  {"y_wobble", T_Y_WOBBLE},
  {"nut_ref_epoch", T_NUT_REF_EPOCH},
  {"num_nut_points", T_NUM_NUT_POINTS},
  {"nut_interval", T_NUT_INTERVAL},
  {"delta_psi", T_DELTA_PSI},
  {"delta_eps", T_DELTA_EPS},
  {"nut_model", T_NUT_MODEL},
  
  {"exper_num", T_EXPER_NUM},
  {"exper_name", T_EXPER_NAME},
  {"exper_description", T_EXPER_DESCRIPTION},
  {"exper_nominal_start", T_EXPER_NOMINAL_START},
  {"exper_nominal_stop", T_EXPER_NOMINAL_STOP},
  {"PI_name", T_PI_NAME},
  {"PI_email", T_PI_EMAIL},
  {"contact_name", T_CONTACT_NAME},
  {"contact_email", T_CONTACT_EMAIL},
  {"scheduler_name", T_SCHEDULER_NAME},
  {"scheduler_email", T_SCHEDULER_EMAIL},
  {"target_correlator", T_TARGET_CORRELATOR},
    
  {"headstack_pos", T_HEADSTACK_POS},
  
  {"if_def", T_IF_DEF},
  
  {"pass_order", T_PASS_ORDER},
  {"S2_group_order", T_S2_GROUP_ORDER},
  
  {"phase_cal_detect", T_PHASE_CAL_DETECT},
  
  {"tape_change", T_TAPE_CHANGE},
  {"headstack_motion", T_HEADSTACK_MOTION},
  {"new_source_command", T_NEW_SOURCE_COMMAND},
  {"new_tape_setup", T_NEW_TAPE_SETUP},
  {"setup_always", T_SETUP_ALWAYS},
  {"parity_check", T_PARITY_CHECK},
  {"tape_prepass", T_TAPE_PREPASS},
  {"preob_cal", T_PREOB_CAL},
  {"midob_cal", T_MIDOB_CAL},
  {"postob_cal", T_POSTOB_CAL},
  {"procedure_name_prefix", T_PROCEDURE_NAME_PREFIX},
    
  {"roll_reinit_period", T_ROLL_REINIT_PERIOD},
  {"roll_inc_period", T_ROLL_INC_PERIOD},
  {"roll", T_ROLL},
  {"roll_def", T_ROLL_DEF},
  
  {"sefd_model", T_SEFD_MODEL},
  {"sefd", T_SEFD},
  
  {"site_type", T_SITE_TYPE},
  {"site_name", T_SITE_NAME},
  {"site_ID", T_SITE_ID},
  {"site_position", T_SITE_POSITION},
  {"site_position_epoch", T_SITE_POSITION_EPOCH},
  {"site_position_ref", T_SITE_POSITION_REF},
  {"site_velocity", T_SITE_VELOCITY},
  {"horizon_map_az", T_HORIZON_MAP_AZ},
  {"horizon_map_el", T_HORIZON_MAP_EL},
  {"zen_atmos", T_ZEN_ATMOS},
  {"ocean_load_vert", T_OCEAN_LOAD_VERT},
  {"ocean_load_horiz", T_OCEAN_LOAD_HORIZ},
  {"occupation_code", T_OCCUPATION_CODE},
  {"inclination", T_INCLINATION},
  {"eccentricity", T_ECCENTRICITY},
  {"arg_perigee", T_ARG_PERIGEE},
  {"ascending_node", T_ASCENDING_NODE},
  {"mean_anomaly", T_MEAN_ANOMALY},
  {"semi-major_axis", T_SEMI_MAJOR_AXIS},
  {"mean_motion", T_MEAN_MOTION},
  {"orbit_epoch", T_ORBIT_EPOCH},
  
  {"source_type", T_SOURCE_TYPE},
  {"source_name", T_SOURCE_NAME},
  {"IAU_name", T_IAU_NAME},
  {"ra", T_RA},
  {"dec", T_DEC},
  {"ref_coord_frame", T_REF_COORD_FRAME},
  {"source_position_ref", T_SOURCE_POSITION_REF},
  {"source_position_epoch", T_SOURCE_POSITION_EPOCH},
  {"ra_rate", T_RA_RATE},
  {"dec_rate", T_DEC_RATE},
  {"velocity_wrt_LSR", T_VELOCITY_WRT_LSR},
  {"source_model", T_SOURCE_MODEL},
  
  {"VSN", T_VSN},
  
  {"fanin_def", T_FANIN_DEF},
  {"fanout_def", T_FANOUT_DEF},
  {"track_frame_format", T_TRACK_FRAME_FORMAT},
  {"data_modulation", T_DATA_MODULATION},
  {"VLBA_frmtr_sys_trk", T_VLBA_FRMTR_SYS_TRK},
  {"VLBA_trnsprt_sys_trk", T_VLBA_TRNSPRT_SYS_TRK},
  {"S2_recording_mode", T_S2_RECORDING_MODE},
  {"S2_data_source", T_S2_DATA_SOURCE},
  {"literals", T_LITERAL},
  {"comment", T_COMMENT},
  {NULL, 0}
};

struct llist *add_list(struct llist *start,void *ptr)
{
  struct llist *last;
  NEWSTRUCT(new,llist);

  new->ptr=ptr;
  new->next=NULL;
  
  if(start == NULL)
    return new;
  
  for (last=start;last->next !=NULL;last=last->next)
    ;

  last->next=new;
  
  return start;
}
struct llist *ins_list(void *ptr, struct llist *start)
{
  NEWSTRUCT(new,llist);

  new->ptr=ptr;
  new->next=start;
  
  return new;
}

struct qref *make_qref(int primitive,char *name,struct llist *qualifiers)
{
  NEWSTRUCT(new,qref);

  new->primitive=primitive;
  new->name=name;
  new->qualifiers=qualifiers;

  return new;
}

struct def *make_def(char *name, struct llist *refs)
{
  NEWSTRUCT(new,def);

  new->name=name;
  new->refs=refs;

  return new;
}

struct block *make_block(int block,struct llist *items)
{
  NEWSTRUCT(new,block);

  new->block=block;
  new->items=items;

  return new;
}

struct vex *make_vex(struct llist *version, struct llist *blocks)
{
  NEWSTRUCT(new,vex);

  new->version=version;
  new->blocks=blocks;

  return new;
}

struct lowl *make_lowl(int statement,void *item)
{
  NEWSTRUCT(new,lowl);

  new->statement=statement;
  new->item=item;

  return new;
}

struct chan_def  *make_chan_def(char *band_id, struct dvalue *sky_freq,
				char *net_sb, struct dvalue *bw,
				char *chan_id, char *bbc_id, char *pcal_id,
				struct llist *states)
{
  NEWSTRUCT(new,chan_def);

  new->band_id=band_id;
  new->sky_freq=sky_freq;
  new->net_sb=net_sb;
  new->bw=bw;
  new->chan_id=chan_id;
  new->bbc_id=bbc_id;
  new->pcal_id=pcal_id;
  new->states=states;

  return new;
}

struct dvalue *make_dvalue(char *value, char *units)
{
  NEWSTRUCT(new,dvalue);

  new->value=value;
  new->units=units;

  return new;
}
struct external *make_external(char *file, int primitive, char *name)
{
  NEWSTRUCT(new,external);

  new->file=file;
  new->primitive=primitive;
  new->name=name;

  return new;
}
struct switching_cycle *make_switching_cycle(char *origin,
					      struct llist *periods)
{
  NEWSTRUCT(new,switching_cycle);

  new->origin=origin;
  new->periods=periods;

  return new;
}

struct station  *make_station(char *key, struct dvalue *start,
			      struct dvalue *stop, struct dvalue *start_pos,
			      char *pass, char *sector, struct llist *drives)
{
  NEWSTRUCT(new,station);

  new->key=key;
  new->start=start;
  new->stop=stop;
  new->start_pos=start_pos;
  new->pass=pass;
  new->sector=sector;
  new->drives=drives;
  return new;
}

struct data_transfer  *make_data_transfer(char *key, char *method, 
					  char *destination,
					  struct dvalue *start,
					  struct dvalue *stop, 
					  char *options)
{
  NEWSTRUCT(new,data_transfer);

  new->key=key;
  new->method=method;
  new->destination=destination;
  new->start=start;
  new->stop=stop;
  new->options=options;
  return new;
}

struct axis_type *make_axis_type(char *axis1, char *axis2)
{
  NEWSTRUCT(new,axis_type);

  new->axis1=axis1;
  new->axis2=axis2;

  return new;
}

struct antenna_motion *make_antenna_motion(char *axis,struct dvalue *rate,
					   struct dvalue *offset)
{
  NEWSTRUCT(new,antenna_motion);

  new->axis=axis;
  new->rate=rate;
  new->offset=offset;

  return new;
}

struct pointing_sector *make_pointing_sector(char *sector, char *axis1,
					     struct dvalue *lolimit1,
					     struct dvalue *hilimit1,
					     char *axis2,
					     struct dvalue *lolimit2,
					     struct dvalue *hilimit2)
{
  NEWSTRUCT(new,pointing_sector);

  new->sector=sector;
  new->axis1=axis1;
  new->lolimit1=lolimit1;
  new->hilimit1=hilimit1;
  new->axis2=axis2;
  new->lolimit2=lolimit2;
  new->hilimit2=hilimit2;

  return new;
}
struct bbc_assign *make_bbc_assign(char *bbc_id,struct dvalue *physical,
				   char *if_id)
{
  NEWSTRUCT(new,bbc_assign);

  new->bbc_id=bbc_id;
  new->physical=physical;
  new->if_id=if_id;

  return new;
}
struct clock_early *make_clock_early(char *start,struct dvalue *offset,
				     char *origin, struct dvalue *rate)
{
  NEWSTRUCT(new,clock_early);

  new->start=start;
  new->offset=offset;
  new->origin=origin;
  new->rate=rate;

  return new;
}
struct headstack *make_headstack(struct dvalue *stack,char *type,
				 struct dvalue *offset)
{
  NEWSTRUCT(new,headstack);

  new->stack=stack;
  new->type=type;
  new->offset=offset;

  return new;
}
struct tape_length *make_tape_length(struct dvalue *duration, char *speed,
				     struct dvalue *tapes)
{
  NEWSTRUCT(new,tape_length);
 
  new->duration=duration;
  new->speed=speed;
  new->tapes=tapes;

  return new;
}
    
struct tape_motion *make_tape_motion(char *type, struct dvalue *early,
				     struct dvalue *late, struct dvalue *gap)
{
  NEWSTRUCT(new,tape_motion);
 
  new->type=type;
  new->early=early;
  new->late=late;
  new->gap=gap;

  return new;
}
struct headstack_pos *make_headstack_pos(struct dvalue *index,
					 struct llist *positions)
{
  NEWSTRUCT(new,headstack_pos);

  new->index=index;
  new->positions=positions;

  return new;
}

struct if_def *make_if_def(char *if_id, char *physical, char *polar,
			   struct dvalue *lo, char *sb,
			   struct dvalue *pcal_spacing,
			   struct dvalue *pcal_base,
			   char *comment)
{
  NEWSTRUCT(new,if_def);

  new->if_id=if_id;
  new->physical=physical;
  new->polar=polar;
  new->lo=lo;
  new->sb=sb;
  new->pcal_spacing=pcal_spacing;
  new->pcal_base=pcal_base;
  new->comment=comment;

  return new;
}
struct phase_cal_detect *make_phase_cal_detect(char *pcal_id,
					       struct llist *tones)
{
  NEWSTRUCT(new,phase_cal_detect);

  new->pcal_id=pcal_id;
  new->tones=tones;

  return new;
}
struct setup_always *make_setup_always(char *state, struct dvalue *time)
{
  NEWSTRUCT(new,setup_always);

  new->state=state;
  new->time=time;

  return new;
}
struct parity_check *make_parity_check(char *state, struct dvalue *time)
{
  NEWSTRUCT(new,parity_check);

  new->state=state;
  new->time=time;

  return new;
}
struct tape_prepass *make_tape_prepass(char *state, struct dvalue *time)
{
  NEWSTRUCT(new,tape_prepass);

  new->state=state;
  new->time=time;

  return new;
}
struct preob_cal *make_preob_cal(char *state, struct dvalue *time,
				 char *name)
{
  NEWSTRUCT(new,preob_cal);

  new->state=state;
  new->time=time;
  new->name=name;

  return new;
}
struct midob_cal *make_midob_cal(char *state, struct dvalue *time,
				 char *name)
{
  NEWSTRUCT(new,midob_cal);

  new->state=state;
  new->time=time;
  new->name=name;

  return new;
}
struct postob_cal *make_postob_cal(char *state, struct dvalue *time,
				 char *name)
{
  NEWSTRUCT(new,postob_cal);

  new->state=state;
  new->time=time;
  new->name=name;

  return new;
}
struct sefd *make_sefd(char *if_id, struct dvalue *flux, struct llist *params)
{
  NEWSTRUCT(new,sefd);

  new->if_id=if_id;
  new->flux=flux;
  new->params=params;

  return new;
}

struct site_position *make_site_position(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z)
{
  NEWSTRUCT(new,site_position);

  new->x=x;
  new->y=y;
  new->z=z;

  return new;
}
struct site_velocity *make_site_velocity(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z)
{
  NEWSTRUCT(new,site_velocity);

  new->x=x;
  new->y=y;
  new->z=z;

  return new;
}
struct ocean_load_vert *make_ocean_load_vert(struct dvalue *amp,
					     struct dvalue *phase)
{
  NEWSTRUCT(new,ocean_load_vert);

  new->amp=amp;
  new->phase=phase;

  return new;
}
struct ocean_load_horiz *make_ocean_load_horiz(struct dvalue *amp,
					       struct dvalue *phase)
{
  NEWSTRUCT(new,ocean_load_horiz);

  new->amp=amp;
  new->phase=phase;

  return new;
}
struct source_model *make_source_model(struct dvalue *component,
				       char *band_id, struct dvalue *flux,
				       struct dvalue *majoraxis,
				       struct dvalue *ratio,
				       struct dvalue *angle,
				       struct dvalue *raoff,
				       struct dvalue *decoff)
{
  NEWSTRUCT(new,source_model);

  new->component=component;
  new->band_id=band_id;
  new->flux=flux;
  new->majoraxis=majoraxis;
  new->ratio=ratio;
  new->angle=angle;
  new->raoff=raoff;
  new->decoff=decoff;

  return new;
}
struct vsn *make_vsn(struct dvalue *drive, char *label, char *start,
		     char *stop)
{
  NEWSTRUCT(new,vsn);

  new->drive=drive;
  new->label=label;
  new->start=start;
  new->stop=stop;

  return new;
}
struct fanin_def *make_fanin_def(char *subpass, struct dvalue *hdstk,
				 struct dvalue *track,
				 struct llist *bitstreams)
{
  NEWSTRUCT(new,fanin_def);

  new->subpass=subpass;
  new->hdstk=hdstk;
  new->track=track;
  new->bitstreams=bitstreams;

  return new;
}
struct fanout_def *make_fanout_def(char *subpass, struct llist *bitstream,
				   struct dvalue *hdstk, struct llist *tracks)
{
  NEWSTRUCT(new,fanout_def);

  new->subpass=subpass;
  new->bitstream=bitstream;
  new->hdstk=hdstk;
  new->tracks=tracks;

  return new;
}
struct vlba_frmtr_sys_trk *make_vlba_frmtr_sys_trk(struct dvalue *output,
						   char *use,
						   struct dvalue *start,
						   struct dvalue *stop)
{
  NEWSTRUCT(new,vlba_frmtr_sys_trk);

  new->output=output;
  new->use=use;
  new->start=start;
  new->stop=stop;

  return new;
}
struct s2_data_source *make_s2_data_source(char *source,char *bbcx_id,
					   char *bbcy_id)
{
  NEWSTRUCT(new,s2_data_source);
 
  new->source=source;
  new->bbcx_id=bbcx_id;
  new->bbcy_id=bbcy_id;

  return new;
}
    

int
lowl2int(char *lowl)
{
  int i;

  for(i=0;lowls[i].str!=NULL;i++)
    if(0==strcmp(lowls[i].str,lowl))
      return lowls[i].value;

  return 0;
}
int
block2int(char *block)
{
  int i;

  for(i=0;blocks[i].str!=NULL;i++)
    if(0==strcmp(blocks[i].str,block))
      return blocks[i].value;

  return 0;
}
char *
int2lowl(int lowl)
{
  int i;

  for(i=0;lowls[i].str!=NULL;i++)
    if(lowls[i].value==lowl)
      return lowls[i].str;

  return NULL;
}
char *
int2block(int block)
{
  int i;

  for(i=0;blocks[i].str!=NULL;i++)
    if(blocks[i].value==block)
      return blocks[i].str;

  return NULL;
}
int
vex_field(int statement,void *ptr,int n,int *link,int *name,char **value,
char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  if(n==0) {
    *value=int2lowl(statement);
    if(*value==NULL)
      return -1;

    return 0;
  }

  switch (statement){
  case T_CHAN_DEF:
    ierr=get_chan_def_field(ptr,n,link,name,value,units);
    break;
  case T_VEX_REV:
  case T_SAMPLE_RATE:
  case T_BITS_PER_SAMPLE:
  case T_ANTENNA_DIAM:
  case T_AXIS_OFFSET:
  case T_NUMBER_DRIVES:
  case T_RECORD_DENSITY:
  case T_RECORDING_SYSTEM_ID:
  case T_TAI_UTC:
  case T_A1_TAI:
  case T_NUM_EOP_POINTS:
  case T_EOP_INTERVAL:
  case T_NUM_NUT_POINTS:
  case T_NUT_INTERVAL:
  case T_EXPER_NUM:
  case T_TAPE_CHANGE:
  case T_HEADSTACK_MOTION:
  case T_NEW_SOURCE_COMMAND:
  case T_NEW_TAPE_SETUP:
  case T_ROLL_REINIT_PERIOD:
  case T_ROLL_INC_PERIOD:
  case T_ZEN_ATMOS:
  case T_INCLINATION:
  case T_ECCENTRICITY:
  case T_ARG_PERIGEE:
  case T_ASCENDING_NODE:
  case T_MEAN_ANOMALY:
  case T_SEMI_MAJOR_AXIS:
  case T_MEAN_MOTION:
  case T_RA_RATE:
  case T_DEC_RATE:
  case T_VELOCITY_WRT_LSR:
    ierr=get_dvalue_field(ptr,n,link,name,value,units);
    break;
  case T_CLOCK_EARLY:
    ierr=get_clock_early_field(ptr,n,link,name,value,units);
    break;
  case T_SWITCHING_CYCLE:
    ierr=get_switching_cycle_field(ptr,n,link,name,value,units);
    break;
  case T_START:
  case T_EOP_REF_EPOCH:
  case T_NUT_REF_EPOCH:
  case T_EXPER_NOMINAL_START:
  case T_EXPER_NOMINAL_STOP:
  case T_ORBIT_EPOCH:
    ierr=get_date_field(ptr,n,link,name,value,units);
    break;
  case T_MODE:
  case T_SOURCE:
  case T_RECORD_TRANSPORT_TYPE:
  case T_ELECTRONICS_RACK_TYPE:
  case T_TAPE_CONTROL:
  case T_NUT_MODEL:
  case T_EXPER_NAME:
  case T_EXPER_DESCRIPTION:
  case T_PI_NAME:
  case T_PI_EMAIL:
  case T_CONTACT_NAME:
  case T_CONTACT_EMAIL:
  case T_SCHEDULER_NAME:
  case T_SCHEDULER_EMAIL:
  case T_TARGET_CORRELATOR:
  case T_PROCEDURE_NAME_PREFIX:
  case T_ROLL:
  case T_SEFD_MODEL:
  case T_SITE_TYPE:
  case T_SITE_NAME:
  case T_SITE_ID:
  case T_SITE_POSITION_EPOCH:
  case T_SITE_POSITION_REF:
  case T_OCCUPATION_CODE:
  case T_SOURCE_NAME:
  case T_IAU_NAME:
  case T_REF_COORD_FRAME:
  case T_SOURCE_POSITION_REF:
  case T_SOURCE_POSITION_EPOCH:
  case T_TRACK_FRAME_FORMAT:
  case T_DATA_MODULATION:
  case T_S2_RECORDING_MODE:
  case T_COMMENT:
    ierr=get_svalue_field(ptr,n,link,name,value,units);
    break;
  case T_STATION:
    ierr=get_station_field(ptr,n,link,name,value,units);
    break;
  case T_DATA_TRANSFER:
    ierr=get_data_transfer_field(ptr,n,link,name,value,units);
    break;
  case T_AXIS_TYPE:
    ierr=get_axis_type_field(ptr,n,link,name,value,units);
    break;
  case T_ANTENNA_MOTION:
    ierr=get_antenna_motion_field(ptr,n,link,name,value,units);
    break;
  case T_POINTING_SECTOR:
    ierr=get_pointing_sector_field(ptr,n,link,name,value,units);
    break;
  case T_BBC_ASSIGN:
    ierr=get_bbc_assign_field(ptr,n,link,name,value,units);
    break;
  case T_HEADSTACK:
    ierr=get_headstack_field(ptr,n,link,name,value,units);
    break;
  case T_TAPE_LENGTH:
    ierr=get_tape_length_field(ptr,n,link,name,value,units);
    break;
  case T_TAPE_MOTION:
    ierr=get_tape_motion_field(ptr,n,link,name,value,units);
    break;
  case T_UT1_UTC:
  case T_X_WOBBLE:
  case T_Y_WOBBLE:
  case T_DELTA_PSI:
  case T_DELTA_EPS:
  case T_S2_GROUP_ORDER:
  case T_ROLL_DEF:
  case T_HORIZON_MAP_AZ:
  case T_HORIZON_MAP_EL:
  case T_VLBA_TRNSPRT_SYS_TRK:
    ierr=get_dvalue_list_field(ptr,n,link,name,value,units);
    break;
  case T_HEADSTACK_POS:
    ierr=get_headstack_pos_field(ptr,n,link,name,value,units);
    break;
  case T_IF_DEF:
    ierr=get_if_def_field(ptr,n,link,name,value,units);
    break;
  case T_PASS_ORDER:
  case T_SOURCE_TYPE:
    ierr=get_svalue_list_field(ptr,n,link,name,value,units);
    break;
  case T_PHASE_CAL_DETECT:
    ierr=get_phase_cal_detect_field(ptr,n,link,name,value,units);
    break;
  case T_SETUP_ALWAYS:
    ierr=get_setup_always_field(ptr,n,link,name,value,units);
    break;
  case T_PARITY_CHECK:
    ierr=get_parity_check_field(ptr,n,link,name,value,units);
    break;
  case T_TAPE_PREPASS:
    ierr=get_tape_prepass_field(ptr,n,link,name,value,units);
    break;
  case T_PREOB_CAL:
    ierr=get_preob_cal_field(ptr,n,link,name,value,units);
    break;
  case T_MIDOB_CAL:
    ierr=get_midob_cal_field(ptr,n,link,name,value,units);
    break;
  case T_POSTOB_CAL:
    ierr=get_postob_cal_field(ptr,n,link,name,value,units);
    break;
  case T_SEFD:
    ierr=get_sefd_field(ptr,n,link,name,value,units);
    break;
  case T_SITE_POSITION:
    ierr=get_site_position_field(ptr,n,link,name,value,units);
    break;
  case T_SITE_VELOCITY:
    ierr=get_site_velocity_field(ptr,n,link,name,value,units);
    break;
  case T_OCEAN_LOAD_VERT:
    ierr=get_ocean_load_vert_field(ptr,n,link,name,value,units);
    break;
  case T_OCEAN_LOAD_HORIZ:
    ierr=get_ocean_load_horiz_field(ptr,n,link,name,value,units);
    break;
  case T_RA:
    ierr=get_time_field(ptr,n,link,name,value,units);
    break;
  case T_DEC:
    ierr=get_angle_field(ptr,n,link,name,value,units);
    break;
  case T_SOURCE_MODEL:
    ierr=get_source_model_field(ptr,n,link,name,value,units);
    break;
  case T_VSN:
    ierr=get_vsn_field(ptr,n,link,name,value,units);
    break;
  case T_FANIN_DEF:
    ierr=get_fanin_def_field(ptr,n,link,name,value,units);
    break;
  case T_FANOUT_DEF:
    ierr=get_fanout_def_field(ptr,n,link,name,value,units);
    break;
  case T_VLBA_FRMTR_SYS_TRK:
    ierr=get_vlba_frmtr_sys_trk_field(ptr,n,link,name,value,units);
    break;
  case T_S2_DATA_SOURCE:
    ierr=get_s2_data_source_field(ptr,n,link,name,value,units);
    break;
  case T_LITERAL:
    ierr=get_svalue_list_field(ptr,n,link,name,value,units);
    break;
  default:
    fprintf(stderr,"can't get here in get_field %d\n",statement);
    exit(1);
  }
  if(ierr==-1)
    return -2;
  else if (ierr!=0) {
    fprintf(stderr,"unknown error in vex_field %d\n",ierr);
    exit(1);
  }
  return 0;
}

static int
get_chan_def_field(Chan_def *chan_def,int n,int *link, int *name, 
		   char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=chan_def->band_id;
    *link=1;
    break;
  case 2:
    *value=chan_def->sky_freq->value;
    *units=chan_def->sky_freq->units;
    *name=0;
    break;
  case 3:
    *value=chan_def->net_sb;
    break;
  case 4:
    *value=chan_def->bw->value;
    *units=chan_def->bw->units;
    *name=0;
    break;
  case 5:
    *value=chan_def->chan_id;
    *link=1;
    break;
  case 6:
    *value=chan_def->bbc_id;
    *link=1;
    break;
  case 7:
    *value=chan_def->pcal_id;
    *link=1;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(chan_def->states,n-7,link,name,value,units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_chan_def_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_switching_cycle_field(Switching_cycle *switching_cycle,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=switching_cycle->origin;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(switching_cycle->periods,n-1,link,name,
			       value,units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_switching_cycle_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_station_field(Station *station,int n,int *link,int *name,char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=station->key;
    break;
  case 2:
    *value=station->start->value;
    *units=station->start->units;
    *name=0;
    break;
  case 3:
    *value=station->stop->value;
    *units=station->stop->units;
    *name=0;
    break;
  case 4:
    if(station->start_pos != NULL) {
      *value=station->start_pos->value;
      *units=station->start_pos->units;
    }
    *name=0;
    break;
  case 5:
    *value=station->pass;
    break;
  case 6:
    *link=1;
    *value=station->sector;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(station->drives,n-6,link,name,value,units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_station_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_data_transfer_field(Data_transfer *data_transfer,int n,int *link,
			int *name,char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=data_transfer->key;
    break;
  case 2:
    *value=data_transfer->method;
    break;
  case 3:
    *value=data_transfer->destination;
    break;
  case 4:
    *value=data_transfer->start->value;
    *units=data_transfer->start->units;
    *name=0;
    break;
  case 5:
    *value=data_transfer->stop->value;
    *units=data_transfer->stop->units;
    *name=0;
    break;
  case 6:
    *value=data_transfer->options;
    break;
  default:
      return -1;
    /*    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(station->drives,n-6,link,name,value,units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_data_transfer_field %d\n",ierr);
      exit(1);
      }*/
  }
  return 0;
}
static int
get_axis_type_field(Axis_type *axis_type,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=axis_type->axis1;
    break;
  case 2:
    *value=axis_type->axis2;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_antenna_motion_field(Antenna_motion *antenna_motion,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=antenna_motion->axis;
    break;
  case 2:
    *value=antenna_motion->rate->value;
    *units=antenna_motion->rate->units;
    *name=0;
    break;
  case 3:
    *value=antenna_motion->offset->value;
    *units=antenna_motion->offset->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}

static int
get_pointing_sector_field(Pointing_sector *pointing_sector,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=pointing_sector->sector;
    *link=1;
    break;
  case 2:
    *value=pointing_sector->axis1;
    break;
  case 3:
    *value=pointing_sector->lolimit1->value;
    *units=pointing_sector->lolimit1->units;
    *name=0;
    break;
  case 4:
    *value=pointing_sector->hilimit1->value;
    *units=pointing_sector->hilimit1->units;
    *name=0;
    break;
  case 5:
    *value=pointing_sector->axis2;
    break;
  case 6:
    *value=pointing_sector->lolimit2->value;
    *units=pointing_sector->lolimit2->units;
    *name=0;
    break;
  case 7:
    *value=pointing_sector->hilimit2->value;
    *units=pointing_sector->hilimit2->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}

static int
get_bbc_assign_field(Bbc_assign *bbc_assign,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=bbc_assign->bbc_id;
    *link=1;
    break;
  case 2:
    *value=bbc_assign->physical->value;
    *units=bbc_assign->physical->units;
    *name=0;
    break;
  case 3:
    *value=bbc_assign->if_id;
    *link=1;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_clock_early_field(Clock_early *clock_early,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=clock_early->start;
    break;
  case 2:
    *value=clock_early->offset->value;
    *units=clock_early->offset->units;
    *name=0;
    break;
  case 3:
    if(clock_early->origin == NULL)
      return -1;
    *value=clock_early->origin;
    break;
  case 4:
    if(clock_early->origin == NULL)
      return -1;
    if(clock_early->rate!=NULL) {
      *value=clock_early->rate->value;
      *units=clock_early->rate->units;
    }
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_headstack_field(Headstack *headstack,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=headstack->stack->value;
    *units=headstack->stack->units;
    *name=0;
    break;
  case 2:
    *value=headstack->type;
    break;
  case 3:
    *value=headstack->offset->value;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_tape_length_field(Tape_Length *tape_length,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=tape_length->duration->value;
    *units=tape_length->duration->units;
    *name=0;
    break;
  case 2:
    if(tape_length->speed==NULL)
      return -1;
    *value=tape_length->speed;
    break;
  case 3:
    if(tape_length->speed==NULL)
      return -1;
    *value=tape_length->tapes->value;
    *units=tape_length->tapes->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_tape_motion_field(Tape_Motion *tape_motion,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=tape_motion->type;
    break;
  case 2:
    if(tape_motion->early==NULL)
      return -1;
    *value=tape_motion->early->value;
    *units=tape_motion->early->units;
    *name=0;
    break;
  case 3:
    if(tape_motion->late==NULL)
      return -1;
    *value=tape_motion->late->value;
    *units=tape_motion->late->units;
    *name=0;
    break;
  case 4:
    if(tape_motion->late==NULL || tape_motion->early==NULL)
      return -1;
    *value=tape_motion->gap->value;
    *units=tape_motion->gap->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_headstack_pos_field(Headstack_pos *headstack_pos,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=headstack_pos->index->value;
    *units=headstack_pos->index->units;
    *name=0;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(headstack_pos->positions,n-1,link,name,
			       value, units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_headstack_pos_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_if_def_field(If_def *if_def,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=if_def->if_id;
    *link=1;
    break;
  case 2:
    *value=if_def->physical;
    break;
  case 3:
    *value=if_def->polar;
    break;
  case 4:
    *value=if_def->lo->value;
    *units=if_def->lo->units;
    *name=0;
    break;
  case 5:
    *value=if_def->sb;
    break;
  case 6:
    if(if_def->pcal_spacing == NULL)
      return -1;
    *value=if_def->pcal_spacing->value;
    *units=if_def->pcal_spacing->units;
    *name=0;
    break;
  case 7:
    if(if_def->pcal_base == NULL)
      return -1;
    *value=if_def->pcal_base->value;
    *units=if_def->pcal_base->units;
    *name=0;
    break;
  case 8:
    *value=if_def->comment;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_phase_cal_detect_field(Phase_cal_detect *phase_cal_detect,int n,int *link,
			   int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=phase_cal_detect->pcal_id;
    *link=1;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(phase_cal_detect->tones,n-1,link,name,value,
			       units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_phase_cal_detect_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_setup_always_field(Setup_always *setup_always,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=setup_always->state;
    break;
  case 2:
    *value=setup_always->time->value;
    *units=setup_always->time->units;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_parity_check_field(Parity_check *parity_check,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=parity_check->state;
    break;
  case 2:
    *value=parity_check->time->value;
    *units=parity_check->time->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_tape_prepass_field(Tape_prepass *tape_prepass,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=tape_prepass->state;
    break;
  case 2:
    *value=tape_prepass->time->value;
    *units=tape_prepass->time->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_preob_cal_field(Preob_cal *preob_cal,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=preob_cal->state;
    break;
  case 2:
    *value=preob_cal->time->value;
    *units=preob_cal->time->units;
    *name=0;
    break;
  case 3:
    *value=preob_cal->name;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_midob_cal_field(Midob_cal *midob_cal,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=midob_cal->state;
    break;
  case 2:
    *value=midob_cal->time->value;
    *units=midob_cal->time->units;
    *name=0;
    break;
  case 3:
    *value=midob_cal->name;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_postob_cal_field(Postob_cal *postob_cal,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=postob_cal->state;
    break;
  case 2:
    *value=postob_cal->time->value;
    *units=postob_cal->time->units;
    *name=0;
    break;
  case 3:
    *value=postob_cal->name;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_sefd_field(Sefd *sefd,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=sefd->if_id;
    *link=1;
    break;
  case 2:
    *value=sefd->flux->value;
    *units=sefd->flux->units;
    *name=0;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(sefd->params,n-2,link,name,
			       value, units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_sefd_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_site_position_field(Site_position *site_position,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=site_position->x->value;
    *units=site_position->x->units;
    *name=0;
    break;
  case 2:
    *value=site_position->y->value;
    *units=site_position->y->units;
    *name=0;
    break;
  case 3:
    *value=site_position->z->value;
    *units=site_position->z->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_site_velocity_field(Site_velocity *site_velocity,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=site_velocity->x->value;
    *units=site_velocity->x->units;
    *name=0;
    break;
  case 2:
    *value=site_velocity->y->value;
    *units=site_velocity->y->units;
    *name=0;
    break;
  case 3:
    *value=site_velocity->z->value;
    *units=site_velocity->z->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_ocean_load_vert_field(Ocean_load_vert *ocean_load_vert,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=ocean_load_vert->amp->value;
    *units=ocean_load_vert->amp->units;
    *name=0;
    break;
  case 2:
    *value=ocean_load_vert->phase->value;
    *units=ocean_load_vert->phase->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_ocean_load_horiz_field(Ocean_load_horiz *ocean_load_horiz,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=ocean_load_horiz->amp->value;
    *units=ocean_load_horiz->amp->units;
    *name=0;
    break;
  case 2:
    *value=ocean_load_horiz->phase->value;
    *units=ocean_load_horiz->phase->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_source_model_field(Source_model *source_model,int n,int *link,
		 int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=source_model->component->value;
    *units=source_model->component->units;
    *name=0;
    break;
  case 2:
    *value=source_model->band_id;
    *link=1;
    break;
  case 3:
    *value=source_model->flux->value;
    *units=source_model->flux->units;
    *name=0;
    break;
  case 4:
    *value=source_model->majoraxis->value;
    *units=source_model->majoraxis->units;
    *name=0;
    break;
  case 5:
    *value=source_model->ratio->value;
    *units=source_model->ratio->units;
    *name=0;
    break;
  case 6:
    *value=source_model->angle->value;
    *units=source_model->angle->units;
    *name=0;
    break;
  case 7:
    *value=source_model->raoff->value;
    *units=source_model->raoff->units;
    *name=0;
    break;
  case 8:
    *value=source_model->decoff->value;
    *units=source_model->decoff->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_vsn_field(Vsn *vsn,int n,int *link,
	      int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=vsn->drive->value;
    *units=vsn->drive->units;
    *name=0;
    break;
  case 2:
    *value=vsn->label;
    break;
  case 3:
    *value=vsn->start;
    *name=0;
    break;
  case 4:
    *value=vsn->stop;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_fanin_def_field(Fanin_def *fanin_def,int n,int *link,
	      int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=fanin_def->subpass;
    break;
  case 2:
    *value=fanin_def->hdstk->value;
    *units=fanin_def->hdstk->units;
    *name=0;
    break;
  case 3:
    *value=fanin_def->track->value;
    *units=fanin_def->track->units;
    *name=0;
    break;
  default:
    if (n <1)
      return -1;
    ierr=get_bitstream_list_field(fanin_def->bitstreams,n-3,link,name,
			       value, units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error in get_fanin_def_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_bitstream_list_field(Llist *list,int n,int *link,int *name,
		      char **value, char **units)
{
  int i;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  for(i=1; i<=n; i++) {
    if(list==NULL)
      return -1;
    if(i==n) {
      switch(i%2) {
      case 1:
	*value=list->ptr;
	*link=1;
	return 0;
	break;
      case 0:
	*value=list->ptr;
	return 0;
	break;
      default:
	fprintf(stderr,
		"impossible condition 1 in get_bitstream_list_field %d\n",i);
	exit(1);
      }
    }
    list=list->next;
  }
  fprintf(stderr,"impossible condition 2 in get_bitstream_list_field\n");
  return -999;
}
static int
get_fanout_def_field(Fanout_def *fanout_def,int n,int *link,
	      int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=fanout_def->subpass;
    break;
  case 2:
  case 3:
    ierr=get_bitstream_list_field(fanout_def->bitstream,n-1,link,name,
			       value, units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error 1 in get_fanout_def_field %d\n",ierr);
      exit(1);
    }
    break;
  case 4:
    ierr=get_bitstream_list_field(fanout_def->bitstream,n-1,link,name,
			       value, units);
    if(ierr!=-1) {
      fprintf(stderr,"too many bitstreams in get_fanout_def_field\n");
      exit(1);
    } else if (ierr!=0 && ierr!=-1) {
      fprintf(stderr,"unknown error 2 in get_fanout_def_field %d\n",ierr);
      exit(1);
    }
    *value=fanout_def->hdstk->value;
    *units=fanout_def->hdstk->units;
    *name=0;
    break;
  default:
    if(n < 1 )
      return -1;
    ierr=get_dvalue_list_field(fanout_def->tracks,n-4,link,name,
			       value, units);
    if(ierr==-1)
      return -1;
    else if (ierr!=0) {
      fprintf(stderr,"unknown error 3 in get_fanout_def_field %d\n",ierr);
      exit(1);
    }
  }
  return 0;
}
static int
get_vlba_frmtr_sys_trk_field(Vlba_frmtr_sys_trk *vlba_frmtr_sys_trk,int n,
			     int *link, int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=vlba_frmtr_sys_trk->output->value;
    *units=vlba_frmtr_sys_trk->output->units;
    *name=0;
    break;
  case 2:
    *value=vlba_frmtr_sys_trk->use;
    break;
  case 3:
    *value=vlba_frmtr_sys_trk->start->value;
    *units=vlba_frmtr_sys_trk->start->units;
    *name=0;
    break;
  case 4:
    if (vlba_frmtr_sys_trk->stop==NULL)
      return -1;
    *value=vlba_frmtr_sys_trk->stop->value;
    *units=vlba_frmtr_sys_trk->stop->units;
    *name=0;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_s2_data_source_field(S2_data_source *s2_data_source,int n,int *link,
			  int *name, char **value, char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=s2_data_source->source;
    break;
  case 2:
    if(s2_data_source->bbcx_id==NULL)
      return -1;
    *value=s2_data_source->bbcx_id;
    *link=1;
    break;
  case 3:
    if(s2_data_source->bbcx_id==NULL)
      return -1;
    *value=s2_data_source->bbcy_id;
    *link=1;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_dvalue_field(Dvalue *dvalue,int n,int *link,int *name,char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=dvalue->value;
    *units=dvalue->units;
    break;
  default:
    return -1;
  }
  return 0;
}
static int
get_dvalue_list_field(Llist *list,int n,int *link,int *name,
		      char **value, char **units)
{
  int i;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  for(i=1; i<=n; i++) {
    if(list==NULL)
      return -1;
    if(i==n) {
      *value=((Dvalue *) list->ptr)->value;
      *units=((Dvalue *) list->ptr)->units;
      return 0;
    }
    list=list->next;
  }

  fprintf(stderr,"impossible condition in get_dvalue_list_field\n");
  return -999;
}
static int
get_svalue_field(char *svalue,int n,int *link,int *name, char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=1;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=svalue;
    
    break;
  default:
    if(n != 1 )
      return -1;
  }
  return 0;
}
static int
get_svalue_list_field(Llist *list,int n,int *link,int *name,
		      char **value, char **units)
{
  int i;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  for(i=1; i<=n; i++) {
    if(list==NULL)
      return -1;
    if(i==n) {
      *value=(char *) list->ptr;
      return 0;
    }
    list=list->next;
  }
  fprintf(stderr,"impossible condition in get_svalue_list_field\n");
  return -999;
}
static int
get_date_field(char *date,int n,int *link, int *name, char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=date;
    
    break;
  default:
    if(n != 1 )
      return -1;
  }
  return 0;
}
static int
get_time_field(char *time,int n,int *link, int *name, char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=time;
    
    break;
  default:
    if(n != 1 )
      return -1;
  }
  return 0;
}
static int
get_angle_field(char *angle,int n,int *link, int *name, char **value,
		   char **units)
{
  int ierr;

  *link=0;
  *name=0;
  *units=NULL;
  *value=NULL;

  switch(n) {
  case 1:
    *value=angle;
    
    break;
  default:
    if(n != 1 )
      return -1;
  }
  return 0;
}
