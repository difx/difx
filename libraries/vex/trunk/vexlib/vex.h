#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */

int yylex();
void yyerror(const char*);

/* structure declarations */
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

struct llist {
  struct llist *next;
  void *ptr;
};

typedef struct llist Llist;

struct vex {
  struct llist *version;
  struct llist *blocks;
};

typedef struct vex Vex;

struct block {
  int block;
  struct llist *items;
};

struct qref {
  int primitive;
  char *name;
  struct llist *qualifiers;
};

typedef struct qref Qref;

struct def {
  char *name;
  struct llist *refs;
};

typedef struct def Def;

struct dvalue {
  char *value;
  char *units;
};

typedef struct dvalue Dvalue;

struct lowl {
  int statement;
  void *item;
};

typedef struct lowl Lowl;

struct chan_def {
  char *band_id;
  struct dvalue *sky_freq;
  char *net_sb;
  struct dvalue *bw;
  char *chan_id;
  char *bbc_id;
  char *pcal_id;
  struct llist *states;
};

typedef struct chan_def Chan_def;

struct external {
  char *file;
  int primitive;
  char *name;
};

struct switching_cycle {
  char *origin;
  struct llist *periods;
};

typedef struct switching_cycle Switching_cycle;

struct station {
  char *key;
  struct dvalue *start;
  struct dvalue *stop;
  struct dvalue *start_pos;
  char *pass;
  char *sector;
  struct llist *drives;
};
	
typedef struct station Station;

struct data_transfer {
  char *key;
  char *method;
  char *destination;
  struct dvalue *start;
  struct dvalue *stop;
  char *options;
};
	
typedef struct data_transfer Data_transfer;

struct axis_type {
  char *axis1;
  char *axis2;
};

typedef struct axis_type Axis_type;

struct antenna_motion {
  char *axis;
  struct dvalue *rate;
  struct dvalue *offset;
};

typedef struct antenna_motion Antenna_motion;

struct pointing_sector {
  char *sector;
  char *axis1;
  struct dvalue *lolimit1;
  struct dvalue *hilimit1;
  char *axis2;
  struct dvalue *lolimit2;
  struct dvalue *hilimit2;
};

typedef struct pointing_sector Pointing_sector;

struct bbc_assign {
  char *bbc_id;
  struct dvalue *physical;
  char *if_id;
};

typedef struct bbc_assign Bbc_assign;

struct clock_early {
  char *start;
  struct dvalue *offset;
  char *origin;
  struct dvalue *rate;
};

typedef struct clock_early Clock_early;

struct headstack {
  struct dvalue *stack;
  char *type;
  struct dvalue *offset;
};

typedef struct headstack Headstack;

struct s2_data_source {
  char *source;
  char *bbcx_id;
  char *bbcy_id;
};

typedef struct s2_data_source S2_data_source;

struct tape_length {
  struct dvalue *duration;
  char *speed;
  struct dvalue *tapes;
};

typedef struct tape_length Tape_Length;

struct tape_motion {
  char *type;
  struct dvalue *early;
  struct dvalue *late;
  struct dvalue *gap;
};

typedef struct tape_motion Tape_Motion;

struct headstack_pos {
  struct dvalue *index;
  struct llist *positions;
};

typedef struct headstack_pos Headstack_pos; 

struct if_def {
  char *if_id;
  char *physical;
  char *polar;
  struct dvalue *lo;
  char *sb;
  struct dvalue *pcal_spacing;
  struct dvalue *pcal_base;

  char *comment;
};

typedef struct if_def If_def; 

struct phase_cal_detect {
  char *pcal_id;
  struct llist *tones;
};

typedef struct phase_cal_detect Phase_cal_detect; 

struct setup_always {
  char *state;
  struct dvalue *time;
};

typedef struct setup_always Setup_always; 

struct parity_check {
  char *state;
  struct dvalue *time;
};

typedef struct parity_check Parity_check; 

struct tape_prepass {
  char *state;
  struct dvalue *time;
};

typedef struct tape_prepass Tape_prepass; 

struct preob_cal {
  char *state;
  struct dvalue *time;
  char *name;
};

typedef struct preob_cal Preob_cal; 

struct midob_cal {
  char *state;
  struct dvalue *time;
  char *name;
};

typedef struct midob_cal Midob_cal; 

struct postob_cal {
  char *state;
  struct dvalue *time;
  char *name;
};

typedef struct postob_cal Postob_cal; 

struct sefd {
  char *if_id;
  struct dvalue *flux;
  struct llist *params;
};

typedef struct sefd Sefd;

struct site_position {
  struct dvalue *x;
  struct dvalue *y;
  struct dvalue *z;
};

typedef struct site_position Site_position;

struct site_velocity {
  struct dvalue *x;
  struct dvalue *y;
  struct dvalue *z;
};

typedef struct site_velocity Site_velocity;

struct ocean_load_vert {
  struct dvalue *amp;
  struct dvalue *phase;
};

typedef struct ocean_load_vert Ocean_load_vert;

struct ocean_load_horiz {
  struct dvalue *amp;
  struct dvalue *phase;
};

typedef struct ocean_load_horiz Ocean_load_horiz;

struct source_model {
  struct dvalue *component;
  char *band_id;
  struct dvalue *flux;
  struct dvalue *majoraxis;
  struct dvalue *ratio;
  struct dvalue *angle;
  struct dvalue *raoff;
  struct dvalue *decoff;
};

typedef struct source_model Source_model;

struct vsn {
  struct dvalue *drive;
  char *label;
  char *start;
  char *stop;
};

typedef struct vsn Vsn;

struct fanin_def {
  char *subpass;
  struct dvalue *hdstk;
  struct dvalue *track;
  struct llist *bitstreams;
};

typedef struct fanin_def Fanin_def;

struct fanout_def {
  char *subpass;
  struct llist *bitstream;
  struct dvalue *hdstk;
  struct llist *tracks;
};

typedef struct fanout_def Fanout_def;

struct vlba_frmtr_sys_trk {
  struct dvalue *output;
  char *use;
  struct dvalue *start;
  struct dvalue *stop;
};

typedef struct vlba_frmtr_sys_trk Vlba_frmtr_sys_trk;

/* prototypes */

struct llist     *add_list(struct llist *start,void *ptr);
struct llist     *ins_list(void *ptr, struct llist *start);
struct qref      *make_qref(int primitive,char *name,struct llist *qualifiers);
struct def       *make_def(char *name, struct llist *refs);
struct block     *make_block(int block,struct llist *items);
struct vex *make_vex(struct llist *version, struct llist *blocks);
struct lowl 	 *make_lowl(int statement,void *items);
struct chan_def  *make_chan_def(char *band_id, struct dvalue *sky_freq,
				char *net_sb, struct dvalue *bw,
				char *chan_id, char *bbc_id, char *pcal_id,
				struct llist *states);
struct dvalue *make_dvalue(char *value, char *units);
struct external *make_external(char *file, int primitive, char *name);
struct switching_cycle *make_switching_cycle(char *origin,
					     struct llist *periods);
struct station  *make_station(char *key, struct dvalue *start,
			      struct dvalue *stop, struct dvalue *start_pos,
			      char *pass, char *sector, struct llist *drives);
struct data_transfer  *make_data_transfer(char *key, char *method,
					  char *destination,
					  struct dvalue *start,
					  struct dvalue *stop, 
					  char *options);
struct axis_type *make_axis_type(char *axis1, char *axis2);
struct antenna_motion *make_antenna_motion(char *axis,struct dvalue *rate,
					   struct dvalue *offset); 
struct pointing_sector *make_pointing_sector(char *sector, char *axis1,
					     struct dvalue *lolimit1,
					     struct dvalue *hilimit1,
					     char *axis2,
					     struct dvalue *lolimit2,
					     struct dvalue *hilimit2);
struct bbc_assign *make_bbc_assign(char *bbc_id,struct dvalue *physical,
				   char *if_id);
struct clock_early *make_clock_early(char *start,struct dvalue *offset,
				     char *origin, struct dvalue *rate);
struct headstack *make_headstack(struct dvalue *stack,char *type,
				 struct dvalue *offset);
struct tape_length *make_tape_length(struct dvalue *duration, char *speed,
				     struct dvalue *tapes);
struct tape_motion *make_tape_motion(char *type, struct dvalue *early,
				     struct dvalue *late, struct dvalue *gap);
struct headstack_pos *make_headstack_pos(struct dvalue *index,
					 struct llist *positions);
struct if_def *make_if_def(char *if_id, char *physical, char *polar,
			   struct dvalue *lo, char *sb,
			   struct dvalue *pcal_spacing,
			   struct dvalue *pcal_base,
			   char *comment);
struct phase_cal_detect *make_phase_cal_detect(char *pcal_id,
					       struct llist *tones);
struct setup_always *make_setup_always(char *state, struct dvalue *time);
struct parity_check *make_parity_check(char *state, struct dvalue *time);
struct tape_prepass *make_tape_prepass(char *state, struct dvalue *time);
struct preob_cal *make_preob_cal(char *state, struct dvalue *time,
				 char *name);
struct midob_cal *make_midob_cal(char *state, struct dvalue *time,
				 char *name);
struct postob_cal *make_postob_cal(char *state, struct dvalue *time,
				 char *name);
struct sefd *make_sefd(char *if_id, struct dvalue *flux, struct llist *params);
struct site_position *make_site_position(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z);
struct site_velocity *make_site_velocity(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z);
struct ocean_load_vert *make_ocean_load_vert(struct dvalue *amp,
					     struct dvalue *phase);
struct ocean_load_horiz *make_ocean_load_horiz(struct dvalue *amp,
					       struct dvalue *phase);
struct source_model *make_source_model(struct dvalue *component,
				       char *band_id, struct dvalue *flux,
				       struct dvalue *majoraxis,
				       struct dvalue *ratio,
				       struct dvalue *angle,
				       struct dvalue *raoff,
				       struct dvalue *decoff);
struct vsn *make_vsn(struct dvalue *drive, char *label, char *start,
		     char *stop);
struct fanin_def *make_fanin_def(char *subpass, struct dvalue *hdstk,
				 struct dvalue *track,
				 struct llist *bitstreams);
struct fanout_def *make_fanout_def(char *subpass, struct llist *bitstream,
				   struct dvalue *hdstk, struct llist *tracks);
struct vlba_frmtr_sys_trk *make_vlba_frmtr_sys_trk(struct dvalue *output,
						   char *use,
						   struct dvalue *start,
						   struct dvalue *stop);
struct s2_data_source *make_s2_data_source(char *source,char *bbcx_id,
					   char *bbcy_id);

int
lowl2int(char *lowl);

int
block2int(char *block);

char *
int2lowl(int lowl);

char *
int2block(int block);

int
vex_field(int statement,void *ptr,int i,int *link,int *name, char **value,
	  char **units);

void print_vex(struct vex *vex);
void print_vex_blocks(struct llist *blocks);
void print_block_name(int block);
void print_qref_block(struct llist *items);
void print_qualifiers(struct llist *items);
void print_lowl(struct llist *items);
void print_lowl_st(int statement, void *ptr);

void print_def_block(struct llist *items, void func());
void print_external(struct external *this);

void print_svalue(char *svalue);

void print_literal_list(struct llist *svalues);

void print_comment(char *comment);
void print_comment_trailing(char *comment_trailing);

char *
get_source_def_next();

char *
get_source_def(struct vex *vex_in);

char *
get_mode_def_next();

char *
get_mode_def(struct vex *vex_in);

char *
get_station_def_next();

char *
get_station_def(struct vex *vex_in);

void *
get_all_lowl_next();

void *
get_all_lowl(char *station, char *mode, int statement,
	     int primitive, struct vex *vex_in);

void *
get_mode_lowl(char *station_in, char *mode_in, int statement,
	      int primitive, struct vex *vex_in);
void *
get_mode_lowl_next();

void *
get_station_lowl(char *station_in, int statement_in,
	      int primitive_in, struct vex *vex_in);

void *
get_station_lowl_next();

void *
get_source_lowl(char *source_in, int statement_in, struct vex *vex_in);

void *
get_source_lowl_next();

void *
get_global_lowl(int primitive_in, int statement_in, struct vex *vex_in);

void *
get_global_lowl_next();

struct llist *
find_block(int block,struct vex *vex);

struct llist *
find_def(struct llist *defs,char *mode);

struct llist *
find_lowl(struct llist *lowls,int statement);

void *
get_scan_start(Llist *lowls);

void *
get_scan_mode(Llist *lowls);

void *
get_scan_source_next();

void *
get_scan_source(Llist *lowls_scan_in);

void *
get_scan_station_next(Llist **lowls_scan, char **scanid);

void *
get_scan_station(Llist **lowls_scan, char **scanid, char *station_in,
		 struct vex *vex_in);

void *
get_scan_next(char **scanid);

void *
get_scan(char **scanid, struct vex *vex_in);

void *
get_station_scan_next();

void *
get_station_scan(Llist *lowls_in);

void *
get_scan_data_transfer_next(Llist **lowls_scan, char **scanid);

void *
get_scan_data_transfer(Llist **lowls_scan, char **scanid,
		       char *data_transfer_in, struct vex *vex_in);
void *
get_data_transfer_scan_next();

void *
get_data_transfer_scan(Llist *lowls_in);

Llist *
find_next_def(Llist *defs);

Llist *
find_next_scan(Llist *defs);

void *
get_literal_lowl(char *source_in, struct vex *vex_in);

void *
get_literal_lowl_next();

char *
get_literal_def_next();

char *
get_literal_def(struct vex *vex_in);

Llist *
find_literal(Llist *defs);

char *
get_all_literals(struct llist *literals, char *str[]);

void *
get_a_literal(struct llist *literals, char **str);

void *
get_next_literal(struct llist *literals);

void *
get_literals(Llist **lowls_literal, char *station_in,
		 struct vex *vex_in);

int vex_open(char *name, struct vex **vex);

void *
create_vex(); /* (int screen_or_file) * zero(0) or one(1) resp.*/

void *
create_block(char *str);

void *
create_def(char *str);

void *
create_qref(char *str, char *str2);

void *
create_qref_qualifier(char *str);

void *
create_external_ref(char *str, char *str2, char *str3);

void *
create_version(char *str);

void *
create_scan(char *str);

void *
create_comment(char *str, char *str2);

void *
create_list(char *str);

void *
create_list_value(char *str);

void *
create_dlist(char *str, char *str2);

void *
create(char *key, char *str);

void *
create_value(char *key, char *str);

void *
create_unit_value(char *key, char *str, char *str2);
/*---------------------------------------------------------------------------*/
/* SCHEDULE block builders                                                   */
/*---------------------------------------------------------------------------*/
void *
create_start(char *str);

void *
create_mode(char *str);

void *
create_source(char *str);

void *
create_scan_list(char *str, char *str2);

void *
create_station(char *str, char *str2, char *str3, char *str4,
	       char *str5, char *str6, char *str7, char *str8,
	       char *str9);

void *
create_station_drive_list(char *str);

void *
create_data_transfer(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6, char *str7, char *str8);

/*---------------------------------------------------------------------------*/
/* ANTENNA block builders                                                    */
/*---------------------------------------------------------------------------*/
void *
create_antenna_diam(char *str, char *str2);

void *
create_axis_type(char *str, char *str2);

void *
create_axis_offset(char *str, char *str2);

void *
create_antenna_motion(char *str, char *str2, char *str3, char *str4,
		       char *str5);

void *
create_pointing_sector(char *str, char *str2, char *str3, char *str4,
		       char *str5, char *str6, char *str7, char *str8,
		       char *str9, char *str10, char *str11);
/*---------------------------------------------------------------------------*/
/* BBC block builders                                                        */
/*---------------------------------------------------------------------------*/
void *
create_bbc_assign(char *str, char *str2, char *str3);
/*---------------------------------------------------------------------------*/
/* CLOCK block builders                                                      */
/*---------------------------------------------------------------------------*/
void *
create_clock(char *str, char *str2, char *str3, char *str4, char *str5);
/*---------------------------------------------------------------------------*/
/* DAS block builders                                                        */
/*---------------------------------------------------------------------------*/
void *
create_record_transport_type(char *str);

void *
create_electronics_rack_type(char *str);

void *
create_number_drives(char *str);

void *
create_headstack(char *str, char *str2, char *str3);

void *
create_record_density(char *str, char *str2);

void *
create_tape_length(char *str, char *str2, char *str3, char *str4);

void *
create_recording_system_id(char *str);

void *
create_tape_motion(char *str, char *str2, char *str3, char *str4,
                   char *str5, char *str6, char *str7);
void *
create_tape_control(char *str);
/*---------------------------------------------------------------------------*/
/* EOP block builders                                                        */
/*---------------------------------------------------------------------------*/
void *
create_tai_utc(char *str, char *str2);

void *
create_a1_tai(char *str, char *str2);

void *
create_eop_ref_epoch(char *str);

void *
create_num_eop_points(char *str);

void *
create_eop_interval(char *str, char *str2);

void *
create_ut1_utc(char *str, char *str2);

void *
create_x_wobble(char *str, char *str2);

void *
create_y_wobble(char *str, char *str2);

void *
create_nut_ref_epoch(char *str);

void *
create_num_nut_points(char *str);

void *
create_nut_interval(char *str, char *str2);

void *
create_delta_psi(char *str, char *str2);

void *
create_delta_eps(char *str, char *str2);

void *
create_nut_model(char *str);
/*---------------------------------------------------------------------------*/
/* EXPER block builders                                                      */
/*---------------------------------------------------------------------------*/
void *
create_exper_num(char *str);

void *
create_exper_name(char *str);

void *
create_exper_description(char *str);

void *
create_exper_nominal_start(char *str);

void *
create_exper_nominal_stop(char *str);

void *
create_pi_name(char *str);

void *
create_pi_email(char *str);

void *
create_contact_name(char *str);

void *
create_contact_email(char *str);

void *
create_scheduler_name(char *str);

void *
create_scheduler_email(char *str);

void *
create_target_correlator(char *str);
/*---------------------------------------------------------------------------*/
/* FREQ block builders */
/*---------------------------------------------------------------------------*/
void *
create_chan_def(char *str, char *str2, char *str3, char *str4,
		char *str5, char *str6, char *str7, char *str8,
		char *str9);

void *
create_chan_def_states(char *str);

void *
create_sample_rate(char *str, char *str2);

void *
create_bits_per_sample(char *str);

void *
create_switching_cycle(char *str);

void *
create_cycle(char *str, char *str2);

/*---------------------------------------------------------------------------*/
/* SITE block builders */
/*---------------------------------------------------------------------------*/
void *
create_site_station(char *str, char *str2, char *str3, char *str4,
		    char *str5, char *str6);

void *
create_list_value2(char *str);

void *
create_unit_value2(char *key, char *str, char *str2);

/*-------------------------------------------------------------------*/
/* HEAD_POS block builders                                           */
/*-------------------------------------------------------------------*/
void *
create_headstack_reference(char *str);

void *
create_position(char *str, char *str2);
/*-------------------------------------------------------------------*/
/* IF block builders                                                 */
/*-------------------------------------------------------------------*/
void *
create_if_def(char *str, char *str2, char *str3, char *str4,
	      char *str5, char *str6, char *str7, char *str8,
	      char *str9, char *str10, char *str11);
/*-------------------------------------------------------------------*/
/* PASS_ORDER block builders                                         */
/*-------------------------------------------------------------------*/
void *
create_pass_order(char *str);

void *
create_s2_group_order(char *str);
/*-------------------------------------------------------------------*/
/* PHASE_CAL_DETECT block builders                                   */
/*-------------------------------------------------------------------*/
void *
create_phase_cal_detect(char *str);

void *
create_phase_cal_detect_list(char *str);
/*-------------------------------------------------------------------*/
/* PROCEDURE block builders                                          */
/*-------------------------------------------------------------------*/
void *
create_tape_change(char *str, char *str2);

void *
create_headstack_motion(char *str, char *str2);

void *
create_new_source_command(char *str, char *str2);

void *
create_new_tape_setup(char *str, char *str2);

void *
create_setup_always(char *str, char *str2, char *str3);

void *
create_parity_check(char *str, char *str2, char *str3);

void *
create_tape_prepass(char *str, char *str2, char *str3);

void *
create_preob_cal(char *str, char *str2, char *str3, char *str4);

void *
create_midob_cal(char *str, char *str2, char *str3, char *str4);

void *
create_postob_cal(char *str, char *str2, char *str3, char *str4);

void *
create_procedure_name_prefix(char *str);
/*-------------------------------------------------------------------*/
/* ROLL block builders                                               */
/*-------------------------------------------------------------------*/
void *
create_roll(char *str);

void *
create_roll_reinit_period(char *str, char *str2);

void *
create_roll_inc_period(char *str);

void *
create_roll_def(char *str);
/*-------------------------------------------------------------------*/
/* SCHEDULING_PARAMS block builders     using literals               */
/*-------------------------------------------------------------------*/
void *
create_literal(char *str);
/*-------------------------------------------------------------------*/
/* SEFD_MODEL block builders                                         */
/*-------------------------------------------------------------------*/
void *
create_sefd_model(char *str);

void *
create_sefd(char *str, char *str2, char *str3);

void *
create_sefd_model_parameter(char *str);
/*-------------------------------------------------------------------*/
/* SITE block builders                                               */
/*-------------------------------------------------------------------*/
void *
create_site_name(char *str);

void *
create_site_type(char *str);

void *
create_site_ID(char *str);

void *
create_site_position(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6);

void *
create_site_position_epoch(char *str);

void *
create_site_position_ref(char *str);

void *
create_site_velocity(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6);

void *
create_horizon_map(char *str, char *str2);

void *
create_horizon_map_az();

void *
create_horizon_map_el();

void *
create_horizon_map_list(char *str);

void *
create_zen_atmos(char *str, char *str2);

void *
create_ocean_load_vert(char *str, char *str2, char *str3, char *str4);

void *
create_ocean_load_horiz(char *str, char *str2, char *str3, char *str4);

void *
create_occupation_code(char *str);

void *
create_inclination(char *str, char *str2);

void *
create_eccentricity(char *str);

void *
create_arg_perigee(char *str, char *str2);

void *
create_ascending_node(char *str, char *str2);

void *
create_mean_anomaly(char *str, char *str2);

void *
create_semi_major_axis(char *str, char *str2);

void *
create_mean_motion(char *str);

void *
create_orbit_epoch(char *str);
/*-------------------------------------------------------------------*/
/* SOURCE block builders                                             */
/*-------------------------------------------------------------------*/
void *
create_source_type(char *str, char *str2);

void *
create_source_name(char *str);

void *
create_ra(char *str);

void *
create_IAU_name(char *str);

void *
create_dec(char *str);

void *
create_ref_coord_frame(char *str);

void *
create_source_position_ref(char *str);

void *
create_source_position_epoch(char *str);

void *
create_ra_rate(char *str, char *str2);

void *
create_dec_rate(char *str, char *str2);

void *
create_velocity_wrt_LSR(char *str, char *str2);

void *
create_source_model(char *str, char *str2, char *str3, char *str4,
		    char *str5, char *str6, char *str7, char *str8,
		    char *str9, char *str10, char *str11, char *str12,
		    char *str13);
/*-------------------------------------------------------------------*/
/* TAPELOG_OBS block builders                                        */
/*-------------------------------------------------------------------*/
void *
create_vsn(char *str, char *str2, char *str3, char *str4);
/*-------------------------------------------------------------------*/
/* TRACKS block builders                                             */
/*-------------------------------------------------------------------*/
void *
create_fanin_def(char *str, char *str2, char *str3);

void *
create_fanin_def_list(char *str);

void *
create_fanout_def_subpass(char *str);

void *
create_fanout_def_headstack(char *str);

void *
create_fanout_trksID_list(char *str);

/*void *
create_fanout_bitstream_list(char *str, char *str2);*/
void *
create_fanout_bitstream_list(char *str);

void *
create_track_frame_format(char *str);

void *
create_data_modulation(char *str);

void *
create_vlba_frmtr_sys_trk(char *str, char *str2, char *str3, char *str4);

void *
create_vlba_trnsprt_sys_trk(char *str, char *str2);

void *
create_s2_recording_mode(char *str);

void *
create_s2_data_source(char *str, char *str2, char *str3);

void * 
end_def();

void *
create_ref(char *str, char *str2);

void *
end_scan();

void *
create_headstack_pos(char *str, char *str2);

/*--------------------------- TEST PILOT ----------------------------*/
void *
create_test(struct llist *start, char *str);

FILE *fp;
char *filename;

