/*
 * Copyright (c) 2020 NVI, Inc.
 *
 * This file is part of VLBI Field System
 * (see http://github.com/nvi-inc/fs).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __VEX_H__
#define __VEX_H__

#ifdef __cplusplus
extern "C" {
#endif

/* structure declarations */

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

struct vex_version {
  char *version;
  int lessthan2;
};

typedef struct vex_version Vex_version;

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
  char *chan_name;
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

struct source {
  char *key;
  struct dvalue *center;
  struct dvalue *correlate;
  struct llist *stations;
};

typedef struct source Source;

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

struct intent {
  char *key;
  char *identifier;
  char *value;
};

typedef struct intent Intent;

struct pointing_offset {
  char *key;
  char *coord1;
  struct dvalue *offset1;
  char *coord2;
  struct dvalue *offset2;
};

typedef struct pointing_offset Pointing_offset;

struct axis_type {
  char *axis1;
  char *axis2;
  struct dvalue *orientation;
};

typedef struct axis_type Axis_type;

struct antenna_motion {
  char *axis;
  struct dvalue *rate;
  struct dvalue *offset;
  struct dvalue *acceleration;
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
  char *name;
};

typedef struct pointing_sector Pointing_sector;

struct nasmyth {
  char *band;
  char *platform;
};

typedef struct nasmyth Nasmyth;

struct bbc_assign {
  char *bbc_id;
  struct dvalue *physical;
  char *if_id;
};

typedef struct bbc_assign Bbc_assign;

struct stream_def {
  char *chan_link;
  char *bit;
  struct dvalue *input;
  struct dvalue *ondisk;
  char *bitstream_link;
};

typedef struct stream_def Stream_def;

struct stream_sample_rate {
  struct dvalue *rate;
  char *bitstream_link;
};

typedef struct stream_sample_rate Stream_sample_rate;

struct stream_label {
  char *label;
  char *bitstream_link;
};

typedef struct stream_label Stream_label;

struct clock_early {
  char *start;
  struct dvalue *offset;
  char *origin;
  struct dvalue *rate;
  struct dvalue *accel;
  struct dvalue *jerk;
  struct dvalue *peculiar;
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

struct equip {
  char *type;
  char *device;
  char *link;
  char *label;
};

typedef struct equip Equip;

struct composite_equip {
  char *link;
  struct llist *sub;
};

typedef struct composite_equip Composite_equip;

struct equip_set {
  char *link;
  char *function;
  struct llist *settings;
};

typedef struct equip_set Equip_set;

struct equip_info {
  char *link;
  char *name;
  struct llist *values;
};

typedef struct equip_info Equip_info;

struct connection {
  char *signal_link;
  char *equip_link;
  char *label;
  char *direction;
  char *type;
};

typedef struct connection Connection;

struct record_method {
  char *pattern;
  struct dvalue *early;
  struct dvalue *gap;
};

typedef struct record_method Record_method;

struct datastream {
  char *link;
  char *format;
  char *label;
};

typedef struct datastream Datastream;

struct thread {
  char *datastream_link;
  char *thread_link;
  struct dvalue *number;
  struct dvalue *channels;
  struct dvalue *sample;
  struct dvalue *bits;
  char *type;
  struct dvalue *bytes;
};

typedef struct thread Thread;

struct channel {
  char *datastream_link;
  char *thread_link;
  char *channel_link;
  struct dvalue *number;
};

typedef struct channel Channel; 

struct merged_datastream {
  char *merged_link;
  char *label;
  struct llist *constituent_links;
};

typedef struct merged_datastream Merged_datastream; 

struct eop_origin {
  char *source;
  char *version;
};

typedef struct eop_origin Eop_origin; 

struct nut_origin {
  char *source;
  char *version;
};

typedef struct nut_origin Nut_origin; 

struct exper_name {
  char *name;
  char *segment;
};

typedef struct exper_name Exper_name;
 
struct scheduling_software {
  char *program;
  char *version;
  char *epoch;
};

typedef struct scheduling_software Scheduling_software;
 
struct vex_file_writer {
  char *program;
  char *version;
  char *epoch;
};

typedef struct vex_file_writer Vex_file_writer;
 
struct extension {
  char *owner;
  char *name;
  struct llist *values;
};

typedef struct extension Extension;

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
  struct dvalue *samp_rate;
};

typedef struct if_def If_def; 

struct receiver_name {
  char *link;
  char *name;
};

typedef struct receiver_name Receiver_name;

struct sub_lo_frequencies {
  char *link;
  struct llist *los;
};

typedef struct sub_lo_frequencies Sub_lo_frequencies;

struct sub_lo_sidebands {
  char *link;
  struct llist *sbs;
};

typedef struct sub_lo_sidebands Sub_lo_sidebands;

struct switched_power {
  char *link;
  char *name;
  struct dvalue *frequency;
};

typedef struct switched_power Switched_power;

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

struct site_id {
  char *code2;
  char *code1;
};

typedef struct site_id Site_id;

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

struct source_type {
  char *generic;
  char *experiment;
  char *coordinate;
};

typedef struct source_type Source_type;

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

struct datum {
  char *time;
  char *ra;
  char *dec;
  struct dvalue *ra_rate;
  struct dvalue *dec_rate;
};

typedef struct datum Datum;

struct c_vector {
  char *time;
  struct dvalue *x;
  struct dvalue *y;
  struct dvalue *z;
  struct dvalue *vx;
  struct dvalue *vy;
  struct dvalue *vz;
};

typedef struct c_vector c_Vector;

struct vsn {
  struct dvalue *drive;
  char *label;
  char *start;
  char *stop;
  struct llist *link;
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

struct format_def {
  char* format;
  char* extendedformat;
  struct dvalue* datarate;
};
typedef struct format_def Format_def;

struct thread_def {
   struct dvalue* threadnr;
   struct dvalue* backendnr;
   struct dvalue* recordernr;
   struct dvalue* datarate;
   struct dvalue* numchan;
   struct dvalue* bitspersample;
   char*          format;
   char*          extendedformat;
   struct dvalue* bytesperpacket;
};
typedef struct thread_def Thread_def;

struct channel_def {
  char* chanid;
  struct dvalue* threadnr;
  struct dvalue* channelnr;
};
typedef struct channel_def Channel_def;

/* prototypes */

char *make_version(char *str);
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
struct source  *make_source(char *key, struct dvalue *center,
			      struct dvalue *correlate, struct llist *stations);
struct station  *make_station(char *key, struct dvalue *start,
			      struct dvalue *stop, struct dvalue *start_pos,
			      char *pass, char *sector, struct llist *drives);
struct data_transfer  *make_data_transfer(char *key, char *method,
					  char *destination,
					  struct dvalue *start,
					  struct dvalue *stop, 
					  char *options);
struct intent  *make_intent(char *key, char *identifier,
                  char *value);
struct pointing_offset  *make_pointing_offset(char *key,
                  char *coord1, struct dvalue *offset1,
                  char *coord2, struct dvalue *offset2);
struct axis_type *make_axis_type(char *axis1, char *axis2,
				 struct dvalue *orientation);
struct antenna_motion *make_antenna_motion(char *axis,struct dvalue *rate,
					   struct dvalue *offset,
					   struct dvalue *acceleration);
struct pointing_sector *make_pointing_sector(char *sector, char *axis1,
					     struct dvalue *lolimit1,
					     struct dvalue *hilimit1,
					     char *axis2,
					     struct dvalue *lolimit2,
					     struct dvalue *hilimit2,
                         char *name);
struct nasmyth *make_nasmyth(char *band, char *platform);
struct bbc_assign *make_bbc_assign(char *bbc_id,struct dvalue *physical,
				   char *if_id);
struct stream_def *make_stream_def(char *chan_link,char *bit,
				   struct dvalue *input,
				   struct dvalue *ondisk,
				   char *bitstream_link);
struct stream_sample_rate *make_stream_sample_rate(struct dvalue *rate,
				   char *bitstream_link);
struct stream_label *make_stream_label(char *label,
				   char *bitstream_link);
struct clock_early *make_clock_early(char *start,struct dvalue *offset,
				     char *origin, struct dvalue *rate,
				     struct dvalue *accel, struct dvalue *jerk,
				     struct dvalue *peculiar);
struct headstack *make_headstack(struct dvalue *stack,char *type,
				 struct dvalue *offset);
struct tape_length *make_tape_length(struct dvalue *duration, char *speed,
				     struct dvalue *tapes);
struct tape_motion *make_tape_motion(char *type, struct dvalue *early,
				     struct dvalue *late, struct dvalue *gap);
struct equip *make_equip(char *type, char *device, char *link, char *label);
struct composite_equip *make_composite_equip(char *link, struct llist *sub);
struct equip_set *make_equip_set(char *link, char *function,
				 struct llist *settings);
struct equip_info *make_equip_info(char *link, char *name,
				   struct llist *values);
struct connection *make_connection(char *signal_link, char *equipment_link,
				   char *label, char *direction, char *type);
struct record_method *make_record_method(char *pattern, struct dvalue *early,
				   struct dvalue *gap);
struct datastream *make_datastream(char *link,char *format,
				   char *label);
struct thread *make_thread(char *datastream_link,char *thread_link,
			   struct dvalue *number, struct dvalue *channnels,
			   struct dvalue *sample, struct dvalue *bits,
			   char *type, struct dvalue *bytes);
struct channel *make_channel(char *datastream_link,char *thread_link,
			     char *channel_link, struct dvalue *number);
struct merged_datastream *make_merged_datastream(char *merged_link,
			       char *label, struct llist *constituent_links);

struct eop_origin *make_eop_origin(char *source, char *version);
struct nut_origin *make_nut_origin(char *source, char *version);

struct exper_name *make_exper_name(char *name, char *segment);

struct scheduling_software *make_scheduling_software(char *program,
						     char *version,
						     char *epoch);

struct vex_file_writer *make_vex_file_writer(char *program,
					     char *version,
					     char *epoch);

struct extension *make_extension(char *owner, char *name,
				 struct llist *values);

struct headstack_pos *make_headstack_pos(struct dvalue *index,
					 struct llist *positions);
struct if_def *make_if_def(char *if_id, char *physical, char *polar,
			   struct dvalue *lo, char *sb,
			   struct dvalue *pcal_spacing,
			   struct dvalue *pcal_base,
               struct dvalue *samp_rate);
struct receiver_name *make_receiver_name(char *link, char *name);
struct sub_lo_frequencies *make_sub_lo_frequencies (char *link,
					 struct llist *los);
struct sub_lo_sidebands *make_sub_lo_sidebands (char *link,
					 struct llist *sbs);
struct switched_power *make_switched_power(char *link,char *name,
				   struct dvalue *frequency);
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
struct site_id *make_site_id(char *code2, char *code1);
struct site_position *make_site_position(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z);
struct site_velocity *make_site_velocity(struct dvalue *x, struct dvalue *y,
					 struct dvalue *z);
struct ocean_load_vert *make_ocean_load_vert(struct dvalue *amp,
					     struct dvalue *phase);
struct ocean_load_horiz *make_ocean_load_horiz(struct dvalue *amp,
					       struct dvalue *phase);
struct source_type *make_source_type(char *generic, char *experiment,
                       char *coordinate);
struct source_model *make_source_model(struct dvalue *component,
				       char *band_id, struct dvalue *flux,
				       struct dvalue *majoraxis,
				       struct dvalue *ratio,
				       struct dvalue *angle,
				       struct dvalue *raoff,
				       struct dvalue *decoff);
struct datum *make_datum(char *time, char *ra, char *dec,
                     struct dvalue *ra_rate, struct dvalue *dec_rate);
struct c_vector *make_vector(char *time,
                          struct dvalue *x,
                          struct dvalue *y,
                          struct dvalue *z,
                          struct dvalue *vx,
                          struct dvalue *vy,
                          struct dvalue *vz);
struct vsn *make_vsn(struct dvalue *drive, char *label, char *start,
		     char *stop, struct llist *link);
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

struct format_def* make_format_def(char* format,
				   char* extendedformat,
				   struct dvalue* datarate);

struct thread_def* make_thread_def(
           struct dvalue* threadnr,
				   struct dvalue* backendnr,
				   struct dvalue* recordernr,
				   struct dvalue* datarate,
				   struct dvalue* numchan,
				   struct dvalue* bitspersample,
				   char*          format,
				   char*          extendedformat,
				   struct dvalue* bytesperpacket);

struct channel_def* make_channel_def(char* chanid,
            struct dvalue* threadnr,
            struct dvalue* channelnr);

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
void print_external(struct external *this_);

void print_svalue(char *svalue);

void print_literal_list(struct llist *svalues);

void print_comment(char *comment);
void print_comment_trailing(char *comment_trailing);

char *
get_vex_rev(struct vex *vex_in);

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

void
end_def();

void
create_ref(char *str, char *str2);

void
end_scan();

void
create_stream_def(char *str, char *str2, char *str3, char *str4, char *str5);

void
create_stream_sample_rate(char *str, char *str2, char *str3);

void
create_stream_label(char *str, char *str2);

void
create_dvalue_list(char *str, char *str2);

void
create_svalue_list(char *str);

void
create_lvalue_list(char *str);

void
create_datastream(char *str, char *str2, char *str3);

void
create_thread(char *str, char *str2, char *str3, char *str4, char *str5,
	      char *str6, char *str7, char *str8, char *str9);

void
create_channel(char *str, char *str2, char *str3, char *str4);

void
create_merged_datastream(char *str, char *str2);

void *
get_all_lowl_next();

void *
get_all_lowl(const char *station, const char *mode, int statement,
	     int primitive, struct vex *vex_in);

void *
get_mode_lowl(const char *station_in, const char *mode_in, int statement,
	      int primitive, struct vex *vex_in);
void *
get_mode_lowl_next();

void *
get_station_lowl(const char *station_in, int statement_in,
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
find_def(struct llist *defs,const char *mode);

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
get_scan_source2_next();

void *
get_scan_source2(Llist *lowls_scan_in);

void *
get_scan_intent_next();

void *
get_scan_intent(Llist *lowls_scan_in);

void *
get_scan_pointing_offset_next();

void *
get_scan_pointing_offset(Llist *lowls_scan_in);

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

void
get_all_literals(struct llist *literals, char *str[]);

void *
get_a_literal(struct llist *literals, char **str);

void *
get_next_literal(struct llist *literals);

void *
get_literals(Llist **lowls_literal, char *station_in,
		 struct vex *vex_in);

int vex_open(const char *name, struct vex **vex);

void
create_vex(); /* (int screen_or_file) * zero(0) or one(1) resp.*/

void
create_block(char *str);

void
create_def(char *str);

void
create_qref(char *str, char *str2);

void
create_qref_qualifier(char *str);

void
create_external_ref(char *str, char *str2, char *str3);

void
create_version(char *str);

void
create_scan(char *str);

void
create_comment(char *str, char *str2);

/*---------------------------------------------------------------------------*/
/* SCHEDULE block builders                                                   */
/*---------------------------------------------------------------------------*/
void
create_start(char *str);

void
create_mode(char *str);

void
create_source(char *str);

void
create_source2(char *str, char *str2, char *str3);

void
create_source2_stations(char *str);

void
create_station(char *str, char *str2, char *str3, char *str4,
	       char *str5, char *str6, char *str7, char *str8,
	       char *str9);

void
create_station_drive_list(char *str);

void
create_data_transfer(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6, char *str7, char *str8);

void
create_intent(char *str, char *str2, char *str3);

void
create_pointing_offset(char *str, char *str2, char *str3, char *str4,
	       char *str5, char *str6, char *str7);

/*---------------------------------------------------------------------------*/
/* ANTENNA block builders                                                    */
/*---------------------------------------------------------------------------*/
void
create_antenna_diam(char *str, char *str2);

void
create_axis_type2(char *str, char *str2, char *str3, char *str4);

void
create_axis_type(char *str, char *str2);

void
create_axis_offset(char *str, char *str2);

void
create_antenna_motion(char *str, char *str2, char *str3, char *str4,
		       char *str5);

void
create_antenna_motion2(char *str, char *str2, char *str3, char *str4,
		       char *str5, char *str6, char *str7);

void
create_pointing_sector(char *str, char *str2, char *str3, char *str4,
		       char *str5, char *str6, char *str7, char *str8,
		       char *str9, char *str10, char *str11);

void
create_pointing_sector2(char *str, char *str2, char *str3, char *str4,
		       char *str5, char *str6, char *str7, char *str8,
		       char *str9, char *str10, char *str11, char *str12);
void
create_nasmyth(char *str, char *str2);

/*---------------------------------------------------------------------------*/
/* BBC block builders                                                        */
/*---------------------------------------------------------------------------*/
void
create_bbc_assign(char *str, char *str2, char *str3);
/*---------------------------------------------------------------------------*/
/* CLOCK block builders                                                      */
/*---------------------------------------------------------------------------*/
void
create_clock(char *str, char *str2, char *str3, char *str4, char *str5);
/* wrong name above for backward compatibility, should have been clock_early*/

void
create_clock_early(char *str, char *str2, char *str3, char *str4, char *str5);

void
create_clock_early2(char *str, char *str2, char *str3, char *str4, char *str5,
	     char *str6, char *str7, char *str8, char *str9, char *str10,
	     char *str11,char *str12);
/*---------------------------------------------------------------------------*/
/* DAS block builders                                                        */
/*---------------------------------------------------------------------------*/
void
create_record_transport_type(char *str);

void
create_electronics_rack_type(char *str);

void
create_number_drives(char *str);

void
create_headstack(char *str, char *str2, char *str3);

void
create_record_density(char *str, char *str2);

void
create_tape_length(char *str, char *str2, char *str3, char *str4);

void
create_recording_system_id(char *str);

void
create_tape_motion(char *str, char *str2, char *str3, char *str4,
                   char *str5, char *str6, char *str7);
void
create_tape_control(char *str);

void
create_equip(char *str, char *str2, char *str3, char *str4);

void
create_composite_equip(char *str);

void
create_equip_set(char *str,char *str2);

void
create_equip_info(char *str,char *str2);

void
create_connection(char *str, char *str2, char *str3, char *str4, char *str5);

void
create_record_method(char *str, char *str2, char *str3, char *str4, char *str5);

void
create_record_control(char *str);
/*---------------------------------------------------------------------------*/
/* EOP block builders                                                        */
/*---------------------------------------------------------------------------*/
void
create_tai_utc(char *str, char *str2);

void
create_a1_tai(char *str, char *str2);

void
create_eop_ref_epoch(char *str);

void
create_num_eop_points(char *str);

void
create_eop_interval(char *str, char *str2);

void
create_ut1_utc(char *str, char *str2);

void
create_x_wobble(char *str, char *str2);

void
create_y_wobble(char *str, char *str2);

void
create_nut_ref_epoch(char *str);

void
create_num_nut_points(char *str);

void
create_nut_interval(char *str, char *str2);

void
create_delta_psi(char *str, char *str2);

void
create_delta_eps(char *str, char *str2);

void
create_nut_model(char *str);

void
create_eop_origin(char *str, char *str2);

void
create_delta_x_nut();

void
create_delta_y_nut();

void
create_nut_origin(char *str, char *str2);
/*---------------------------------------------------------------------------*/
/* EXPER block builders                                                      */
/*---------------------------------------------------------------------------*/
void
create_exper_num(char *str);

void
create_exper_name(char *str);

void
create_exper_name2(char *str, char *str2);

void
create_exper_description(char *str);

void
create_exper_nominal_start(char *str);

void
create_exper_nominal_stop(char *str);

void
create_pi_name(char *str);

void
create_pi_email(char *str);

void
create_contact_name(char *str);

void
create_contact_email(char *str);

void
create_scheduler_name(char *str);

void
create_scheduler_email(char *str);

void
create_target_correlator(char *str);

void
create_scheduling_software(char *str, char *str2, char *str3);

void
create_vex_file_writer(char *str, char *str2, char *str3);

/*---------------------------------------------------------------------------*/
/* EXTENSION block builders */
/*---------------------------------------------------------------------------*/
void
create_extension(char *str,char *str2);

/*---------------------------------------------------------------------------*/
/* FREQ block builders */
/*---------------------------------------------------------------------------*/
void
create_chan_def(char *str, char *str2, char *str3, char *str4,
		char *str5, char *str6, char *str7, char *str8,
		char *str9);

void
create_chan_def_states(char *str);

void
create_chan_def2(char *str, char *str2, char *str3, char *str4,
		char *str5, char *str6, char *str7, char *str8,
		char *str9, char *str10);

void
create_chan_def2_states(char *str);

void
create_sample_rate(char *str, char *str2);

void
create_bits_per_sample(char *str);

void
create_switching_cycle(char *str);

void
create_cycle(char *str, char *str2);

/*-------------------------------------------------------------------*/
/* HEAD_POS block builders                                           */
/*-------------------------------------------------------------------*/
void
create_headstack_reference(char *str);

void
create_headstack_pos(char *str, char *str2);
/*-------------------------------------------------------------------*/
/* IF block builders                                                 */
/*-------------------------------------------------------------------*/
void
create_if_def(char *str, char *str2, char *str3, char *str4,
	      char *str5, char *str6, char *str7, char *str8,
	      char *str9, char *str10);
void
create_if_def2(char *str, char *str3, char *str4,
	      char *str5, char *str6, char *str7, char *str8,
	      char *str9, char *str10, char *str11, char *str12);
void
create_receiver_name(char *str, char *str2);
void
create_sub_lo_frequencies(char *str);
void
create_sub_lo_sidebands(char *str);
void
create_switched_power(char *str, char *str2, char *str3, char *str4);
/*-------------------------------------------------------------------*/
/* PASS_ORDER block builders                                         */
/*-------------------------------------------------------------------*/
void
create_pass_order(char *str);

void
create_s2_group_order(char *str);
/*-------------------------------------------------------------------*/
/* PHASE_CAL_DETECT block builders                                   */
/*-------------------------------------------------------------------*/
void
create_phase_cal_detect(char *str);

void
create_phase_cal_detect_list(char *str);
/*-------------------------------------------------------------------*/
/* PROCEDURE block builders                                          */
/*-------------------------------------------------------------------*/
void
create_tape_change(char *str, char *str2);

void
create_headstack_motion(char *str, char *str2);

void
create_new_source_command(char *str, char *str2);

void
create_new_tape_setup(char *str, char *str2);

void
create_setup_always(char *str, char *str2, char *str3);

void
create_parity_check(char *str, char *str2, char *str3);

void
create_tape_prepass(char *str, char *str2, char *str3);

void
create_preob_cal(char *str, char *str2, char *str3, char *str4);

void
create_midob_cal(char *str, char *str2, char *str3, char *str4);

void
create_postob_cal(char *str, char *str2, char *str3, char *str4);

void
create_procedure_name_prefix(char *str);
/*-------------------------------------------------------------------*/
/* ROLL block builders                                               */
/*-------------------------------------------------------------------*/
void
create_roll(char *str);

void
create_roll_reinit_period(char *str, char *str2);

void
create_roll_inc_period(char *str);

void
create_roll_def(char *str);
/*-------------------------------------------------------------------*/
/* SCHEDULING_PARAMS block builders     using literals               */
/*-------------------------------------------------------------------*/
void
create_literal(char *str);
/*-------------------------------------------------------------------*/
/* SEFD_MODEL block builders                                         */
/*-------------------------------------------------------------------*/
void
create_sefd_model(char *str);

void
create_sefd(char *str, char *str2, char *str3);

void
create_sefd_model_parameter(char *str);
/*-------------------------------------------------------------------*/
/* SITE block builders                                               */
/*-------------------------------------------------------------------*/
void
create_site_name(char *str);

void
create_site_type(char *str);

void
create_site_ID(char *str);

void
create_site_ID2(char *str, char *str2);

void
create_site_position(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6);

void
create_site_position_epoch(char *str);

void
create_site_position_ref(char *str);

void
create_site_velocity(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6);

void
create_horizon_map(char *str, char *str2);

void
create_horizon_map_az();

void
create_horizon_map_el();

void
create_zen_atmos(char *str, char *str2);

void
create_ocean_load_vert(char *str, char *str2, char *str3, char *str4);

void
create_ocean_load_horiz(char *str, char *str2, char *str3, char *str4);

void
create_occupation_code(char *str);

void
create_inclination(char *str, char *str2);

void
create_eccentricity(char *str);

void
create_arg_perigee(char *str, char *str2);

void
create_ascending_node(char *str, char *str2);

void
create_mean_anomaly(char *str, char *str2);

void
create_semi_major_axis(char *str, char *str2);

void
create_mean_motion(char *str);

void
create_orbit_epoch(char *str);
/*-------------------------------------------------------------------*/
/* SOURCE block builders                                             */
/*-------------------------------------------------------------------*/
void
create_source_name(char *str);

void
create_ra(char *str);

void
create_IAU_name(char *str);

void
create_dec(char *str);

void
create_ref_coord_frame(char *str);

void
create_source_position_ref(char *str);

void
create_source_position_epoch(char *str);

void
create_ra_rate(char *str, char *str2);

void
create_dec_rate(char *str, char *str2);

void
create_velocity_wrt_LSR(char *str, char *str2);

void
create_source_type(char *str, char *str2);

void
create_source_type2(char *str, char *str2, char *str3);

void
create_source_model(char *str, char *str2, char *str3, char *str4,
		    char *str5, char *str6, char *str7, char *str8,
		    char *str9, char *str10, char *str11, char *str12,
		    char *str13);

void
create_bsp_file_name(char *str);

void
create_bsp_object_id(char *str);

void
create_tle0(char *str);

void
create_tle1(char *str);

void
create_tle2(char *str);

void
create_datum(char *str, char *str2, char *str3, char *str4, char *str5,
        char *str6, char *str7);

void
create_vector(char *str, char *str2, char *str3, char *str4, char *str5,
        char *str6, char *str7, char *str8, char *str9, char *str10,
        char *str11, char *str12, char *str13);
/*-------------------------------------------------------------------*/
/* TAPELOG_OBS block builders                                        */
/*-------------------------------------------------------------------*/
void
create_vsn(char *str, char *str2, char *str3, char *str4);
void
create_vsn2(char *str, char *str2, char *str3, char *str4);
/*-------------------------------------------------------------------*/
/* TRACKS block builders                                             */
/*-------------------------------------------------------------------*/
void
create_fanin_def(char *str, char *str2, char *str3);

void
create_fanin_def_list(char *str);

void
create_fanout_def_subpass(char *str);

void
create_fanout_def_headstack(char *str);

void
create_fanout_trksID_list(char *str);

void
create_fanout_bitstream_list(char *str);

void
create_track_frame_format(char *str);

void
create_data_modulation(char *str);

void
create_vlba_frmtr_sys_trk(char *str, char *str2, char *str3, char *str4);

void
create_vlba_trnsprt_sys_trk(char *str, char *str2);

void
create_s2_recording_mode(char *str);

void
create_s2_data_source(char *str, char *str2, char *str3);

/*--------------------------- TEST PILOT ----------------------------*/
void *
create_test(struct llist *start, char *str);

extern FILE *fp;
extern char *filename;

#ifdef __cplusplus
}
#endif

#endif
