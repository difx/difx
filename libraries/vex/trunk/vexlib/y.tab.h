/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_VEX_REV = 258,
     T_REF = 259,
     T_DEF = 260,
     T_ENDDEF = 261,
     T_SCAN = 262,
     T_ENDSCAN = 263,
     T_CHAN_DEF = 264,
     T_SAMPLE_RATE = 265,
     T_BITS_PER_SAMPLE = 266,
     T_SWITCHING_CYCLE = 267,
     T_START = 268,
     T_SOURCE = 269,
     T_MODE = 270,
     T_STATION = 271,
     T_DATA_TRANSFER = 272,
     T_ANTENNA_DIAM = 273,
     T_AXIS_OFFSET = 274,
     T_ANTENNA_MOTION = 275,
     T_POINTING_SECTOR = 276,
     T_AXIS_TYPE = 277,
     T_BBC_ASSIGN = 278,
     T_CLOCK_EARLY = 279,
     T_RECORD_TRANSPORT_TYPE = 280,
     T_ELECTRONICS_RACK_TYPE = 281,
     T_NUMBER_DRIVES = 282,
     T_HEADSTACK = 283,
     T_RECORD_DENSITY = 284,
     T_TAPE_LENGTH = 285,
     T_RECORDING_SYSTEM_ID = 286,
     T_TAPE_MOTION = 287,
     T_TAPE_CONTROL = 288,
     T_TAI_UTC = 289,
     T_A1_TAI = 290,
     T_EOP_REF_EPOCH = 291,
     T_NUM_EOP_POINTS = 292,
     T_EOP_INTERVAL = 293,
     T_UT1_UTC = 294,
     T_X_WOBBLE = 295,
     T_Y_WOBBLE = 296,
     T_NUT_REF_EPOCH = 297,
     T_NUM_NUT_POINTS = 298,
     T_NUT_INTERVAL = 299,
     T_DELTA_PSI = 300,
     T_DELTA_EPS = 301,
     T_NUT_MODEL = 302,
     T_EXPER_NUM = 303,
     T_EXPER_NAME = 304,
     T_EXPER_NOMINAL_START = 305,
     T_EXPER_NOMINAL_STOP = 306,
     T_PI_NAME = 307,
     T_PI_EMAIL = 308,
     T_CONTACT_NAME = 309,
     T_CONTACT_EMAIL = 310,
     T_SCHEDULER_NAME = 311,
     T_SCHEDULER_EMAIL = 312,
     T_TARGET_CORRELATOR = 313,
     T_EXPER_DESCRIPTION = 314,
     T_HEADSTACK_POS = 315,
     T_IF_DEF = 316,
     T_PASS_ORDER = 317,
     T_S2_GROUP_ORDER = 318,
     T_PHASE_CAL_DETECT = 319,
     T_TAPE_CHANGE = 320,
     T_NEW_SOURCE_COMMAND = 321,
     T_NEW_TAPE_SETUP = 322,
     T_SETUP_ALWAYS = 323,
     T_PARITY_CHECK = 324,
     T_TAPE_PREPASS = 325,
     T_PREOB_CAL = 326,
     T_MIDOB_CAL = 327,
     T_POSTOB_CAL = 328,
     T_HEADSTACK_MOTION = 329,
     T_PROCEDURE_NAME_PREFIX = 330,
     T_ROLL_REINIT_PERIOD = 331,
     T_ROLL_INC_PERIOD = 332,
     T_ROLL = 333,
     T_ROLL_DEF = 334,
     T_SEFD_MODEL = 335,
     T_SEFD = 336,
     T_SITE_TYPE = 337,
     T_SITE_NAME = 338,
     T_SITE_ID = 339,
     T_SITE_POSITION = 340,
     T_SITE_POSITION_EPOCH = 341,
     T_SITE_POSITION_REF = 342,
     T_SITE_VELOCITY = 343,
     T_HORIZON_MAP_AZ = 344,
     T_HORIZON_MAP_EL = 345,
     T_ZEN_ATMOS = 346,
     T_OCEAN_LOAD_VERT = 347,
     T_OCEAN_LOAD_HORIZ = 348,
     T_OCCUPATION_CODE = 349,
     T_INCLINATION = 350,
     T_ECCENTRICITY = 351,
     T_ARG_PERIGEE = 352,
     T_ASCENDING_NODE = 353,
     T_MEAN_ANOMALY = 354,
     T_SEMI_MAJOR_AXIS = 355,
     T_MEAN_MOTION = 356,
     T_ORBIT_EPOCH = 357,
     T_SOURCE_TYPE = 358,
     T_SOURCE_NAME = 359,
     T_IAU_NAME = 360,
     T_RA = 361,
     T_DEC = 362,
     T_SOURCE_POSITION_REF = 363,
     T_RA_RATE = 364,
     T_DEC_RATE = 365,
     T_SOURCE_POSITION_EPOCH = 366,
     T_REF_COORD_FRAME = 367,
     T_VELOCITY_WRT_LSR = 368,
     T_SOURCE_MODEL = 369,
     T_VSN = 370,
     T_FANIN_DEF = 371,
     T_FANOUT_DEF = 372,
     T_TRACK_FRAME_FORMAT = 373,
     T_DATA_MODULATION = 374,
     T_VLBA_FRMTR_SYS_TRK = 375,
     T_VLBA_TRNSPRT_SYS_TRK = 376,
     T_S2_RECORDING_MODE = 377,
     T_S2_DATA_SOURCE = 378,
     B_GLOBAL = 379,
     B_STATION = 380,
     B_MODE = 381,
     B_SCHED = 382,
     B_EXPER = 383,
     B_SCHEDULING_PARAMS = 384,
     B_PROCEDURES = 385,
     B_EOP = 386,
     B_FREQ = 387,
     B_CLOCK = 388,
     B_ANTENNA = 389,
     B_BBC = 390,
     B_CORR = 391,
     B_DAS = 392,
     B_HEAD_POS = 393,
     B_PASS_ORDER = 394,
     B_PHASE_CAL_DETECT = 395,
     B_ROLL = 396,
     B_IF = 397,
     B_SEFD = 398,
     B_SITE = 399,
     B_SOURCE = 400,
     B_TRACKS = 401,
     B_TAPELOG_OBS = 402,
     T_LITERAL = 403,
     T_NAME = 404,
     T_LINK = 405,
     T_ANGLE = 406,
     T_COMMENT = 407,
     T_COMMENT_TRAILING = 408
   };
#endif
/* Tokens.  */
#define T_VEX_REV 258
#define T_REF 259
#define T_DEF 260
#define T_ENDDEF 261
#define T_SCAN 262
#define T_ENDSCAN 263
#define T_CHAN_DEF 264
#define T_SAMPLE_RATE 265
#define T_BITS_PER_SAMPLE 266
#define T_SWITCHING_CYCLE 267
#define T_START 268
#define T_SOURCE 269
#define T_MODE 270
#define T_STATION 271
#define T_DATA_TRANSFER 272
#define T_ANTENNA_DIAM 273
#define T_AXIS_OFFSET 274
#define T_ANTENNA_MOTION 275
#define T_POINTING_SECTOR 276
#define T_AXIS_TYPE 277
#define T_BBC_ASSIGN 278
#define T_CLOCK_EARLY 279
#define T_RECORD_TRANSPORT_TYPE 280
#define T_ELECTRONICS_RACK_TYPE 281
#define T_NUMBER_DRIVES 282
#define T_HEADSTACK 283
#define T_RECORD_DENSITY 284
#define T_TAPE_LENGTH 285
#define T_RECORDING_SYSTEM_ID 286
#define T_TAPE_MOTION 287
#define T_TAPE_CONTROL 288
#define T_TAI_UTC 289
#define T_A1_TAI 290
#define T_EOP_REF_EPOCH 291
#define T_NUM_EOP_POINTS 292
#define T_EOP_INTERVAL 293
#define T_UT1_UTC 294
#define T_X_WOBBLE 295
#define T_Y_WOBBLE 296
#define T_NUT_REF_EPOCH 297
#define T_NUM_NUT_POINTS 298
#define T_NUT_INTERVAL 299
#define T_DELTA_PSI 300
#define T_DELTA_EPS 301
#define T_NUT_MODEL 302
#define T_EXPER_NUM 303
#define T_EXPER_NAME 304
#define T_EXPER_NOMINAL_START 305
#define T_EXPER_NOMINAL_STOP 306
#define T_PI_NAME 307
#define T_PI_EMAIL 308
#define T_CONTACT_NAME 309
#define T_CONTACT_EMAIL 310
#define T_SCHEDULER_NAME 311
#define T_SCHEDULER_EMAIL 312
#define T_TARGET_CORRELATOR 313
#define T_EXPER_DESCRIPTION 314
#define T_HEADSTACK_POS 315
#define T_IF_DEF 316
#define T_PASS_ORDER 317
#define T_S2_GROUP_ORDER 318
#define T_PHASE_CAL_DETECT 319
#define T_TAPE_CHANGE 320
#define T_NEW_SOURCE_COMMAND 321
#define T_NEW_TAPE_SETUP 322
#define T_SETUP_ALWAYS 323
#define T_PARITY_CHECK 324
#define T_TAPE_PREPASS 325
#define T_PREOB_CAL 326
#define T_MIDOB_CAL 327
#define T_POSTOB_CAL 328
#define T_HEADSTACK_MOTION 329
#define T_PROCEDURE_NAME_PREFIX 330
#define T_ROLL_REINIT_PERIOD 331
#define T_ROLL_INC_PERIOD 332
#define T_ROLL 333
#define T_ROLL_DEF 334
#define T_SEFD_MODEL 335
#define T_SEFD 336
#define T_SITE_TYPE 337
#define T_SITE_NAME 338
#define T_SITE_ID 339
#define T_SITE_POSITION 340
#define T_SITE_POSITION_EPOCH 341
#define T_SITE_POSITION_REF 342
#define T_SITE_VELOCITY 343
#define T_HORIZON_MAP_AZ 344
#define T_HORIZON_MAP_EL 345
#define T_ZEN_ATMOS 346
#define T_OCEAN_LOAD_VERT 347
#define T_OCEAN_LOAD_HORIZ 348
#define T_OCCUPATION_CODE 349
#define T_INCLINATION 350
#define T_ECCENTRICITY 351
#define T_ARG_PERIGEE 352
#define T_ASCENDING_NODE 353
#define T_MEAN_ANOMALY 354
#define T_SEMI_MAJOR_AXIS 355
#define T_MEAN_MOTION 356
#define T_ORBIT_EPOCH 357
#define T_SOURCE_TYPE 358
#define T_SOURCE_NAME 359
#define T_IAU_NAME 360
#define T_RA 361
#define T_DEC 362
#define T_SOURCE_POSITION_REF 363
#define T_RA_RATE 364
#define T_DEC_RATE 365
#define T_SOURCE_POSITION_EPOCH 366
#define T_REF_COORD_FRAME 367
#define T_VELOCITY_WRT_LSR 368
#define T_SOURCE_MODEL 369
#define T_VSN 370
#define T_FANIN_DEF 371
#define T_FANOUT_DEF 372
#define T_TRACK_FRAME_FORMAT 373
#define T_DATA_MODULATION 374
#define T_VLBA_FRMTR_SYS_TRK 375
#define T_VLBA_TRNSPRT_SYS_TRK 376
#define T_S2_RECORDING_MODE 377
#define T_S2_DATA_SOURCE 378
#define B_GLOBAL 379
#define B_STATION 380
#define B_MODE 381
#define B_SCHED 382
#define B_EXPER 383
#define B_SCHEDULING_PARAMS 384
#define B_PROCEDURES 385
#define B_EOP 386
#define B_FREQ 387
#define B_CLOCK 388
#define B_ANTENNA 389
#define B_BBC 390
#define B_CORR 391
#define B_DAS 392
#define B_HEAD_POS 393
#define B_PASS_ORDER 394
#define B_PHASE_CAL_DETECT 395
#define B_ROLL 396
#define B_IF 397
#define B_SEFD 398
#define B_SITE 399
#define B_SOURCE 400
#define B_TRACKS 401
#define B_TAPELOG_OBS 402
#define T_LITERAL 403
#define T_NAME 404
#define T_LINK 405
#define T_ANGLE 406
#define T_COMMENT 407
#define T_COMMENT_TRAILING 408




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 17 "vex.y"
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
/* Line 1489 of yacc.c.  */
#line 416 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

