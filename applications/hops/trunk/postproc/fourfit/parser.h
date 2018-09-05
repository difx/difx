              /* Structure definitions for the FSM table */

struct fsm_table_entry
   {
   short   current_state;  /* small integer index for current state */
   short   token_type;     /* category of current token */
   short   action;         /* action to be performed for current state*/
   short   next_state;     /* index of possible following state */
   };

              /* Structure def. for tokenized input arrays */

struct token_struct
   {
   short symbol;           /* small integer to represent which token it is */
   short category;         /* small integer to define type of token this is*/
   int   value;            /* either null, an integer value for integer tokens,
                              or an index into float and char ptr arrays */
   int   line;             /* control file line number that this token is in */
   };




        /* Definitions for tokens which represent the 
           original symbols in the input control file. */


#define STATION_          1
#define BASELINE_         2
#define SOURCE_           3
#define SCAN_             4
#define F_GROUP_          5
#define IF_               6
#define ELSE_             7
#define AND_              8
#define OR_               9
#define NOT_             10
#define LPAREN_          11
#define RPAREN_          12
#define LESS_THAN_       13
#define GREATER_THAN_    14
#define MIN_WEIGHT_      15
#define X_CRC_           16
#define Y_CRC_           17
#define X_SLIP_SYNC_     18
#define Y_SLIP_SYNC_     19
#define FREQS_           20
#define INDEX_           21
#define PC_PHASE_        22
#define PC_MODE_         23
#define SB_WIN_          24
#define MB_WIN_          25
#define DR_WIN_          26
#define SKIP_            27
#define WILDCARD_        28
#define INT_             29
#define FLOAT_           30
#define TIME_VAL_        31
#define ONE_CHAR_        32
#define TWO_CHAR_        33
#define MANY_CHAR_       34
#define START_           35
#define STOP_            36
#define KEEP_            37
#define DISCARD_         38
#define NORMAL_          39
#define AP_BY_AP_        40
#define MANUAL_          41
#define SCAN_START_      42
#define EACH_MINUTE_     43
#define LSB_OFFSET_      44
#define REF_FREQ_        45
#define SWITCHED_        46
#define PERIOD_          47
#define GATES_           48
#define TRUE_            49
#define FALSE_           50
#define RA_OFFSET_       51
#define DEC_OFFSET_      52
#define TO_              53
#define ADHOC_PHASE_     54
#define SINEWAVE_        55
#define POLYNOMIAL_      56
#define ADHOC_PERIOD_    57
#define ADHOC_AMP_       58
#define ADHOC_POLY_      59
#define ADHOC_TREF_      60
#define PC_FREQ_         61
#define USE_SAMPLES_     62
#define PASSBAND_        63
#define T_COHERE_        64
#define MULTITONE_       65
#define IONOSPHERE_      66
#define PC_PERIOD_       67
#define PC_TONEMASK_     68
#define DELAY_OFFS_      69
#define DC_BLOCK_        70
#define SAMPLERS_        71
#define OPTIMIZE_CLOSURE_ 72
#define PC_PHASE_L_      73
#define PC_PHASE_R_      74
#define ION_WIN_         75
#define ION_NPTS_        76
#define INTERPOLATOR_    77
#define ITERATE_         78
#define SIMUL_           79
#define STATION_DELAY_   80
#define PC_DELAY_L_      81
#define PC_DELAY_R_      82
#define WEAK_CHANNEL_    83
#define PC_AMP_HCODE_    84
#define FMATCH_BW_PCT_   85
#define FILE_            86
#define ADHOC_FILE_      87
#define ADHOC_FILE_CHANS_ 88
#define MBD_ANCHOR_      89
#define MODEL_           90
#define SBD_             91
#define PC_PHASE_X_      92
#define PC_PHASE_Y_      93
#define PC_DELAY_X_      94
#define PC_DELAY_Y_      95
#define SAMPLER_DELAY_L_ 96
#define SAMPLER_DELAY_R_ 97
#define SAMPLER_DELAY_X_ 98
#define SAMPLER_DELAY_Y_ 99
#define ION_SMOOTH_      100
#define DELAY_OFFS_L_    101
#define DELAY_OFFS_R_    102
#define DELAY_OFFS_X_    103
#define DELAY_OFFS_Y_    104
#define PC_PHASE_OFFSET_L_ 105
#define PC_PHASE_OFFSET_R_ 106
#define PC_PHASE_OFFSET_X_ 107
#define PC_PHASE_OFFSET_Y_ 108
#define NOTCHES_           109
#define GEN_CF_RECORD_     110
#define EST_PC_MANUAL_     111
#define ADHOC_FLAG_FILE_   112
#define PLOT_DATA_DIR_     113
#define MAX_TOKENS         114   /* Increase to equal or exceed # tokens */


        /* Definitions of token categories */

#define INT_PARAM            1
#define FLOAT_PARAM          2
#define TWO_FLOAT_PARAM      3
#define VECTOR_INT_PARAM     4
#define VECTOR_FLOAT_PARAM   5
#define INTEGER              6
#define FLOAT                7
#define PARAMETER            8
#define IF                   9
#define MATCH_ALL           10
#define ONE_CHAR            11
#define TWO_CHAR            12
#define MANY_CHAR           13
#define TIME_VAL            14
#define STATION             15
#define BASELINE            16
#define SOURCE              17
#define SCAN                18
#define F_GROUP             19
#define NOT                 20
#define AND                 21
#define CHAN_PARAM          22
#define VECTOR_CHAR_PARAM   23
#define OR                  24
#define LESS_THAN           25
#define GREATER_THAN        26
#define TO                  27
#define VECTOR_STRING_PARAM 28
#define STRING_PARAM        29
#define INT_CONST           1048576


        /* Definitions of various FSM states */

#define BLOCK_INTERIOR       1
#define NEED_INT             2
#define NEED_FLOAT           3
#define NEED_TWO_FLOAT_1     4
#define NEED_TWO_FLOAT_2     5
#define NEED_VECTOR_INT      6
#define NEED_VECTOR_FLOAT    7
#define NEED_CONDITION       8
#define NEED_F_GROUP         9
#define END_STATE           10
#define NEED_STATION        11
#define NEED_SCAN           12
#define NEED_SOURCE         13
#define NEED_BASELINE       14
#define NEED_VECTOR_CHAR    15
#define NEED_CODES          16
#define NEED_OR             17
#define MAY_HAVE_TO         18
#define NEED_2ND_SCAN       19
#define NEED_VECTOR_STRING  20
#define NEED_VS_NUMBER      21
#define NEED_STRING         22
#define MAX_STATES          25   /* Increase to equal or exceed # states */

        /* Definitions of FSM actions */

#define NO_OP                1
#define SAVE_TOKEN_NUM       2
#define EOF_CLEANUP          3
#define INSERT_PAR           4
#define INSERT_V_PAR         5
#define POP_TOKEN            6
#define GEN_CBLOCKS          7
#define NEGATE               8
#define SAVE_FG              9
#define SAVE_STAT           10
#define SAVE_BASE           11
#define SAVE_SCAN           12
#define SAVE_SOURCE         13
#define CLEAR_CONDS         14
#define INSERT_V_CHAR       15
#define SAVE_CODES          16
#define CLEAR_FREQS         17
#define SAVE_2ND_SCAN       18
#define INSERT_STRING       19
#define SAVE_CSV_LIST       20
