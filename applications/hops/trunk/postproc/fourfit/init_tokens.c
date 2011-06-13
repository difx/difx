/******************************************************************************
*    init_tokens initializes the actual string values of each of fourfit's    *
*                recognizable tokens. It also initializes the category for    *
*                each token.                                                  *
*                                                 rjc  92.10.1                *
******************************************************************************/
#include "parser.h"
#include "control.h"
#include <stdio.h>

#define tokenize(aa,bb,cc) token_string[aa] = bb; token_cat[aa] = cc

int
init_tokens ()
    {
    int i;
    extern char *token_string[];          /* these token arrays could be put */
    extern int  token_cat[];              /* in a structure & initialized    */

    for (i=0; i<MAX_TOKENS; i++)
	tokenize (i, "", 0);                  /* initialize all tokens to null */


    /*       token #        string        category      */

    tokenize (STATION_,     "station",    STATION);
    tokenize (BASELINE_,    "baseline",   BASELINE);
    tokenize (SOURCE_,      "source",     SOURCE);
    tokenize (SCAN_,        "scan",       SCAN);
    tokenize (F_GROUP_,     "f_group",    F_GROUP);
    tokenize (IF_,          "if",         IF);
    tokenize (ELSE_,        "else",       0);
    tokenize (AND_,         "and",        AND);
    tokenize (OR_,          "or",         OR);
    tokenize (NOT_,         "not",        NOT);
    tokenize (LPAREN_,      "(",          0);
    tokenize (RPAREN_,      ")",          0);
    tokenize (LESS_THAN_,   "<",          LESS_THAN);
    tokenize (GREATER_THAN_,">",          GREATER_THAN);
    tokenize (MAX_PARITY_,  "max_parity", FLOAT_PARAM);
    tokenize (X_CRC_,       "x_crc",      INT_PARAM);
    tokenize (Y_CRC_,       "y_crc",      INT_PARAM);
    tokenize (X_SLIP_SYNC_, "x_slip_sync",INT_PARAM);
    tokenize (Y_SLIP_SYNC_, "y_slip_sync",INT_PARAM);
    tokenize (FREQS_,       "freqs",      VECTOR_CHAR_PARAM);
    tokenize (INDEX_,       "index",      VECTOR_INT_PARAM);
    tokenize (PC_PHASE_,    "pc_phases",  CHAN_PARAM);
    tokenize (PC_MODE_,     "pc_mode",    INT_PARAM);
    tokenize (PC_PERIOD_,   "pc_period",  INT_PARAM);
    tokenize (PC_TONEMASK_, "pc_tonemask",CHAN_PARAM);
    tokenize (LSB_OFFSET_,  "lsb_offset", FLOAT_PARAM);
    tokenize (SB_WIN_,      "sb_win",     TWO_FLOAT_PARAM);
    tokenize (MB_WIN_,      "mb_win",     TWO_FLOAT_PARAM);
    tokenize (DR_WIN_,      "dr_win",     TWO_FLOAT_PARAM);
    tokenize (SKIP_,        "skip",       INT_PARAM);
    tokenize (WILDCARD_,    "?",          ONE_CHAR);
    tokenize (INT_,         "~INTEGER~",  INTEGER);
    tokenize (FLOAT_,       "~FLOAT~",    FLOAT);
    tokenize (TIME_VAL_,    "~TIME_VAL~", TIME_VAL);
    tokenize (ONE_CHAR_,    "~1 CHAR~",   ONE_CHAR);
    tokenize (TWO_CHAR_,    "~2 CHARS~",  TWO_CHAR);
    tokenize (MANY_CHAR_,   "~STRING~",   MANY_CHAR);
    tokenize (START_,       "start",      INT_PARAM);
    tokenize (STOP_,        "stop",       INT_PARAM);
    tokenize (KEEP_,        "keep",       INT_CONST + KEEP);
    tokenize (DISCARD_,     "discard",    INT_CONST + DISCARD);
    tokenize (NORMAL_,      "normal",     INT_CONST + NORMAL);
    tokenize (AP_BY_AP_,    "ap_by_ap",   INT_CONST + AP_BY_AP);
    tokenize (MANUAL_,      "manual",     INT_CONST + MANUAL);
    tokenize (MULTITONE_,   "multitone",  INT_CONST + MULTITONE);
    tokenize (SCAN_START_,  "scan_start", INT_CONST + SCAN_START);
    tokenize (EACH_MINUTE_, "each_minute",INT_CONST + EACH_MINUTE);
    tokenize (TRUE_,        "true",       INT_CONST + 1);
    tokenize (FALSE_,       "false",      INT_CONST + 0);
    tokenize (REF_FREQ_,    "ref_freq",   FLOAT_PARAM);
    tokenize (SWITCHED_,    "switched",   INT_PARAM);
    tokenize (PERIOD_,      "period",     INT_PARAM);
    tokenize (GATES_,       "gates",      CHAN_PARAM);
    tokenize (RA_OFFSET_,   "ra_offset",  FLOAT_PARAM);
    tokenize (DEC_OFFSET_,  "dec_offset", FLOAT_PARAM);
    tokenize (TO_,          "to",         TO);
    tokenize (ADHOC_PHASE_, "adhoc_phase",INT_PARAM);
    tokenize (SINEWAVE_,    "sinewave",   INT_CONST + SINEWAVE);
    tokenize (POLYNOMIAL_,  "polynomial", INT_CONST + POLYNOMIAL);
    tokenize (ADHOC_PERIOD_,"adhoc_period",FLOAT_PARAM);
    tokenize (ADHOC_AMP_,   "adhoc_amp",  FLOAT_PARAM);
    tokenize (ADHOC_POLY_,  "adhoc_poly", VECTOR_FLOAT_PARAM);
    tokenize (ADHOC_TREF_,  "adhoc_tref", FLOAT_PARAM);
    tokenize (PC_FREQ_,     "pc_freqs"  , CHAN_PARAM);
    tokenize (USE_SAMPLES_, "use_samples",INT_PARAM);
    tokenize (PASSBAND_,    "passband",   VECTOR_FLOAT_PARAM);
    tokenize (T_COHERE_,    "t_cohere",   FLOAT_PARAM);
    tokenize (IONOSPHERE_,  "ionosphere", FLOAT_PARAM);
    }

