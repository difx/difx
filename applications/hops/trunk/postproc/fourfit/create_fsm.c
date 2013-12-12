/*******************************************************************************
*   create_fsm allocates space for the finite state machine table,             *
*              and fills it with the appropriate entries. Changing             *
*              the table entries will result in a changed syntax in            *
*              the control file. The table is searched in order for the        *
*              matching state and token type in subroutine parser. The         *
*              order of the state entries is important only insofar as the     *
*              first matching state and token type will be used. Thus, for     *
*              example, the token type match_all would normally come at the    *
*              end of the entries for a particular state.                      *
*                                                                              *
*                                                     rjc  92.9.18             *
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "parser.h"

#define fsm_table(aa,bb,cc,dd)                                        \
    fsm_base = (struct fsm_table_entry *)                             \
    realloc(fsm_base, (state_num+1)*sizeof(struct fsm_table_entry));  \
    (fsm_base+state_num)->current_state = aa;                         \
    (fsm_base+state_num)->token_type = bb;                            \
    (fsm_base+state_num)->action =  cc;                               \
    (fsm_base+state_num)->next_state = dd;                            \
    state_num++


int 
create_fsm ()
    {
    extern struct fsm_table_entry *fsm_base;
    int state_num;



    state_num = 0;                                 /* Initialize state number */
    fsm_base = (struct fsm_table_entry *) malloc(1);  
                                 /* So we can use realloc, even on first call */




    /*         current_state      token_type         action          next_state */
    fsm_table (NEED_CONDITION,    AND,               NO_OP,          NEED_CONDITION);
    fsm_table (NEED_CONDITION,    NOT,               NEGATE,         NEED_CONDITION);
    fsm_table (NEED_CONDITION,    F_GROUP,           NO_OP,          NEED_F_GROUP);
    fsm_table (NEED_CONDITION,    STATION,           NO_OP,          NEED_STATION);
    fsm_table (NEED_CONDITION,    BASELINE,          NO_OP,          NEED_BASELINE);
    fsm_table (NEED_CONDITION,    SCAN,              NO_OP,          NEED_SCAN);
    fsm_table (NEED_CONDITION,    SOURCE,            NO_OP,          NEED_SOURCE);
    fsm_table (NEED_CONDITION,    OR,                GEN_CBLOCKS,    NEED_OR);
    fsm_table (NEED_CONDITION,    MATCH_ALL,         GEN_CBLOCKS,    BLOCK_INTERIOR);

    fsm_table (NEED_F_GROUP,      ONE_CHAR,          SAVE_FG,        NEED_CONDITION);

    fsm_table (NEED_STATION,      ONE_CHAR,          SAVE_STAT,      NEED_CONDITION);

    fsm_table (NEED_BASELINE,     TWO_CHAR,          SAVE_BASE,      NEED_CONDITION);

    fsm_table (NEED_SCAN,         TIME_VAL,          SAVE_SCAN,      MAY_HAVE_TO);
    fsm_table (NEED_SCAN,         LESS_THAN,         SAVE_TOKEN_NUM, NEED_2ND_SCAN);
    fsm_table (NEED_SCAN,         GREATER_THAN,      SAVE_TOKEN_NUM, NEED_2ND_SCAN);

    fsm_table (MAY_HAVE_TO,       TO,                SAVE_TOKEN_NUM, NEED_2ND_SCAN);
    fsm_table (MAY_HAVE_TO,       MATCH_ALL,         POP_TOKEN,      NEED_CONDITION);  

    fsm_table (NEED_2ND_SCAN,     TIME_VAL,          SAVE_2ND_SCAN,  NEED_CONDITION);

    fsm_table (NEED_SOURCE,       ONE_CHAR,          SAVE_SOURCE,    NEED_CONDITION);
    fsm_table (NEED_SOURCE,       TWO_CHAR,          SAVE_SOURCE,    NEED_CONDITION);
    fsm_table (NEED_SOURCE,       MANY_CHAR,         SAVE_SOURCE,    NEED_CONDITION);

    fsm_table (NEED_OR,           OR,                CLEAR_CONDS,    NEED_CONDITION);

    fsm_table (BLOCK_INTERIOR,	  IF,                CLEAR_CONDS,    NEED_CONDITION);
    fsm_table (BLOCK_INTERIOR,	  INT_PARAM,	     SAVE_TOKEN_NUM, NEED_INT);
    fsm_table (BLOCK_INTERIOR,	  FLOAT_PARAM,       SAVE_TOKEN_NUM, NEED_FLOAT);
    fsm_table (BLOCK_INTERIOR,	  TWO_FLOAT_PARAM,   SAVE_TOKEN_NUM, NEED_TWO_FLOAT_1);
    fsm_table (BLOCK_INTERIOR,	  STRING_PARAM,      SAVE_TOKEN_NUM, NEED_STRING);
    fsm_table (BLOCK_INTERIOR,	  VECTOR_INT_PARAM,  SAVE_TOKEN_NUM, NEED_VECTOR_INT);
    fsm_table (BLOCK_INTERIOR,	  VECTOR_FLOAT_PARAM,SAVE_TOKEN_NUM, NEED_VECTOR_FLOAT);
    fsm_table (BLOCK_INTERIOR,	  VECTOR_CHAR_PARAM, CLEAR_FREQS,    NEED_VECTOR_CHAR);
    fsm_table (BLOCK_INTERIOR,	  VECTOR_STRING_PARAM,SAVE_TOKEN_NUM,NEED_VS_NUMBER);
    fsm_table (BLOCK_INTERIOR,	  CHAN_PARAM,        SAVE_TOKEN_NUM, NEED_CODES);
    fsm_table (BLOCK_INTERIOR,	  0,                 EOF_CLEANUP,    END_STATE);      

    fsm_table (NEED_INT,          INTEGER,           INSERT_PAR,     BLOCK_INTERIOR);

    fsm_table (NEED_FLOAT,        FLOAT,             INSERT_PAR,     BLOCK_INTERIOR);

    fsm_table (NEED_TWO_FLOAT_1,  FLOAT,             INSERT_V_PAR,   NEED_TWO_FLOAT_2);
    fsm_table (NEED_TWO_FLOAT_1,  INTEGER,           INSERT_V_PAR,   NEED_TWO_FLOAT_2);

    fsm_table (NEED_TWO_FLOAT_2,  FLOAT,             INSERT_V_PAR,   BLOCK_INTERIOR);
    fsm_table (NEED_TWO_FLOAT_2,  INTEGER,           INSERT_V_PAR,   BLOCK_INTERIOR);

    fsm_table (NEED_STRING,       MANY_CHAR,         INSERT_STRING,  BLOCK_INTERIOR);

    fsm_table (NEED_VECTOR_INT,   INTEGER,           INSERT_V_PAR,   NEED_VECTOR_INT);
    fsm_table (NEED_VECTOR_INT,	  MATCH_ALL,         POP_TOKEN,      BLOCK_INTERIOR);  

    fsm_table (NEED_VECTOR_FLOAT, FLOAT,             INSERT_V_PAR,   NEED_VECTOR_FLOAT);
    fsm_table (NEED_VECTOR_FLOAT, INTEGER,           INSERT_V_PAR,   NEED_VECTOR_FLOAT);
    fsm_table (NEED_VECTOR_FLOAT, MATCH_ALL,         POP_TOKEN,      BLOCK_INTERIOR);  

    fsm_table (NEED_VECTOR_CHAR,  ONE_CHAR,          INSERT_V_CHAR,  NEED_VECTOR_CHAR);
    fsm_table (NEED_VECTOR_CHAR,  TWO_CHAR,          INSERT_V_CHAR,  NEED_VECTOR_CHAR);
    fsm_table (NEED_VECTOR_CHAR,  MATCH_ALL,         POP_TOKEN,      BLOCK_INTERIOR);  

    fsm_table (NEED_CODES,        ONE_CHAR,          SAVE_CODES,     NEED_VECTOR_FLOAT);
    fsm_table (NEED_CODES,        TWO_CHAR,          SAVE_CODES,     NEED_VECTOR_FLOAT);
    fsm_table (NEED_CODES,        MANY_CHAR,         SAVE_CODES,     NEED_VECTOR_FLOAT);

    fsm_table (NEED_VS_NUMBER,    INTEGER,           INSERT_PAR,     NEED_VECTOR_STRING);

    fsm_table (NEED_VECTOR_STRING,ONE_CHAR,          INSERT_STRING,  NEED_VECTOR_STRING);
    fsm_table (NEED_VECTOR_STRING,TWO_CHAR,          INSERT_STRING,  NEED_VECTOR_STRING);
    fsm_table (NEED_VECTOR_STRING,MANY_CHAR,         INSERT_STRING,  NEED_VECTOR_STRING);

    fsm_table (0, 0, 0, 0);                                 /* mark table end */
    }
