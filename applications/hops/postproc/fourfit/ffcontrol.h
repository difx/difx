#ifndef FFCONTROL_H__
#define FFCONTROL_H__

#include "control.h"

int copy_cblock_parts ( struct c_block* f, struct c_block* t);
int create_fsm();
int criteria_match (struct c_block* cb_ptr, char base[2], char sour[31], char group, int time);
int default_cblock (struct c_block *cb_ptr);
char* get_bfstring (char* barg);
int init_tokens();
int is_keyword (char* next_token);
int is_timeval(char *next_token, int *i_value);
int is_integer (char *next_token, int *i_value);
int is_float (char *next_token, double *f_value);
int is_char (char *next_token);
int lex (char* input_string);
int nullify_cblock (struct c_block *cb_ptr);
int parse_control_file (char* control_file_name, char** control_file_buff, char** set_string_buff);
int parser(void);
static int append_cblocks (struct c_block **cb_start, struct c_block **cb_end, int num);
static void parsing_error (int state_num, int ntok);
int read_control_file (char* control_file_name, char** input_string, int* flag);
int skip_data (int scantime, char* baseline, char* source, char group);
int skip_index (int ind, struct c_block* cblock);
int fcode (char c, char *codes);

#endif /* end of include guard: FFCONTROL_H__ */
