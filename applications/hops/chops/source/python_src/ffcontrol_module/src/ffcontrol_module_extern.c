#include <stddef.h> 

char progname[200] = "ffcontrolpy.so";
int msglev = 3;
char datadir[200] = "";

struct token_struct *tokens = NULL;   /* input struct of tokens & values   */
double *float_values = NULL;          /* array of actual fl. pt. values    */
struct fsm_table_entry *fsm_base = NULL;             /* start of fsm table */
struct c_block *cb_head = NULL;                  /* start of c_block chain */
char *char_values = NULL;             /* pointer to array of actual strings*/

char* control_file_buff = NULL;  //stash stripped but unparsed contents of control file
char* set_string_buff = NULL;     //stash stripped but unparsed contents of set commands
