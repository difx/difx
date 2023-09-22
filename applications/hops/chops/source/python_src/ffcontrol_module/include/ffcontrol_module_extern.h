#ifndef FFCONTROLPY_EXTERN_H__
#define FFCONTROLPY_EXTERN_H__

extern char *progname;
extern int *msglev;
extern char *datadir;

extern struct token_struct *tokens;   /* input struct of tokens & values   */
extern double *float_values;          /* array of actual fl. pt. values    */
extern struct fsm_table_entry *fsm_base;             /* start of fsm table */
extern struct c_block *cb_head;                  /* start of c_block chain */
extern char *char_values;             /* pointer to array of actual strings*/

extern char* control_file_buff;  //stash stripped but unparsed contents of control file
extern char* set_string_buff;     //stash stripped but unparsed contents of set commands

#endif /* end of include guard: FFCONTROLPY_EXTERN_H__ */
