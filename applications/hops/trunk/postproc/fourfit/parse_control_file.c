/*******************************************************************************
*   parse_control_file is the high-level routine in charge of reading          *
*   the input control file, conditioning the text, lexically analyzing         *
*   it, parsing the text, and generating c_blocks that embody the              *
*   appropriate information. All code here must be serially reusable,          *
*   as this routine is called twice.                                           *
*                                                 rjc 92.10.7                  *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include "control.h"
#include "parser.h"

#define IS_CONTROL_FILE 1
#define IS_SET_STRING 2

char *token_string[MAX_TOKENS];           /* global storage for token strings */
int token_cat[MAX_TOKENS];                /*    "      "     "   " categories */

double *float_values;
struct token_struct *tokens;       /* global ptrs to various temporary arrays */
char *char_values;
struct fsm_table_entry *fsm_base;
int *line_end;

int
parse_control_file (char* control_file_name,
                    char** control_file_buff,  //stash stripped but unparsed contents of control file
                    char** set_string_buff     //stash stripped but unparsed contents of set commands
)
    {
    int n;
    int flag = 0;
    char *input_string;

                                                   /* read input control file */ 
    if (read_control_file (control_file_name,&input_string, &flag) != 0) 
        {
        msg ("Error opening control file %s \n", 2, control_file_name);
        return (-1);
        }

    //these strings are malloced here so we can can save them to the type_222 
    //record later, they will be freed at the end of fourfit main 4/7/17 JPB
    //this is not a perfect solution, as it relies on this function only ever
    //being called with control_file_buff = param.control_file_buff
    //and set_string_buff = param.set_string_buff, this probably needs to be revised
    if(flag == IS_CONTROL_FILE)
        {
        if(*control_file_buff != NULL)
            {   
            free(*control_file_buff);
            *control_file_buff = (char*) malloc( strlen(input_string) + 1 );
            }
        else
            {
            *control_file_buff = (char*) malloc( strlen(input_string) + 1 );
            }
        strcpy(*control_file_buff, input_string);
        }
    else if(flag == IS_SET_STRING)
        {
        if(*set_string_buff != NULL)
            {   
            free(*set_string_buff);
            *set_string_buff = (char*) malloc( strlen(input_string) + 1 );
            }
        else
            {
            *set_string_buff = (char*) malloc( strlen(input_string) + 1 );
            }
        strcpy(*set_string_buff, input_string);
        }
        
                                                  /* perform lexical analysis */
    init_tokens ();                               /* first set up token array */

    if (lex (input_string) != 0)
        {
        msg ("Error lexically analyzing control file ", 2);
        return (-1);
        }

    create_fsm ();                                        /* create fsm table */

    if (parser () != 0)               /* parse token string and fill c_blocks */
        {
        msg ("Error parsing control file ", 2);
        return (-1);
        }



                                                  /* free up temporary storage */
    free (input_string);
    free (fsm_base);
    free (tokens);
    free (float_values);
    free (char_values);
    free (line_end);
    return (0);                                /* signal successful execution */
    }
