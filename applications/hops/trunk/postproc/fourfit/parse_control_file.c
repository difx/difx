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
#include "control.h"
#include "parser.h"
#include "mk4_data.h"
#include "pass_struct.h"

char *token_string[MAX_TOKENS];           /* global storage for token strings */
int token_cat[MAX_TOKENS];                /*    "      "     "   " categories */

double *float_values;
struct token_struct *tokens;       /* global ptrs to various temporary arrays */
char *char_values;
struct fsm_table_entry *fsm_base;
int *line_end;

int
parse_control_file (control_file_name)
char *control_file_name;

    {
    int n;
    char *input_string;
    
                                                   /* read input control file */ 
    if (read_control_file (control_file_name,&input_string) != 0) 
        {
        msg ("Error opening control file %s", 2, control_file_name);
        return (-1);
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

    free (input_string);                         /* free up temporary storage */
    free (fsm_base);
    free (tokens);
    free (float_values);
    free (char_values);
    free (line_end);
    return (0);                                /* signal successful execution */
    }
