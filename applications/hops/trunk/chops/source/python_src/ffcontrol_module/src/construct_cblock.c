#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "control.h"
#include "ffcontrol_module_extern.h"

//same basic fuctionality as generate_cblock but without dependence on pass/param struct
int construct_cblock (char* filename, struct c_block* cb_out, char baseline[static 2], char source[static 31], char fgroup, int time)
{
    extern struct c_block* cb_head;
    extern void nullify_cblock (struct c_block* );
    extern void default_cblock (struct c_block* );
    extern int criteria_match (struct c_block* , char[2], char[31], char, int );
    extern int copy_cblock_parts ( struct c_block* , struct c_block* );
    extern int parse_control_file (char*, char**, char**);

    //init control record buffers
    char* control_file_buff = NULL;
    char* set_string_buff = NULL;
    struct c_block* cb_ptr = cb_head;

    cb_head = NULL;
    cb_head = (struct c_block *) malloc (sizeof (struct c_block) );

    nullify_cblock (cb_head);
    parse_control_file(filename, &control_file_buff, &set_string_buff);
    cb_ptr = cb_head;

    nullify_cblock( cb_out );
    default_cblock( cb_out );

    for (cb_ptr=cb_head; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
    {
        if(criteria_match (cb_ptr, baseline, source, fgroup, time))
        {
            copy_cblock_parts (cb_ptr,cb_out);
        }
    }

    if(control_file_buff != NULL){free(control_file_buff);};
    if(set_string_buff != NULL){free(set_string_buff);};

    return(0);
}
