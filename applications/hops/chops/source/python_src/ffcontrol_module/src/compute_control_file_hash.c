#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "control.h"
#include "adler32_checksum.h"
#include "ffcontrol_module_extern.h"

unsigned int compute_control_file_hash(char* filename)
{
    extern int read_control_file (char* , char**, int*);
    unsigned int cf_hash = 0;
    int flag = 0;
    int retval = 0;
    char* control_file_buff = NULL;
    int j, cf_start, cf_stop, cf_len;

    retval = read_control_file (filename, &control_file_buff, &flag);
    cf_len = strlen(control_file_buff);

    if(retval == 0)
    {
        cf_start = 0;
        cf_stop = cf_len;
        for(j=0; j<cf_len;j++)
        {
            cf_start = j;
            if( (control_file_buff[j] != ' ') && (control_file_buff[j] != '\t') && (control_file_buff[j] != '\n') )
            { break; }
        }
        for(j=cf_len-1; j>=0; j--)
        {
            cf_stop = j;
            if( (control_file_buff[j] != ' ') && (control_file_buff[j] != '\t') && (control_file_buff[j] != '\n') )
            { break; }
        }

        //now do the hashing
        cf_hash = adler32_checksum( (unsigned char*) &(control_file_buff[cf_start]), cf_stop-cf_start);
        if(control_file_buff != NULL){free(control_file_buff);};
    }
    return cf_hash;
}
