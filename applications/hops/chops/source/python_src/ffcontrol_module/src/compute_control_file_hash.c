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

    retval = read_control_file (filename, &control_file_buff, &flag);

    if(retval == 0)
    {
        cf_hash = adler32_checksum( (unsigned char*) control_file_buff, strlen(control_file_buff) );
        if(control_file_buff != NULL){free(control_file_buff);};
    }
    return cf_hash;
}
