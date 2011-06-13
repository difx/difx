/************************************************************************/
/*									*/
/* Parses the default control file, which gives FOURFIT reasonably	*/
/* sensible behaviour for the inexperienced user			*/
/*									*/
/* Created 9 April 1992 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "control.h"


int
set_defaults()
    {
    char *default_cfile, *getenv();
    extern struct c_block *cb_head;

    cb_head = NULL;                     /* signifies no c_blocks yet */

                                        /* Set up defaults through prepared */
                                        /* default control file.  Place the */
                                        /* behaviour under shell script control by */
                                        /* using environment variable */
    default_cfile = getenv("DEF_CONTROL");
    if (default_cfile == NULL)
        {
        msg ("Default control file DEF_CONTROL environment variable undefined!", 3);
        return (1);
        }
    if (parse_control_file(default_cfile) != 0)
        {
        msg ("Fatal error parsing default control file '%s'", 3, default_cfile);
	msg ("You must point $DEF_CONTROL to a valid fourfit", 3);
	msg ("control file name", 3);
        return (1);
        }
    return (0);
    }
