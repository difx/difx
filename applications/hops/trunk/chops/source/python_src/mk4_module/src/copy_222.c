/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_222().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status   */
/*                                                                      */
/*      Inputs:         t222            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created  April 3 2017 JPB                                            */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_222.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define TRUE 1

int
copy_222 (struct type_222 *t222,
          char **ptr,
          int *alloced)
    {
    short padded;
    int version;
    unsigned int setstr_hash;
    unsigned int control_hash;
    int setstr_len;
    int cf_len;
    int setstr_pad;
    int cf_pad;
    int full_size;
    int i;

    struct type_222_v0 *t222_v0;

    /* What version is requested for */
    /* the disk format? */
    sscanf (t222->version_no, "%2d", &version);
    /* This needs work for type 222 */
    if (version != T222_VERSION)
        {
        msg ("Version control not yet implemented for type 222 record", 2);
        return (-1);
        }
    
    if (version == 0)
        {
        //figure out the memory size we need to allocate (see addr_222.c)
        setstr_len = t222->setstring_length;
        cf_len = t222->cf_length;
        setstr_pad = (( setstr_len + 7 ) & ~7) + 8;
        cf_pad = ( (cf_len + 7 ) & ~7) + 8;
        full_size = sizeof(struct type_222) + setstr_pad + cf_pad; 

        /* Allocate space for output record */
        *ptr = (char *) malloc ( full_size );
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_222()", 2);
            return (-1);
            }
        *alloced = TRUE;
        
        
        /* Fill it in */
        t222_v0 = (struct type_222_v0 *) *ptr;
        strncpy (t222_v0->record_id, "222", 3);
        strncpy (t222_v0->version_no, "00", 2);
        padded = 1;
        cp_short (t222_v0->padded, padded);
        cp_int (t222_v0->setstring_hash, t222->setstring_hash);
        cp_int (t222_v0->control_hash, t222->control_hash);
        cp_int (t222_v0->setstring_length, t222->setstring_length);
        cp_int (t222_v0->cf_length, t222->cf_length);
        
        memcpy (t222_v0->control_contents, t222->control_contents, setstr_len);
        for(i=setstr_len; i<setstr_pad; i++){(t222_v0->control_contents)[i] = '\0';}

        //set the starting position of the control contents to the right place
        memcpy ( &( (t222_v0->control_contents)[setstr_pad] ) ,  &( (t222->control_contents)[setstr_pad] ) , cf_len);
        for(i=setstr_pad + cf_len; i<setstr_pad + cf_pad; i++){ (t222_v0->control_contents)[i] = '\0';}

        return (full_size);

        }
    else
        {
        msg ("Unrecognized version number %d in copy_222()", 2, version);
        return (-1);
        }
}
