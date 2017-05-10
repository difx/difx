/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.  Either way, byte flipping is performed */
/* as necessary by the architecture-dependent macros cp_xxxx() defined  */
/* in bytflp.h                                                          */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         size            number of bytes read from input */
/*                                      address                         */
/*                      Return value    Address of filled app structure */
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

struct type_222 *
addr_222 (short version,
          void *address,
          int *size)
    {
    int setstr_hash;
    int control_hash;
    int setstr_len;
    int cf_len;
    int setstr_pad;
    int cf_pad;
    int full_size;
    int i;

    struct type_222 *t222;
    struct type_222_v0 *t222_v0;
    
    if (version != T222_VERSION)
        {
        msg ("Type 222 records not yet supported by version control", 2);
        return (NULL);
        }
    
    if (version == 0)
        {
        //cast the void ptr
        t222_v0 = (struct type_222_v0*)address;
        
        //collect the structure data through bytflp.h routines
        cp_int(setstr_len, t222_v0->setstring_length);
        cp_int(cf_len, t222_v0->cf_length);

        //NOTE: making the assumption that we need alignment on 8-byte boundaries
        //on some machines this may not be the case, and the word size might
        //be 16, 32, etc.
        
        //round up to get the next multiple of 8, then add 8 bytes 
        //done for aligned allocation and extra space for null termination
        int setstr_pad = (( setstr_len + 7 ) & ~7) + 8;
        int cf_pad = ( (cf_len + 7 ) & ~7) + 8;

        //now allocate and configure the return struct
        //There is some extra space allocated here, due to double counting 
        //of the first 8 bytes of the char array in both sizeof and 
        //in their padded lengths, but ~8 wasted bytes is not a big deal
        full_size = sizeof(struct type_222) + setstr_pad + cf_pad; 
        t222 = (struct type_222*) malloc ( full_size );
        
        /* Note input bytes so we can maintain */
        /* pointer in file image */
        *size = full_size;
        
        msg ("Allocated memory block %d", -1, t222);

        //fill in the data
        strncpy(t222->record_id, "222", 3);
        strncpy(t222->version_no, "00", 2);
        cp_short(t222->padded, t222_v0->padded);
        cp_int(t222->setstring_hash, t222_v0->setstring_hash);
        cp_int(t222->control_hash, t222_v0->control_hash);
        cp_int(t222->setstring_length, t222_v0->setstring_length);
        cp_int(t222->cf_length, t222_v0->cf_length);

        //copy in the set string contents (if any)
        memcpy(t222->control_contents, t222_v0->control_contents, setstr_len);
        
        //null out the trailing values
        for(i=setstr_len; i<setstr_pad; i++){(t222->control_contents)[i] = '\0';}
        
        //for now we don't bother with compression (cf file size ~5k or less),
        //but this might be needed in the future if control files become
        //very complex
        memcpy( &( (t222->control_contents)[setstr_pad] ), &( (t222_v0->control_contents)[setstr_pad] ), cf_len);

        //null out the trailing values
        for(i = setstr_pad + cf_len; i<setstr_pad + cf_pad; i++){(t222->control_contents)[i] = '\0';}
        return (t222);
        }
    else 
        {
        msg ("Unrecognized type 222 record version number %d", 2, version);
        return (NULL);
        }
    }
