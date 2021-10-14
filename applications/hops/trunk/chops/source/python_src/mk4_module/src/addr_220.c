/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.                                         */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         Return value    Address of filled app structure */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "type_220.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

struct type_220 *
addr_220 (short version,
          void *address,
          int *size)
    {
                                        /* Disk format same as app struct */
                                        /* so can use overlay directly */
    if (version == T220_VERSION)
        {
        *size = sizeof (struct type_220);
        return ((struct type_220 *)address);
        }
                                        /* Only one version currently supported */
                                        /* so must be an error to get here */
    msg ("Unsupported record format version number %d for", 2, version);
    msg ("type 220 record in addr_220()", 2);
    return (NULL);
    }
