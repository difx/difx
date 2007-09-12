/*  @(#)byteorder.h  version 1.1  created 04/04/20 10:08:16
                 fetched from SCCS 04/04/27 15:29:00
 LANGUAGE: C
 ENVIRONMENT: Any
 AUTHOR: Walter Brisken  

 Adapted from wfblib
*/
#ifndef __BYTEORDER_H__
#define __BYTEORDER_H__

#define BO_BIG_ENDIAN		0	/* Motorola */
#define BO_LITTLE_ENDIAN	1	/* Intel */

#ifdef __cplusplus
extern "C"
{
#endif

int byteorder();
int swapbytes(void *data, int order[][2]);

#ifdef __cplusplus
}
#endif	

#endif
