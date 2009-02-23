/* vexf.c FORTRAN VEX library */
/* ----------------------------------------------------------------------- */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include<stdlib.h>

#ifdef F2C
#include "f2c.h"
#else
typedef long int integer;
typedef int ftnlen;
typedef double doublereal;
#endif

#include "vex.h"
#include "vex_parse.tab.h"

#define LOOKUP(found,units,table,out)	{ int i; if(!found)\
                                    for (i=0; table[i].str !=NULL; i++)\
                                      if(strcmp(units,table[i].str)==0) {\
                                        out=table[i].factor;\
					found=1; } }
static void *save_ptr=NULL;
static int save_type=0;
static char *save_units=NULL;
static Llist *save_lowls=NULL;

static int
field_copy(char *field,int field_len,char *ptr);

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_open__
#else
fvex_open
#endif
(name, vex)
char **name;
integer *vex;
/*<       integer function fvex_open(ptr_ch(name),vex) >*/
/*<       character*(*) name >*/
/*<       integer vex >*/

/* opens a vex file and reads it into memory */

/* input: */
/*   character*(*) name      - pathname to file to be read in, */
/*                             null terminated */

/* output: */
/*   integer vex             - vex file reference, for subsequent calls */
/*   integer (return value)  - error code */
/*                           - non-zero indicates error */
{
  return vex_open(*name,(struct vex **)vex);
}

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fget_station_def__
#else
fget_station_def
#endif
(station, station_len, vex)
char **station;
integer *station_len, *vex;
/*<       integer function fget_station_def(ptr_ch(station),len(station), >*/
/*<                                         vex) >*/
/*<       implicit none >*/
/*<       character*(*) station >*/
/*<       integer vex >*/

/* returns the station defs in the $STATION section of the vex file */

/* To retreive the list of station defs, call the this routine the first */
/* time with vex set to the value returned by open_vex, on susequent */
/* calls use 0. If fvex_len reports that the returned character string has */
/* zero length then al the stations have been returned. */

/* input: */
/*   integer vex             - vex file reference */
/*                             use value returned open_vex for first call */
/*                             use 0 for subsequent calls */
/* output: */
/*   character*(*) station   - station def name */
/*                             use fvex_len to determine useful length */
/*                             should be at least 1 byte longer than */
/*                             longest expected to value to accomodate */
/*                             null termination */
			       
/*   integer (return value)  - error code, zero indicates no error */
/*                             otherwise errors determined by bits, if */
/*                             bit is on the error occurred, bits are */
/*                             numbered from 0 and correspond to */
/*                             the value of the corresponding power of 2, */
/*                             e.g. bit 0 is decimal 1 */
/*                          bit 0 - station def name did not fit in station */
{
  char *ptr;
  integer len, clen;

  if(*vex!=0)
    ptr=get_station_def((struct vex *)*vex);
  else
    ptr=get_station_def_next();

  if(ptr==NULL) {
    if(*station_len>0)
      *station[0]='\0';
    return 0;
  }

  len=strlen(ptr)+1;
  clen=len > *station_len ? *station_len : len;
  memcpy(*station,ptr,clen);

  if((len-1)>*station_len)
    return 1;
  
  return 0;

} 

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fget_mode_def__
#else
fget_mode_def
#endif
(mode, mode_len, vex)
char **mode;
integer *mode_len,*vex;
/*<       integer function fget_mode_def(ptr_ch(mode),len(mode),vex) >*/
/*<       implicit none >*/
/*<       character*(*) mode >*/
/*<       integer vex >*/

/* returns the mode def names from the $MODE section of the vex file */

/* To retreive the list of mode defs, call the this routine the first */
/* time with vex set to the value returned by open_vex, on susequent */
/* calls use 0. If fvex_len reports that the returned character string has */
/* zero length then al the stations have been returned. */

/* input: */
/*   integer vex             - vex file reference */
/*                             use value returned open_vex for first call */
/*                             use 0 for subsequent calls */
/* output: */
/*   character*(*) mode      - mode def name */
/*                             use fvex_len to determine useful length */
/*                             should be at least 1 byte longer than */
/*                             longest expected to value to accomodate */
/*                             null termination */
/*   integer (return value)  - error code, zero indicates no error */
/*                             otherwise errors determined by bits, if */
/*                             bit is on the error occurred, bits are */
/*                             numbered from 0 and correspond to */
/*                             the value of the corresponding power of 2, */
/*                             e.g. bit 0 is decimal 1 */
/*                             bit 0 - mode def name did not fit in mode */
{
  char *ptr;
  integer len, clen;

  if(*vex!=0)
    ptr=get_mode_def((struct vex *)*vex);
  else
    ptr=get_mode_def_next();

  if(ptr==NULL) {
    if(*mode_len>0)
      *mode[0]='\0';
    return 0;
  }

  len=strlen(ptr)+1;
  clen=len > *mode_len ? *mode_len : len;
  memcpy(*mode,ptr,clen);

  if((len-1)>*mode_len)
    return 1;
  
  return 0;
}

/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_source_def__
#else
fget_source_def
#endif
(source, source_len, vex)
char **source;
integer *source_len,*vex;
/*<       integer function fget_source_def(ptr_ch(source),len(source),vex) >*/
/*<       implicit none >*/
/*<       character*(*) source >*/
/*<       integer vex >*/

/* returns the source defs in the $SOURCE section of the vex file */

/* To retreive the list of source defs, call the this routine the first */
/*  time with vex set to the value returned by open_vex, on susequent calls */
/*  use 0. If fvex_len reports that the returned character string has zero */
/*   length then al the stations have been returned. */

/* input: */
/*   integer vex             - vex file reference */
/*                             use value returned open_vex for first call */
/*                             use 0 for subsequent calls */
/* output: */
/*   character*(*) source    - source def name */
/*                             use fvex_len to determine useful length */
/*                             should be at least 1 byte longer than */
/*                             longest expected to value to accomodate */
/*                             null termination */
/*   integer (return value)  - error code, zero indicates no error */
/*                             otherwise errors determined by bits, if */
/*                             bit is on the error occurred, bits are */
/*                             numbered from 0 and correspond to */
/*                             the value of the corresponding power of 2, */
/*                             e.g. bit 0 is decimal 1 */
/*                            bit 0 - source def name did not fit in source */
{
  char *ptr;
  integer len, clen;

  if(*vex!=0)
    ptr=get_source_def((struct vex *)*vex);
  else
    ptr=get_source_def_next();

  if(ptr==NULL) {
    if(*source_len>0)
      *source[0]='\0';
    return 0;
  }

  len=strlen(ptr)+1;
  clen=len > *source_len ? *source_len : len;
  memcpy(*source,ptr,clen);

  if((len-1)>*source_len)
    return 1;
  
  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_all_lowl__
#else
fget_all_lowl
#endif
(station, mode, statement, primitive, vex) 
char **station, **mode, **statement, **primitive;
integer *vex;
/*<      integer function fget_all_lowl(ptr_ch(station), >*/
/*<     &                               ptr_ch(mode), >*/
/*<     &                               ptr_ch(statement), >*/
/*<     &                               ptr_ch(primitive), >*/
/*<	&                               vex) >*/
/*<      implicit none >*/
/*<      character*(*) station,mode,statement,primitive >*/
/*<      integer vex >*/

/*get the low-level statement associated with a mode for a given statement */

/* This routine can be used to retrieve all the low-level statememts */
/*   associated with a given mode for a given station. Call this routine */
/*   the first time with vex set to the value returned by open_vex, on */
/*   susequent calls use 0. The call with vex nonzero should specify */
/*   the station, mode, statement, and primitive block (containing the */
/*   statement). When vex is zero, station, mode, statement, and primitive */
/*   are ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/*   Statements are returned in order of $MODE, $STATION, $GLOBAL. */

/* input: */
/*   character*(*) station     - station def name, null terminated */
/*   character*(*) mode        - mode def name, null terminated */
/*   character*(*) statement   - the statement to be retrieved */
/*                               null terminated */
/*   character*(*) primitive   - primitive block from which station */
/*                               should be retrieved. omit the leading "$" */
/*                               null terminated */
/*   integer vex               - vex file reference */
/*                              use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/* output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
{
  int iprimitive;

  if(*vex!=0) {
    save_type=lowl2int(*statement);
    iprimitive=block2int(*primitive);
    save_ptr=get_all_lowl(*station,*mode,save_type,iprimitive,(Vex *)*vex);
  } else
    save_ptr=get_all_lowl_next();

  if(save_ptr==NULL)
    return -3;

  return 0;
}

/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_mode_lowl__
#else
fget_mode_lowl
#endif
(station, mode, statement, primitive, vex)
char **station, **mode, **statement, **primitive;
integer *vex;
/*<      integer function fget_mode_lowl(ptr_ch(station), >*/
/*<     &                                ptr_ch(mode), >*/
/*<     &                                ptr_ch(statement), >*/
/*<     &                                ptr_ch(primitive), >*/
/*<	&                                vex) >*/
/*<      implicit none >*/
/*<      character*(*) station,mode,statement,primitive >*/
/*<      integer vex >*/

/* get the low-level statement associated with a mode for a given statement */

/* This routine can be used to retrieve the $MODE specified low-level */
/* statements */
/* associated with a given mode for a given station. Call this routine */
/* the first time with vex set to the value returned by open_vex, on */
/* susequent calls use 0. The call with vex nonzero should specify */
/* the station, mode, statement, and primitive block (containing the */
/* statement). When vex is zero, station, mode, statement, and primitive */
/* are ignored.

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* Information specified in the $STATION and $GLOBAL blocks is not */
/* returned. In general this routine is not useful, fget_all_lowl is */
/* usually what is wanted. */

/*  input: */
/*   character*(*) station     - station def name, null terminated */
/*   character*(*) mode        - mode def name, null terminated */
/*   character*(*) statement   - the statement to be retrieved, */
/*                               null terminated
/*   character*(*) primitive   - primitive block from which the statement */
/*                               should be retrieved. omit the leading "$" */
/*                               null terminated */
/*  integer vex                - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
{
  int iprimitive;

  if(*vex!=0) {
    save_type=lowl2int(*statement);
    iprimitive=block2int(*primitive);
    save_ptr=get_mode_lowl(*station,*mode,save_type,iprimitive,(Vex *)*vex);
  } else
    save_ptr=get_mode_lowl_next();

  if(save_ptr==NULL)
    return -3;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_station_lowl__
#else
fget_station_lowl
#endif
( station, statement, primitive, vex)
char **station, **statement, **primitive;
integer *vex;
/*<      integer function fget_station_lowl(ptr_ch(station), >*/
/*<     &                                 ptr_ch(statement), >*/
/*<     &                                 ptr_ch(primitive), >*/
/*<	&                                 vex) >*/
/*<      implicit none >*/
/*<      character*(*) station,statement,primitive >*/
/*<      integer vex >*/

/*get the low-level statement associated with a mode for a given statement */

/* This routine can be used to retrieve the $STATION specified low-level */
/*  statememts associated with a station. Call this routine the first time */
/*  with vex set to the value returned by open_vex, on susequent calls */
/*  use 0. The call with vex nonzero should specify the station, statement, */
/*   and primitive block (containing the statement). When vex is zero, */
/*   station, statement, and primitive are ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* Information specified in the $MODE and $GLOBAL blocks is not */
/*   returned. */

/*  input: */
/*   character*(*) station     - station def name, null terminated */
/*   character*(*) statement   - the statement to be retrieved, */
/*                               null terminated */
/*   character*(*) primitive   - primitive block from which station */
/*                               should be retrieved. omit the leading "$" */
/*                               null terminated */
/*   integer vex               - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
{
  int iprimitive;

  if(*vex!=0) {
    save_type=lowl2int(*statement);
    iprimitive=block2int(*primitive);
    save_ptr=get_station_lowl(*station,save_type,iprimitive,(Vex *)*vex);
  } else
    save_ptr=get_station_lowl_next();

  if(save_ptr==NULL)
    return -3;

  return 0;
}

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fget_global_lowl__
#else
fget_global_lowl
#endif
(statement, primitive, vex)
char **statement, **primitive;
integer *vex;
/*<      integer function fget_global_lowl(ptr_ch(statement), >*/
/*<     &                                 ptr_ch(primitive), >*/
/*<	&                                 vex) >*/
/*<      implicit none >*/
/*<      character*(*) statement,primitive >*/
/*<      integer vex >*/

/*get the low-level statement associated with a mode for a given statement */

/* This routine can be used to retrieve all the low-level statememts */
/*  global referenced. Call this routine the first time with vex set to the */
/*  value returned by open_vex, on susequent calls use 0. The call with vex */
/*   nonzero should specify the statement  and primitive block (containing  */
/*   the statement). When vex is zero statement and primitive are ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* Information specified in the $MODE and $STATION blocks is not */
/*   returned. */

/*  input: */
/*   character*(*) statement   - the statement to be retrieved, */
/*                               null terminated */
/*   character*(*) primitive   - primitive block from which station */
/*                               should be retrieved. omit the leading "$" */
/*                               null terminated */
/*  integer vex                - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
{
  int iprimitive;

  if(*vex!=0) {
    save_type=lowl2int(*statement);
    iprimitive=block2int(*primitive);
    save_ptr=get_global_lowl(save_type,iprimitive,(Vex *)*vex);
  } else
    save_ptr=get_global_lowl_next();

  if(save_ptr==NULL)
    return -3;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_scan_station__
#else
fget_scan_station
#endif
(start, start_len, mode, mode_len, scanid, scanid_len, station, vex)
char **start, **mode, **station, **scanid;
integer *start_len, *mode_len, *scanid_len, *vex;
/*<      integer function fget_scan_station(ptr_ch(start), len(start),>*/
/*<     &                                   ptr_ch(mode), len(mode), >*/
/*<     &                                   ptr_ch(scanid), len(scanid), >*/
/*<     &                                   ptr_ch(station), >*/
/*<	&                                   vex) >*/
/*<       implicit none >*/
/*<       character*(*) start,mode,station,scanid >*/
/*<       integer vex >*/

/* This routine can be used to retrieve all of the station statments */
/*  associated with a station. Call this routine the first time with vex set */
/*  to the value returned by open_vex, on susequent calls use 0. When vex */
/*  is zero, station is ignored. The call with vex nonzero should specify */
/*  the station. When vex is zero station is ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* When this routine does not return an error, the sources can be accessed */
/*   using fvex_scan_source. */

/*  input: */
/*   character*(*) station     - the station to reurn statements for */
/*                               null terminated
/*   integer vex               - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   character*(*) start       - nominal start time for this scan */
/*                               use fvex_len to determine useful length */
/*   character*(*) mode        - mode for this scan */
/*                               use fvex_len to determine useful length */
/*   character*(*) scanid      - scanid for this scan */
/*                               use fvex_len to determine useful length */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
/*                               -4 = start did not fit in start */
/*                               -5 = mode did not fit in mode */
/*                               -6 = scanid did not fit in scanid */
{
  int iprimitive, ierr;
  void *ptr;
  char *sidptr;

  if(*vex!=0) {
    save_type=T_STATION;
    save_ptr=get_scan_station(&save_lowls,&sidptr,*station,(Vex *)*vex);
  } else
    save_ptr=get_scan_station_next(&save_lowls,&sidptr);

  if(save_ptr==NULL)
    return -3;

  ptr=get_scan_mode(save_lowls);
  ierr=field_copy(*mode,*mode_len,ptr);
  if(ierr==-1)
    return -4;

  ptr=get_scan_start(save_lowls);
  ierr=field_copy(*start,*start_len,ptr);
  if(ierr==-1)
    return -5;

  ierr=field_copy(*scanid,*scanid_len,sidptr);
  if(ierr==-1)
    return -6;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_scan_data_transfer__
#else
fget_scan_data_transfer
#endif
(start, start_len, mode, mode_len, scanid, scanid_len, station, vex)
char **start, **mode, **station, **scanid;
integer *start_len, *mode_len, *scanid_len, *vex;
/*<      integer function fget_scan_data_transfer(ptr_ch(start), len(start),>*/
/*<     &                                   ptr_ch(mode), len(mode), >*/
/*<     &                                   ptr_ch(scanid), len(scanid), >*/
/*<     &                                   ptr_ch(station), >*/
/*<	&                                   vex) >*/
/*<       implicit none >*/
/*<       character*(*) start,mode,station,scanid >*/
/*<       integer vex >*/

/* This routine can be used to retrieve all of the data_transfer statments */
/*  associated with a station. Call this routine the first time with vex set */
/*  to the value returned by open_vex, on susequent calls use 0. When vex */
/*  is zero, station is ignored. The call with vex nonzero should specify */
/*  the station. When vex is zero station is ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* When this routine does not return an error, the sources can be accessed */
/*   using fvex_scan_source. */

/*  input: */
/*   character*(*) station     - the station to reurn statements for */
/*                               null terminated
/*   integer vex               - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   character*(*) start       - nominal start time for this scan */
/*                               use fvex_len to determine useful length */
/*   character*(*) mode        - mode for this scan */
/*                               use fvex_len to determine useful length */
/*   character*(*) scanid      - scanid for this scan */
/*                               use fvex_len to determine useful length */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
/*                               -4 = start did not fit in start */
/*                               -5 = mode did not fit in mode */
/*                               -6 = scanid did not fit in scanid */
{
  int iprimitive, ierr;
  void *ptr;
  char *sidptr;

  if(*vex!=0) {
    save_type=T_DATA_TRANSFER;
    save_ptr=get_scan_data_transfer(&save_lowls,&sidptr,*station,(Vex *)*vex);
  } else
    save_ptr=get_scan_data_transfer_next(&save_lowls,&sidptr);

  if(save_ptr==NULL)
    return -3;

  ptr=get_scan_mode(save_lowls);
  ierr=field_copy(*mode,*mode_len,ptr);
  if(ierr==-1)
    return -4;

  ptr=get_scan_start(save_lowls);
  ierr=field_copy(*start,*start_len,ptr);
  if(ierr==-1)
    return -5;

  ierr=field_copy(*scanid,*scanid_len,sidptr);
  if(ierr==-1)
    return -6;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_scan__
#else
fget_scan
#endif
(start, start_len, mode, mode_len, scanid, scanid_len, vex)
char **start, **mode, **scanid;
integer *start_len, *mode_len, *scanid_len,*vex;
/*<      integer function fget_scan(ptr_ch(start),len(start),  */
/*     &                           ptr_ch(mode),len(mode),     */
/*     &                           ptr_ch(scanid),len(scanid), */
/*     &                                   vex)               >*/
/*<     implicit none                                         >*/
/*<      character*(*) start,mode,station,scanid
/*<      integer vex                                          >*/

/* This routine can be used to retrieve all of the station statments */
/*   associated with a station. Call this routine the first time with vex set*/
/*   to the value returned by open_vex, on subsequent calls use 0. */

/* When this routine does not return an error, use the fget_station_scan to */
/*   find the station statements for this scan. */

/* When this routine does not return an error, the source names can be */
/*   accessed using fvex_scan_source. */

/*  input: */
/*   integer vex               - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/*  output: */
/*   character*(*) start       - nominal start time for this scan */
/*                               use fvex_len to determine useful length */
/*                               should be at least 1 character larger than */
/*                               longest possible value to hold null */
/*                               termination */
/*   character*(*) mode        - mode for this scan */
/*                               use fvex_len to determine useful length */
/*                               should be at least 1 character larger than */
/*                               longest possible value to hold null */
/*                               termination */
/*   character*(*) scanid      - scanid for this scan */
/*                               use fvex_len to determine useful length */
/*                               should be at least 1 character larger than */
/*                               longest possible value to hold null */
/*                               termination */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -2 = no more scans */
/*                               -4 = start time did not fit in start */
/*                               -5 = mode did not fit in mode */
/*                               -6 = scanid did not fit in scanid */
{
  int  ierr;
  void *ptr;
  char *cptr;

  if(*vex!=0) {
    save_lowls=get_scan(&cptr,(Vex *)*vex);
  } else
    save_lowls=get_scan_next(&cptr);

  if(save_lowls==NULL)
    return -2;

  ptr=get_scan_mode(save_lowls);
  ierr=field_copy(*mode,*mode_len,ptr);
  if(ierr==-1)
    return -4;

  ptr=get_scan_start(save_lowls);
  ierr=field_copy(*start,*start_len,ptr);
  if(ierr==-1)
    return -5;

  ierr=field_copy(*scanid,*scanid_len,cptr);
  if(ierr==-1)
    return -6;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_station_scan__
#else
fget_station_scan
#endif
(n)
int *n;
/*<      integer function fget_station_scan(n) >*/
/*<      implicit none >*/
/*<      integer n >*/

/* This routine can be used to retrieve the station for a station statement */
/*   in scan block found by fget_scan. */

/* When this routine does not return an error, the fields in the station */
/*   statement can be accessed using fvex_field. */

/* This is highly efficent when n increases by one on each call. */

/*   integer n                 - the number of the station statement in this*/
/*                               scan to return */
/*  input: */
/*   integer n                - station statement to return */

/*  output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -2 = no such station statment in this scan */
{
  int i;
  static int save_n=0;
  static Llist *save2_lowls=NULL;

  save_type=T_STATION;
  if(*n!=1 && *n==save_n+1 && save_lowls == save2_lowls)
    save_ptr=get_station_scan_next();
  else {
    save_ptr=get_station_scan(save_lowls);
    save2_lowls=save_lowls;
    for (i=1;i<*n && save_ptr!=NULL;i++)
      save_ptr=get_station_scan_next();
  }

  if(save_ptr==NULL)
    return -2;
  save_n=*n;
  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_data_transfer_scan__
#else
fget_data_transfer_scan
#endif
(n)
int *n;
/*<      integer function fget_data_transfer_scan(n) >*/
/*<      implicit none >*/
/*<      integer n >*/

/* This routine can be used to retrieve the station for a data_transfer *
 * statement */
/*   in scan block found by fget_scan. */

/* When this routine does not return an error, the fields in the */
/* data_transfer statement can be accessed using fvex_field. */

/* This is highly efficent when n increases by one on each call. */

/*   integer n            - the number of the data_transfer statement in this*/
/*                          scan to return */
/*  input: */
/*   integer n            - data_transfer statement to return */

/*  output: */
/*   integer (return value) - error code, zero indicates no error */
/*                         -2 = no such data_transfer statment in this scan */
{
  int i;
  static int save_n=0;
  static Llist *save2_lowls=NULL;

  save_type=T_DATA_TRANSFER;
  if(*n!=1 && *n==save_n+1 && save_lowls == save2_lowls)
    save_ptr=get_data_transfer_scan_next();
  else {
    save_ptr=get_data_transfer_scan(save_lowls);
    save2_lowls=save_lowls;
    for (i=1;i<*n && save_ptr!=NULL;i++)
      save_ptr=get_data_transfer_scan_next();
  }

  if(save_ptr==NULL)
    return -2;
  save_n=*n;
  return 0;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fget_source_lowl__
#else
fget_source_lowl
#endif
(source, statement, vex)
char **source, **statement;
integer *vex;
/*<       integer function fget_source_lowl(ptr_chr(source), */
/*                                          ptr_chr(statement), */
/*                                          vex) >*/
/*<       implicit none >*/
/*<       character*(*) source,statement >*/
/*<       integer vex >*/

/* get a low-level statement associated with a source */

/* This routine is used to retrieve the $SOURCE low-level statements */
/*   associated with a given source def. Call */
/*   this routine the first time with vex set to the value returned by */
/*   open_vex, on subsequent calls use 0. The call with vex nonzero should */
/*   specify the source and the statement. When vex is zero, source and */
/*   statement are ignored are ignored. */

/* When this routine does not return an error, the fields can be accessed */
/*   using fvex_field. */

/* input: */
/*   character*(*) source      - source def name, null terminated */
/*   character*(*) statement   - the statement to be retrieved, */
/*                               null terminated */
/*   integer vex               - vex file reference */
/*                               use value returned open_vex for first call */
/*                               use 0 for subsequent calls */
/* output: */
/*   integer fields            - number of useful elements returned */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -3 = no more statements to return */
{
  if(*vex!=0) {
    save_type=lowl2int(*statement);
    save_ptr=get_source_lowl(*source,save_type,(Vex *)*vex);
  } else
    save_ptr=get_source_lowl_next();

  if(save_ptr==NULL)
    return -3;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fget_literal__
#else
fget_literal
#endif
(string)
char *string;
/*<       integer function fget_literal(string) > */

/*<       implicit none >*/
/*<       I*2 string >*/

/* get a low-level statement associated with a scheduling parameters */

/* This routine is used to retrieve the $SCHEDULING_PARAMS low-level */
/*   statements associated with a given global,mode, or station ref. */

/* input: */
/*   none */

/* output: */
/*   I*2 string                - string text */
/*   integer (return value)    - number of characters or error  */
/*                               -3 = no more statements to return */

{
  char *string2;
  int ierr,count;

  /* first read in a string of literal text, and return the */
  /* pointer to the next literal string. */
  save_ptr=get_a_literal(save_ptr,&string2);
  
  /* How many characters do we have. */
  count = strlen(string2);
  if(count!=0)
    ierr=field_copy(string,count+1,string2);

  /* Evaluate the next pointer 'save_ptr' for end of a literal block */
  /* There could be several literal sub-blocks. */
  if(save_ptr==NULL)
    {
      save_ptr=get_all_lowl_next();
      /* check again to see if we've reached the end */
      /* of all literal sub-blocks. */
      if(save_ptr==NULL) 
	{
	  return -3;
	}
      return count;
    }
  return count;
}
/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fget_literal_st__
#else
fget_literal_st
#endif
(string)
char **string;
/*<       integer function fget_literal(ptr_chr(string) >*/
/*<       implicit none >*/
/*<       character*(*) string >*/

/* get a low-level statement associated with a scheduling parameters */

/* This routine is used to retrieve the $SCHEDULING_PARAMS low-level */
/*   statements associated with a given global,mode, or station ref. */

/* input: */
/*   none */

/* output: */
/*   character*(*) string      - string name, null terminated */
/*   integer (return value)    - count or error  -3 last statement */
{
  char *string2;
  int ierr,count;
  void *ptr;

  save_ptr=get_a_literal(save_ptr,&string2);
  count = strlen(string2);
  string2[count]='\0';
  if(count!=0)
    ierr=field_copy(*string,count+1,string2);

  if(save_ptr==NULL)
    {
      save_ptr=get_all_lowl_next();
      if(save_ptr==NULL) return -3;
      return count;
    }
  return count;
}
/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fvex_len__
#else
fvex_len
#endif
(field, field_len)
char *field;
ftnlen field_len;
/*<       integer function fvex_len(field) >*/
/*<       implicit none >*/
/*<       character*(*) field >*/

/* Returns the number of useful characters in a vex field */

/* input: */
/*   character*(*) field      - vex field to be examined */

/* output: */
/*   integer (return value)   - number of useful characters in field */
/*                              0 <= (return value) <= (*) */
{
  int i;

  for(i=0;i<field_len;i++)
    if(field[i]=='\0')
      return i;

  return field_len;
}


/* ----------------------------------------------------------------------- */
integer 
#ifdef F2C
fvex_field__
#else
fvex_field
#endif
(n, field, field_len)
integer *n, *field_len;
char **field;
/*<       integer function fvex_field(n,ptr_ch(field),len(field)) >*/
/*<       implicit none >*/
/*<       integer n >*/
/*<       character*(*) field >*/

/* Returns the field from a statement located using get_*_lowl*() routine. */

/* input: */
/*   integer n                - field to return */

/* output: */
/*   character*(*) field      - returned field, use fvex_len to get useful */
/*                              length */

/* output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -4 = field did not fit in field variable */
/*				 -6 = n out of range */
/*				 -9 = no statement available */
{
  int i,link,name, ierr;
  char *ptr, *units;

  save_units=NULL;

  if(save_type == 0 || save_ptr==NULL)
    return -9;

  ierr=vex_field(save_type,save_ptr,*n,&link,&name,&ptr,&units);
  if(ierr==-1) {
    fprintf(stderr,"unknown lowl %d\n",save_type);
    exit(1);
  } else if (ierr==-2)
    return -6;
  else if(ierr!=0) {
    fprintf(stderr,"unknown error in fvex_field %d\n",ierr);
    exit(1);
  }

  save_units=units;

  ierr=field_copy(*field,*field_len,ptr);
  if(ierr==-1)
    return -4;

  return 0;

}

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_units__
#else
fvex_units
#endif
(units, units_len)
char **units;
integer *units_len;
/*<       integer function fvex_units(ptr_ch(units),len(units)) >*/
/*<       implicit none >*/
/*<       character*(*) units >*/

/*Returns the units from the most recently accessed field by fget_field(). */

/* input: */
/*   none */

/* output: */
/*   character*(*) units      - returned units, use fvex_len to get useful */
/*                              length, zero length means no units */
/*                              should be at least 1 character larger than */
/*                              longest possible value to hold null */
/*                              termination */

/* output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               - 4 = units did not fit in units variable */
{
  int ierr;

  ierr=field_copy(*units,*units_len,save_units);
  if(ierr==-1)
    return -4;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_scan_source__
#else
fvex_scan_source
#endif
(n, src, src_len)
integer *n;
char **src;
integer *src_len;
/*<       integer function fvex_src_field(n,ptr_ch(src),len(src)) >*/
/*<       implicit none >*/
/*<       integer n >*/
/*<       character*(*) src >*/

/* Returns a source field from a station statement located using the */
/*    get_scan_station_lowl() routine. */

/* input: */
/*   integer n                - source field to return */

/* output: */
/*   character*(*) src        - returned field, use fvex_len to get useful */
/*                              length */
/*                              should be at least 1 character larger than */
/*                              longest possible value to hold null */
/*                              termination */

/* output: */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -4 = source did not fit in src variable */
/*                               -6 = n out of range */
{
  int i, ierr;
  char *ptr;

  if (*n < 1)
    return -6;

  ptr=get_scan_source(save_lowls);
  for (i=1;i < *n && ptr!= NULL;i++)
    ptr=get_scan_source_next();

  if(ptr==NULL)
    return -6;

  ierr=field_copy(*src,*src_len,ptr);
  if(ierr==-1)
    return -4;

  return 0;

}

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_double__
#else
fvex_double
#endif
(field, units, double__)
char **field, **units;
doublereal *double__;
/*<       integer function fvex_double(ptr_ch(field),ptr_ch(units),double) >*/
/*<       implicit none >*/
/*<       character*(*) field,units >*/
/*<       double precision double >*/

/* Will convert the ASCII representation of a number in field to */
/* MKS units according to the units specified in units. */

/* input: */
/*   character*(*) field      - vex fields to be examined, zero terminated */
/*   character*(*) units      - units to used in conversion, zero terminated */

/* output: */
/*   double precision double   - converted value */
/*   integer (return value)    - error code, zero indicates no error */
/*                              -7 = field was not a valid number */
/*                              -8 = units contained unknown units */
{

  char num[5], denom[5], *slash;
  int found, num_found, denom_found;
  double factor, num_factor, denom_factor;

  static struct {
    char *str;
    double factor;
  } time[] = {
    { "psec", 1e-12 },
    { "nsec", 1e-9 },
    { "usec", 1e-6 },
    { "msec", 1e-3 },
    { "sec" , 1.0 },
    { "min" , 60.0 },
    { "hr"  , 60.0*60.0 },
    { "day" , 60.0*60.0*24.0 },
    { "yr"  , 60.0*60.0*24.0*365.25 },
    { NULL  , 0.0 }
  };

  static struct {
    char *str;
    double factor;
  } freq[] = {
    { "mHz", 1e-3 },
    { "Hz" , 1.0 },
    { "kHz", 1e3 },
    { "MHz", 1e6 },
    { "GHz", 1e9 },
    { NULL  , 0.0 }
  };

  static struct {
    char *str;
    double factor;
  } sample_rate []= {
    { "ks/sec", 1e3 },
    { "Ms/sec", 1e6 },
    { NULL  , 0.0 }
  };

  static struct {
    char *str;
    double factor;
  } length[] = {
    { "um", 1e-6 },
    { "mm", 1e-3 },
    { "cm", 1e-2 },
    { "m" , 1.0 },
    { "km", 1e3 },
    { "in", 2.54*1e-2 },
    { "ft", 12*2.54*1e-2 },
    { NULL  , 0.0 }
  };

  static struct {
    char *str;
    double factor;
  } angle[] = {
    { "mdeg", 1e-3*M_PI/180.0 },
    { "deg" , M_PI/180.0 },
    { "amin", M_PI/(60.0*180.0) },
    { "asec", M_PI/(60.0*60.0*180.0) },
    { "rad" , 1.0 },
    { NULL  , 0.0 }
  };

  static struct {
    char *str;
    double factor;
  } flux[] = {
    { "mJy", 1e-29 },
    { "Jy", 1e-26 },
    { NULL  , 0.0 },
  };

  static struct {
    char *str;
    double factor;
  } bit_density[] = {
    {  "bpi", 1.0 },
    { "kbpi", 1e3 },
    { NULL  , 0.0 },
  };
  
  if(1!=sscanf(*field,"%lf",double__))
    return -7;

  found=0;
  LOOKUP(found,*units,time,factor);
  LOOKUP(found,*units,freq,factor);
  LOOKUP(found,*units,sample_rate,factor);
  LOOKUP(found,*units,length,factor);
  LOOKUP(found,*units,angle,factor);
  LOOKUP(found,*units,flux,factor);
  LOOKUP(found,*units,bit_density,factor);
  if(found) {
    *double__*=factor;
    return 0;
  }

  slash=strchr(*units,'/');
  if(slash==NULL)
    return -8;

  if(slash-*units > (sizeof(num)-1)) {
    fprintf(stderr,"num too small in fvex_double %.24s",*units);
    return -8;
  }
  strncpy(num,*units,slash-*units);
  num[slash-*units]='\0';

  if(strlen(slash) > sizeof(denom)) {
    fprintf(stderr,"denom too small in fvex_double %.24s",*units);
    return -8;
  }
  strcpy(denom,slash+1);

  num_found=0;
  LOOKUP(num_found,num,angle,num_factor);
  LOOKUP(num_found,num,length,num_factor);

  denom_found=0;
  LOOKUP(denom_found,denom,time,denom_factor);

  if(num_found && denom_found) {
    *double__*=(num_factor/denom_factor);
    return 0;
  }

  return -8;
}
/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_int__
#else
fvex_int
#endif
(field, int__)
char **field;
integer *int__;
/*<       integer function fvex_int(ptr_ch(field),int) >*/
/*<       implicit none >*/
/*<       character*(*) field >*/
/*<       integer int >*/

/* Will convert the ASCII representation of a number into int. */

/* input: */
/*   character*(*) field      - vex fields to be examined, zero terminated
 */

/* output: */
/*   integer int               - converted value */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -7 = field was not a valid int */
{
  if(1!=sscanf(*field,"%d",int__))
    return -7;

  return 0;
}


/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_date__
#else
fvex_date
#endif
(field, iarray, seconds)
char **field;
integer *iarray;
doublereal *seconds;
/*<       integer function fvex_date(ptr_chr(field),iarray,seconds) >*/
/*<       implicit none >*/
/*<       character*(*) field >*/
/*<       integer iarray(4) >*/
/*<       double precision seconds >*/

/*Will convert the ASCII representation of a date field to an integer array */
/* and double precision variable. */

/* input: */
/*   character*(*) field       - field to be converted, zero terminated */

/* output: */
/*   integer iarray(4)         - year,day of year,hour,and minutes */
/*           iarray(1)           year */
/*           iarray(2)           day of year */
/*           iarray(3)           hour */
/*           iarray(4)           minutes */
/*   double precision seconds  - seconds portion of date */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -7 = error in date field */
{
  int count;

  count=sscanf(*field,"%dy%dd%dh%dm%lfs",
	       iarray,iarray+1,iarray+2,iarray+3,seconds);

  if (count<2)
    return -7;
  else if (count<5) {
    *seconds=0.0;
    if (count<4) {
      iarray[3]=0;
      if (count<3)
	iarray[2]=0;
    }
  }

  return 0;
}

/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_ra__
#else
fvex_ra
#endif
(field, ra)
char **field;
doublereal *ra;
/*<       integer function fvex_ra(field,ra) >*/
/*<       implicit none >*/
/*<       character*(*) field >*/
/*<       double precision ra >*/

/* Convert a Right Ascenion field to radians */

/* input: */
/*   character*(*) field       - field to be converted, null terminated */

/* output: */
/*   double precision ra       - Right Ascenion value in radians */
/*   integer (return value)    - error code, zero indicates no error */
/*                               -7 = - error in ra field */
{
  int hour, min;
  double sec;

  if(3!=sscanf(*field,"%dh%dm%lfs",&hour,&min,&sec))
    return -7;

  *ra=M_PI*(hour+(min+sec/60.0)/60.0)/12.0;

  return 0;
}
/* ----------------------------------------------------------------------- */
integer
#ifdef F2C
fvex_dec__
#else
fvex_dec
#endif
(field, dec)
char **field;
doublereal *dec;
/*<       integer function fvex_dec(ptr_ch(field),dec) >*/
/*<       implicit none >*/
/*<       character*(*) field >*/
/*<       double precision dec >*/

/* Convert a Decliniation field to radians */

/* input: */
/*   character*(*) field       - field to be converted, zero terminated */

/* output: */
/*   double precision dec      - Declination value in radians */
/*   integer (return value)    - error code, zero indicates no error */
/*                               otherwise errors determined by bits, if */
/*                               bit is on the error occurred, bits are */
/*                               numbered from 0 and correspond to */
/*                               the value of the corresponding power of 2, */
/*                               e.g. bit 0 is decimal 1 */
/*                               -7 = error in dec field */
{
  int deg, min, isign;
  double sec;

  if(3!=sscanf(*field,"%dd%d'%lf\"",&deg,&min,&sec))
    return -7;

  isign=+1;

  if(deg<0 || **field=='-') {
    isign=-1;
    deg=-deg;
  }
    
  *dec=M_PI*(deg+(min+sec/60.0)/60.0)/180.0;
  if(isign<0)
    *dec=-*dec;

  return 0;
}
/* ----------------------------------------------------------------------- */
char *
#ifdef F2C
ptr_ch__
#else
ptr_ch
#endif
(char *ptr,ftnlen len)
{
  return ptr;
}
/* ----------------------------------------------------------------------- */
static int
field_copy(char *field,int field_len,char *ptr)
{
  int clen, len;

  if(ptr==NULL) {
    if(field_len < 1)
      return -1;

    field[0]='\0';
    return 0;
  }

  field_len--;

  len=strlen(ptr);
  clen=len >field_len ? field_len : len;

  memcpy(field,ptr,clen);
  field[clen]='\0';

  if (len > field_len)
    return -1;

  return 0;
}








