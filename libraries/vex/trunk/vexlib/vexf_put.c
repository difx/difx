/* vexf.c FORTRAN VEX library */
/* ----------------------------------------------------------------------- */
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <stdio.h>
#include <math.h>
#include <string.h>

#ifdef F2C
#include "f2c.h"
#else
typedef long int integer;
typedef int ftnlen;
typedef double doublereal;
#endif

#include "vex.h"
#include "y.tab.h"

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
void
#ifdef F2C
fcreate_vex__
#else
fcreate_vex
#endif
(file_or_screen) 
char **file_or_screen;
/*<      subroutine fcreate_vex(ptr_ch(file_or_screen//char(0)) >*/
/*<      implicit none >*/
/*<      character*(*) file_or_screen; >*/

/* input: */
/*   character*(*) file_or_screen;        - filename or NULL */
/*                                   */
/* output: */
/*   VEX format file to disk 'filename' or to screen */
/* */
{
  create_vex(*file_or_screen);
/*  return;*/
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_version__
#else
fcreate_version
#endif
(number) 
char **number;
/*<      subroutine fcreate_version(ptr_ch(number//char(0)) >*/
/*<      implicit none >*/
/*<      character*(*) file_or_screen; >*/

/* input: */
/*   character*(*) number;        - version number */
/*                                   */
/* output: */
/* */
{
  create_version(*number);
/*  return;*/
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_block__
#else
fcreate_block
#endif
(blockname) 
char **blockname;
/*<      subroutine fcreate_block(ptr_ch(blockname)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) blockname; >*/

/* ... */

/* input: */
/*   character*(*) blockname        - Name of Block being built */
/* output: */
/*   VEX format block name */
/* */
{
  create_block(*blockname);
/*  return;*/
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_def__
#else
fcreate_def
#endif
(defname) 
char **defname;
/*<      subroutine fcreate_def(ptr_ch(defname)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) defname; >*/

/* ... */

/* input: */
/*   character*(*) defname        - Name of def being built */
/* output: */
/*   VEX format def name */
/* */
{
  create_def(*defname);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fend_def__
#else
fend_def
#endif
() 
/*<      subroutine fend_def >*/
/*<      implicit none >*/

/* ... */

/* input: */
/*   none
/* output: */
/*   VEX format def name */
/* */
{
  end_def();
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_qref_qualifier__
#else
fcreate_qref_qualifier
#endif
(qualifier) 
char **qualifier;
/*<      subroutine fcreate_qref_qualifier(ptr_ch(qualifier)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) qualifier; >*/

/* ... */

/* input: */
/*   character*(*) qualifier      - qualifier for refs */
/* */
/* output: */
/*   VEX format def name */
/* */
{
  create_qref_qualifier(*qualifier);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_qref__
#else
fcreate_qref
#endif
(blockname, keyword)
char **blockname;
char **keyword;
/*<      subroutine fcreate_qref(ptr_ch(blockname)//(char *)0, >*/
/*<                              ptr_ch(keyword)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) blockname; >*/
/*<      character*(*) keyword; >*/

/* ... */

/* input: */
/*   character*(*) blockname      - Primitive block name */
/*   character*(*) keyword        - defined def with a specified */
/*                                  primitive blockname */

/* output: */
/*   VEX format def name */
/* */
{
  create_qref(*blockname,*keyword);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ref__
#else
fcreate_ref
#endif
(blockname, keyword)
char **blockname;
char **keyword;
/*<      subroutine fcreate_ref(ptr_ch(blockname)//(char *)0, >*/
/*<                               ptr_ch(keyword)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) blockname; >*/
/*<      character*(*) keyword; >*/

/* ... */

/* input: */
/*   character*(*) blockname      - Primitive block name */
/*   character*(*) keyword        - defined def with a specified */
/*                                  primitive blockname */

/* output: */
/*   VEX format def name */
/* */
{
  create_ref(*blockname,*keyword);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_external_ref__
#else
fcreate_external_ref
#endif
(filename, blockname, keyword) 
char **filename;
char **blockname;
char **keyword;
/*<      subroutine fcreate_external_ref(ptr_ch(blockname)//(char *)0, >*/
/*<                                      ptr_ch(filename)//(char *)0, >*/
/*<                                      ptr_ch(keyword)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) filename; >*/
/*<      character*(*) blockname; >*/
/*<      character*(*) keyword; >*/

/* ... */

/* input: */
/*   character*(*) filename       - External file name */
/*   character*(*) blockname      - Primitive block name */
/*   character*(*) keyword        - defined def with a specified */
/*                                  primitive blockname */
/* output: */
/*   VEX format for external ref */
/* */
{
  create_external_ref(*filename,
		      *blockname,
		      *keyword);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_scan__
#else
fcreate_scan
#endif
(str) 
char **str;
/*<      subroutine fcreate_scan(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str        - Name of scan being built */
/* output: */
/*   VEX format scan name */
/* */
{
  create_scan(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fend_scan__
#else
fend_scan
#endif
(str) 
char **str;
/*<      subroutine fend_scan >*/
/*<      implicit none >*/

/* ... */

/* input: */
/*   NONE */
/* output: */
/*   NONE */
/* */
{
  end_scan();
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_comment__
#else
fcreate_comment
#endif
(where, comment) 
char **where;
char **comment;
/*<      subroutine fcreate_comment(ptr_ch(where)//(char *)0, >*/
/*<                               ptr_ch(comment)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) where; >*/
/*<      character*(*) comment; >*/

/* ... */

/* input: */
/*   character*(*) where       - blank on top, nonblank on the side*/
/*   character*(*) blockname      - comment */
/* output: */
/*   VEX format for comment */
/* */
{
  create_comment(*where,
		 *comment);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_start__
#else
fcreate_start
#endif
(str) 
char **str;
/*<      subroutine fcreate_start(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str        - start of scan*/
/* output: */
/*   VEX format start */
/* */
{
  create_start(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_mode__
#else
fcreate_mode
#endif
(str) 
char **str;
/*<      subroutine fcreate_mode(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str        - scan mode */
/* output: */
/*   VEX format mode */
/* */
{
  create_mode(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source__
#else
fcreate_source
#endif
(str) 
char **str;
/*<      subroutine fcreate_source(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str        - scan source*/
/* output: */
/*   VEX format source */
/* */
{
  create_source(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_station__
#else
fcreate_station
#endif
(str, str2, str3, str4, str5, str6, str7, str8, str9)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
char **str9;
/*<      subroutine fcreate_station(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0, >*/
/*<                                 ptr_ch(str3)//(char *)0, >*/
/*<                                 ptr_ch(str4)//(char *)0, >*/
/*<                                 ptr_ch(str5)//(char *)0, >*/
/*<                                 ptr_ch(str6)//(char *)0, >*/
/*<                                 ptr_ch(str7)//(char *)0, >*/
/*<                                 ptr_ch(str8)//(char *)0, >*/
/*<                                 ptr_ch(str9)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/
/*<      character*(*) str9; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */
/*   character*(*) str4      - string 4 */
/*   character*(*) str5      - string 5 */
/*   character*(*) str6      - string 6 */
/*   character*(*) str7      - string 7 */
/*   character*(*) str8      - string 8 */
/*   character*(*) str9      - string 9 */

/* output: */
/*   VEX format */
/* */
{
  create_station(*str,
		 *str2,
		 *str3,
		 *str4,
		 *str5,
		 *str6,
		 *str7,
		 *str8,
		 *str9);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_station_drive_list__
#else
fcreate_station_drive_list
#endif
(str)
char **str;
/*<      subroutine fcreate_station_drive_list(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_station_drive_list(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_data_transfer__
#else
fcreate_data_transfer
#endif
(str, str2, str3, str4, str5, str6, str7, str8)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
/*<      subroutine fcreate_data_transfer(ptr_ch(str)//(char *)0, >*/
/*<                                       ptr_ch(str2)//(char *)0, >*/
/*<                                       ptr_ch(str3)//(char *)0, >*/
/*<                                       ptr_ch(str4)//(char *)0, >*/
/*<                                       ptr_ch(str5)//(char *)0, >*/
/*<                                       ptr_ch(str6)//(char *)0, >*/
/*<                                       ptr_ch(str7)//(char *)0, >*/
/*<                                       ptr_ch(str8)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */
/*   character*(*) str4      - string 4 */
/*   character*(*) str5      - string 5 */
/*   character*(*) str6      - string 6 */
/*   character*(*) str7      - string 7 */
/*   character*(*) str8      - string 8 */

/* output: */
/*   VEX format */
/* */
{
  create_data_transfer(*str,
		       *str2,
		       *str3,
		       *str4,
		       *str5,
		       *str6,
		       *str7,
		       *str8);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_antenna_diam__
#else
fcreate_antenna_diam
#endif
(str, str2) 
char **str;
char **str2;
/*<      subroutine fcreate_antenna_diam(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_antenna_diam(*str,
		      *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_axis_type__
#else
fcreate_axis_type
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_axis_type(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_axis_type(*str,
		   *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_axis_offset__
#else
fcreate_axis_offset
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_axis_offset(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_axis_offset(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_antenna_motion__
#else
fcreate_antenna_motion
#endif
(str, str2, str3, str4, str5)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
/*<      subroutine fcreate_antenna_motion(ptr_ch(str)//(char *)0, >*/
/*<                                    ptr_ch(str2)//(char *)0, >*/
/*<                                    ptr_ch(str3)//(char *)0, >*/
/*<                                    ptr_ch(str4)//(char *)0, >*/
/*<                                    ptr_ch(str5)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */
/*   character*(*) str4      - string 1 */
/*   character*(*) str5      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_antenna_motion(*str,
			*str2,
			*str3,
			*str4,
			*str5);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_pointing_sector__
#else
fcreate_pointing_sector
#endif
(str, str2, str3, str4, str5, str6, str7, str8, str9,str10,str11)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
char **str9;
char **str10;
char **str11;
/*<      subroutine fcreate_pointing_sector(ptr_ch(str)//(char *)0, >*/
/*<                                    ptr_ch(str2)//(char *)0, >*/
/*<                                    ptr_ch(str3)//(char *)0, >*/
/*<                                    ptr_ch(str4)//(char *)0, >*/
/*<                                    ptr_ch(str5)//(char *)0, >*/
/*<                                    ptr_ch(str6)//(char *)0, >*/
/*<                                    ptr_ch(str7)//(char *)0, >*/
/*<                                    ptr_ch(str8)//(char *)0, >*/
/*<                                    ptr_ch(str9)//(char *)0, >*/
/*<                                    ptr_ch(str10)//(char *)0, >*/
/*<                                    ptr_ch(str11)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/
/*<      character*(*) str9; >*/
/*<      character*(*) str10; >*/
/*<      character*(*) str11; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */
/*   character*(*) str7      - string */
/*   character*(*) str8      - string */
/*   character*(*) str9      - string */
/*   character*(*) str10     - string */
/*   character*(*) str11     - string */

/* output: */
/*   VEX format */
/* */
{
  create_pointing_sector(*str,
			 *str2,
			 *str3,
			 *str4,
			 *str5,
			 *str6,
			 *str7,
			 *str8,
			 *str9,
			 *str10,
			 *str11);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_bbc_assign__
#else
fcreate_bbc_assign
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_bbc_assign(ptr_ch(str)//(char *)0, >*/
/*<                                    ptr_ch(str2)//(char *)0, >*/
/*<                                    ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_bbc_assign(*str,
		    *str2,
		    *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_clock__
#else
fcreate_clock
#endif
(str, str2, str3, str4, str5)
/*
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
*/
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
/*<      subroutine fcreate_clock(ptr_ch(str)//(char *)0, >*/
/*<                                    ptr_ch(str2)//(char *)0, >*/
/*<                                    ptr_ch(str3)//(char *)0, >*/
/*<                                    ptr_ch(str4)//(char *)0, >*/
/*<                                    ptr_ch(str5)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/
/*<      character*(*) str5; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */
/*   character*(*) str5      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_clock(*str,
	       *str2,
	       *str3,
	       *str4,
	       *str5);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_record_transport_type__
#else
fcreate_record_transport_type
#endif
(name) 
char **name;
/*<      subroutine fcreate_record_transport_type(ptr_ch(name)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) name; >*/

/* ... */

/* input: */
/*   character*(*) name        - list of things*/
/* output: */
/*   VEX format name */
/* */
{
  create_record_transport_type(*name);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_electronics_rack_type__
#else
fcreate_electronics_rack_type
#endif
(name) 
char **name;
/*<      subroutine fcreate_electronics_rack_type(ptr_ch(name)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) name; >*/

/* ... */

/* input: */
/*   character*(*) name        - list of things*/
/* output: */
/*   VEX format name */
/* */
{
  create_electronics_rack_type(*name);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_number_drives__
#else
fcreate_number_drives
#endif
(name) 
char **name;
/*<      subroutine fcreate_number_drives(ptr_ch(name)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) name; >*/

/* ... */

/* input: */
/*   character*(*) name        - list of things*/
/* output: */
/*   VEX format name */
/* */
{
  create_number_drives(*name);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_headstack__
#else
fcreate_headstack
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_headstack(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0, >*/
/*<                                   ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_headstack(*str,
		   *str2,
		   *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_record_density__
#else
fcreate_record_density
#endif
(str, str2) 
char **str;
char **str2;
/*<      subroutine fcreate_record_density(ptr_ch(str)//(char *)0, >*/
/*<                                        ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_record_density(*str,
			*str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_recording_system_id__
#else
fcreate_recording_system_id
#endif
(name) 
char **name;
/*<      subroutine fcreate_recording_system_id(ptr_ch(name)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) name; >*/

/* ... */

/* input: */
/*   character*(*) name        - list of things*/
/* output: */
/*   VEX format name */
/* */
{
  create_recording_system_id(*name);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tape_length__
#else
fcreate_tape_length
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_tape_length(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0, >*/
/*<                                     ptr_ch(str3)//(char *)0, >*/
/*<                                     ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_tape_length(*str,
		     *str2,
		     *str3,
		     *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tape_motion__
#else
fcreate_tape_motion
#endif
(str, str2, str3, str4, str5, str6, str7)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
/*<      subroutine fcreate_tape_motion(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0, >*/
/*<                                     ptr_ch(str3)//(char *)0, >*/
/*<                                     ptr_ch(str4)//(char *)0, >*/
/*<                                     ptr_ch(str5)//(char *)0, >*/
/*<                                     ptr_ch(str6)//(char *)0, >*/
/*<                                     ptr_ch(str7)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */
/*   character*(*) str7      - string */

/* output: */
/*   VEX format */
/* */
{
  create_tape_motion(*str,
		     *str2,
		     *str3,
		     *str4,
		     *str5,
		     *str6,
		     *str7);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tape_control__
#else
fcreate_tape_control
#endif
(str)
char **str;
/*<      subroutine fcreate_tape_control(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_tape_control(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tai_utc__
#else
fcreate_tai_utc
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_tai_utc(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_tai_utc(*str,
		 *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_a1_tai__
#else
fcreate_a1_tai
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_a1_tai(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_a1_tai(*str,
		*str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_eop_ref_epoch__
#else
fcreate_eop_ref_epoch
#endif
(str)
char **str;
/*<      subroutine fcreate_eop_ref_epoch(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_eop_ref_epoch(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_num_eop_points__
#else
fcreate_num_eop_points
#endif
(str)
char **str;
/*<      subroutine fcreate_num_eop_points(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_num_eop_points(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_eop_interval__
#else
fcreate_eop_interval
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_eop_interval(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_eop_interval(*str,
		      *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ut1_utc__
#else
fcreate_ut1_utc
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_ut1_utc(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_ut1_utc(*str,
		 *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_x_wobble__
#else
fcreate_x_wobble
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_x_wobble(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_x_wobble(*str,
		  *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_y_wobble__
#else
fcreate_y_wobble
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_y_wobble(ptr_ch(str)//(char *)0, >*/
/*<                                  ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_y_wobble(*str,
		  *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_nut_ref_epoch__
#else
fcreate_nut_ref_epoch
#endif
(str)
char **str;
/*<      subroutine fcreate_nut_ref_epoch(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_nut_ref_epoch(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_num_nut_points__
#else
fcreate_num_nut_points
#endif
(str)
char **str;
/*<      subroutine fcreate_num_nut_points(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_num_nut_points(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_nut_interval__
#else
fcreate_nut_interval
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_nut_interval(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_nut_interval(*str,
		      *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_delta_psi__
#else
fcreate_delta_psi
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_delta_psi(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_delta_psi(*str,
		  *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_delta_eps__
#else
fcreate_delta_eps
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_delta_eps(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_delta_eps(*str,
		  *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_nut_model__
#else
fcreate_nut_model
#endif
(str)
char **str;
/*<      subroutine fcreate_nut_model(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_nut_model(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_exper_num__
#else
fcreate_exper_num
#endif
(str)
char **str;
/*<      subroutine fcreate_exper_num(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_exper_num(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_exper_name__
#else
fcreate_exper_name
#endif
(str)
char **str;
/*<      subroutine fcreate_exper_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_exper_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_exper_description__
#else
fcreate_exper_description
#endif
(str)
char **str;
/*<      subroutine fcreate_exper_description(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_exper_description(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_exper_nominal_start__
#else
fcreate_exper_nominal_start
#endif
(str)
char **str;
/*<      subroutine fcreate_exper_nominal_start(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_exper_nominal_start(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_exper_nominal_stop__
#else
fcreate_exper_nominal_stop
#endif
(str)
char **str;
/*<      subroutine fcreate_exper_nominal_stop(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_exper_nominal_stop(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_pi_name__
#else
fcreate_pi_name
#endif
(str)
char **str;
/*<      subroutine fcreate_pi_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_pi_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_pi_email__
#else
fcreate_pi_email
#endif
(str)
char **str;
/*<      subroutine fcreate_pi_email(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_pi_email(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_contact_name__
#else
fcreate_contact_name
#endif
(str)
char **str;
/*<      subroutine fcreate_contact_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_contact_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_contact_email__
#else
fcreate_contact_email
#endif
(str)
char **str;
/*<      subroutine fcreate_contact_email(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_contact_email(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_scheduler_name__
#else
fcreate_scheduler_name
#endif
(str)
char **str;
/*<      subroutine fcreate_scheduler_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_scheduler_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_scheduler_email__
#else
fcreate_scheduler_email
#endif
(str)
char **str;
/*<      subroutine fcreate_scheduler_email(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_scheduler_email(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_target_correlator__
#else
fcreate_target_correlator
#endif
(str)
char **str;
/*<      subroutine fcreate_target_correlator(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_target_correlator(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_chan_def__
#else
fcreate_chan_def
#endif
(str, str2, str3, str4, str5, str6, str7, str8, str9)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
char **str9;
/*<      subroutine fcreate_chan_def(ptr_ch(str)//(char *)0, >*/
/*<                                  ptr_ch(str2)//(char *)0, >*/
/*<                                  ptr_ch(str3)//(char *)0, >*/
/*<                                  ptr_ch(str4)//(char *)0, >*/
/*<                                  ptr_ch(str5)//(char *)0, >*/
/*<                                  ptr_ch(str6)//(char *)0, >*/
/*<                                  ptr_ch(str7)//(char *)0, >*/
/*<                                  ptr_ch(str8)//(char *)0, >*/
/*<                                  ptr_ch(str9)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/
/*<      character*(*) str9; >*/
/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */
/*   character*(*) str7      - string */
/*   character*(*) str8      - string */
/*   character*(*) str9      - string */

/* output: */
/*   VEX format */
/* */
{
  create_chan_def(*str,
		  *str2,
		  *str3,
		  *str4,
		  *str5,
		  *str6,
		  *str7,
		  *str8,
		  *str9);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_chan_def_states__
#else
fcreate_chan_def_states
#endif
(str)
char **str;
/*<      subroutine fcreate_chan_def_states(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{

  create_chan_def_states(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_sample_rate__
#else
fcreate_sample_rate
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_sample_rate(ptr_ch(str)//(char *)0, >*/
/*<                                  ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_sample_rate(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_bits_per_sample__
#else
fcreate_bits_per_sample
#endif
(str)
char **str;
/*<      subroutine fcreate_bits_per_sample(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_bits_per_sample(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_cycle__
#else
fcreate_cycle
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_cycle(ptr_ch(str)//(char *)0, >*/
/*<                                  ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_cycle(*str,
	       *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_switching_cycle__
#else
fcreate_switching_cycle
#endif
(str)
char **str;
/*<      subroutine fcreate_switching_cycle(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_switching_cycle(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_headstack_pos__
#else
fcreate_headstack_pos
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_headstack_pos(ptr_ch(str)//(char *)0, >*/
/*<                                       ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_headstack_pos(*str,
		       *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_headstack_reference__
#else
fcreate_headstack_reference
#endif
(str)
char **str;
/*<      subroutine fcreate_headstack_reference(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_headstack_reference(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_if_def__
#else
fcreate_if_def
#endif
(str, str2, str3, str4, str5, str6, str7, str8, str9,str10, str11)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
char **str9;
char **str10;
char **str11;
/*<      subroutine fcreate_if_def(ptr_ch(str)//(char *)0, >*/
/*<                                ptr_ch(str2)//(char *)0, >*/
/*<                                ptr_ch(str3)//(char *)0, >*/
/*<                                ptr_ch(str4)//(char *)0, >*/
/*<                                ptr_ch(str5)//(char *)0, >*/
/*<                                ptr_ch(str6)//(char *)0, >*/
/*<                                ptr_ch(str7)//(char *)0, >*/
/*<                                ptr_ch(str8)//(char *)0, >*/
/*<                                ptr_ch(str9)//(char *)0, >*/
/*<                                ptr_ch(str10)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/
/*<      character*(*) str9; >*/
/*<      character*(*) str10; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */
/*   character*(*) str5      - string  */
/*   character*(*) str6      - string  */
/*   character*(*) str7      - string  */
/*   character*(*) str8      - string  */
/*   character*(*) str9      - string  */
/*   character*(*) str10     - string  */

/* output: */
/*   VEX format */
/* */
{
  create_if_def(*str,
		*str2,
		*str3,
		*str4,
		*str5,
		*str6,
		*str7,
		*str8,
		*str9,
		*str10,
		*str11);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_phase_cal_detect__
#else
fcreate_phase_cal_detect
#endif
(str)
char **str;
/*<      subroutine fcreate_phase_cal_detect(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_phase_cal_detect(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_phase_cal_detect_list__
#else
fcreate_phase_cal_detect_list
#endif
(str)
char **str;
/*<      subroutine fcreate_phase_cal_detect_list(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_phase_cal_detect_list(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_s2_group_order__
#else
fcreate_s2_group_order
#endif
(str)
char **str;
/*<      subroutine fcreate_s2_group_order(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_s2_group_order(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tape_change__
#else
fcreate_tape_change
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_tape_change(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_tape_change(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_pass_order__
#else
fcreate_pass_order
#endif
(str)
char **str;
/*<      subroutine fcreate_pass_order(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_pass_order(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_new_tape_setup__
#else
fcreate_new_tape_setup
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_new_tape_setup(ptr_ch(str)//(char *)0, >*/
/*<                                        ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_new_tape_setup(*str,
			*str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_new_source_command__
#else
fcreate_new_source_command
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_new_source_command(ptr_ch(str)//(char *)0, >*/
/*<                                            ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_new_source_command(*str,
			    *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_headstack_motion__
#else
fcreate_headstack_motion
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_headstack_motion(ptr_ch(str)//(char *)0, >*/
/*<                                          ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_headstack_motion(*str,
			  *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_tape_prepass__
#else
fcreate_tape_prepass
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_tape_prepass(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0, >*/
/*<                                      ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_tape_prepass(*str,
		      *str2,
		      *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_parity_check__
#else
fcreate_parity_check
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_parity_check(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0, >*/
/*<                                      ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_parity_check(*str,
		      *str2,
		      *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_setup_always__
#else
fcreate_setup_always
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_setup_always(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0, >*/
/*<                                      ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_setup_always(*str,
		      *str2,
		      *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_postob_cal__
#else
fcreate_postob_cal
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_postob_cal(ptr_ch(str)//(char *)0, >*/
/*<                                    ptr_ch(str2)//(char *)0, >*/
/*<                                    ptr_ch(str3)//(char *)0, >*/
/*<                                    ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_postob_cal(*str,
		    *str2,
		    *str3,
		    *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_midob_cal__
#else
fcreate_midob_cal
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_midob_cal(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0, >*/
/*<                                   ptr_ch(str3)//(char *)0, >*/
/*<                                   ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_midob_cal(*str,
		   *str2,
		   *str3,
		   *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_preob_cal__
#else
fcreate_preob_cal
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_preob_cal(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0, >*/
/*<                                   ptr_ch(str3)//(char *)0, >*/
/*<                                   ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_preob_cal(*str,
		   *str2,
		   *str3,
		   *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_roll__
#else
fcreate_roll
#endif
(str)
char **str;
/*<      subroutine fcreate_roll(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{

  create_roll(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_procedure_name_prefix__
#else
fcreate_procedure_name_prefix
#endif
(str)
char **str;
/*<      subroutine fcreate_procedure_name_prefix(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_procedure_name_prefix(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_roll_reinit_period__
#else
fcreate_roll_reinit_period
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_roll_reinit_period(ptr_ch(str)//(char *)0, >*/
/*<                                            ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_roll_reinit_period(*str,
			    *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_roll_inc_period__
#else
fcreate_roll_inc_period
#endif
(str)
char **str;
/*<      subroutine fcreate_roll_inc_period(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_roll_inc_period(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_roll_def__
#else
fcreate_roll_def
#endif
(str)
char **str;
/*<      subroutine fcreate_roll_def(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_roll_def(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_literal__
#else
fcreate_literal
#endif
(str)
     /*
char **str;
*/
char **str;
/*<      subroutine fcreate_literal(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_literal(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_sefd_model__
#else
fcreate_sefd_model
#endif
(str)
char **str;
/*<      subroutine fcreate_sefd_model(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_sefd_model(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_sefd_model_parameter__
#else
fcreate_sefd_model_parameter
#endif
(str)
char **str;
/*<      subroutine fcreate_sefd_model_parameter(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_sefd_model_parameter(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_sefd__
#else
fcreate_sefd
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_sefd(ptr_ch(str)//(char *)0, >*/
/*<                              ptr_ch(str2)//(char *)0, >*/
/*<                              ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/* output: */
/*   VEX format */
/* */
{
  create_sefd(*str,
	      *str2,
	      *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_name__
#else
fcreate_site_name
#endif
(str)
char **str;
/*<      subroutine fcreate_site_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_type__
#else
fcreate_site_type
#endif
(str)
char **str;
/*<      subroutine fcreate_site_type(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_type(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_id__
#else
fcreate_site_id
#endif
(str)
char **str;
/*<      subroutine fcreate_site_id(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_ID(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_position__
#else
fcreate_site_position
#endif
(str, str2, str3, str4, str5, str6)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
/*<      subroutine fcreate_site_position(ptr_ch(str)//(char *)0, >*/
/*<                                       ptr_ch(str2)//(char *)0, >*/
/*<                                       ptr_ch(str3)//(char *)0, >*/
/*<                                       ptr_ch(str4)//(char *)0, >*/
/*<                                       ptr_ch(str5)//(char *)0, >*/
/*<                                       ptr_ch(str6)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_position(*str,
		       *str2,
		       *str3,
		       *str4,
		       *str5,
		       *str6);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_position_epoch__
#else
fcreate_site_position_epoch
#endif
(str)
char **str;
/*<      subroutine fcreate_site_position_epoch(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_position_epoch(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_position_ref__
#else
fcreate_site_position_ref
#endif
(str)
char **str;
/*<      subroutine fcreate_site_position_ref(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_position_ref(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_site_velocity__
#else
fcreate_site_velocity
#endif
(str, str2, str3, str4, str5, str6)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
/*<      subroutine fcreate_site_velocity(ptr_ch(str)//(char *)0, >*/
/*<                                       ptr_ch(str2)//(char *)0, >*/
/*<                                       ptr_ch(str3)//(char *)0, >*/
/*<                                       ptr_ch(str4)//(char *)0, >*/
/*<                                       ptr_ch(str5)//(char *)0, >*/
/*<                                       ptr_ch(str6)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */

/* output: */
/*   VEX format */
/* */
{
  create_site_velocity(*str,
		       *str2,
		       *str3,
		       *str4,
		       *str5,
		       *str6);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_horizon_map_az__
#else
fcreate_horizon_map_az
#endif
(str)
char **str;
/*<      subroutine fcreate_horizon_map_az >*/
/*<      implicit none >*/

/* ... */

/* input: */

/* output: */
/*   VEX format */
/* */
{

  create_horizon_map_az();
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_horizon_map_el__
#else
fcreate_horizon_map_el
#endif
(str)
char **str;
/*<      subroutine fcreate_horizon_map_el >*/
/*<      implicit none >*/

/* ... */

/* input: */

/* output: */
/*   VEX format */
/* */
{

  create_horizon_map_el();
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_horizon_map__
#else
fcreate_horizon_map
#endif
(str, str2) 
char **str;
char **str2;
/*<      subroutine fcreate_horizon_map(ptr_ch(str)//(char *)0, >*/
/*<                                        ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */

/* output: */
/*   VEX format */
/* */
{
  create_horizon_map(*str,*str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_zen_atmos__
#else
fcreate_zen_atmos
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_zen_atmos(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_zen_atmos(*str,
		   *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ocean_load_vert__
#else
fcreate_ocean_load_vert
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_ocean_load_vert(ptr_ch(str)//(char *)0, >*/
/*<                                         ptr_ch(str2)//(char *)0, >*/
/*<                                         ptr_ch(str3)//(char *)0, >*/
/*<                                         ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_ocean_load_vert(*str,
			 *str2,
			 *str3,
			 *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ocean_load_horiz__
#else
fcreate_ocean_load_horiz
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_ocean_load_horiz(ptr_ch(str)//(char *)0, >*/
/*<                                          ptr_ch(str2)//(char *)0, >*/
/*<                                          ptr_ch(str3)//(char *)0, >*/
/*<                                          ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_ocean_load_horiz(*str,
			  *str2,
			  *str3,
			  *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_occupation_code__
#else
fcreate_occupation_code
#endif
(str)
char **str;
/*<      subroutine fcreate_occupation_code(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_occupation_code(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_inclination__
#else
fcreate_inclination
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_inclination(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_inclination(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_eccentricity__
#else
fcreate_eccentricity
#endif
(str)
char **str;
/*<      subroutine fcreate_eccentricity(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_eccentricity(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_arg_perigee__
#else
fcreate_arg_perigee
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_arg_perigee(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_arg_perigee(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ascending_node__
#else
fcreate_ascending_node
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_ascending_node(ptr_ch(str)//(char *)0, >*/
/*<                                        ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_ascending_node(*str,
			*str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_mean_anomaly__
#else
fcreate_mean_anomaly
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_mean_anomaly(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_mean_anomaly(*str,
		      *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_semi_major_axis__
#else
fcreate_semi_major_axis
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_semi_major_axis(ptr_ch(str)//(char *)0, >*/
/*<                                         ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_semi_major_axis(*str,
			 *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_mean_motion__
#else
fcreate_mean_motion
#endif
(str)
char **str;
/*<      subroutine fcreate_mean_motion(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_mean_motion(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_orbit_epoch__
#else
fcreate_orbit_epoch
#endif
(str)
char **str;
/*<      subroutine fcreate_orbit_epoch(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_orbit_epoch(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source_type__
#else
fcreate_source_type
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_source_type(ptr_ch(str)//(char *)0, >*/
/*<                                     ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_source_type(*str,
		     *str2);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source_name__
#else
fcreate_source_name
#endif
(str)
char **str;
/*<      subroutine fcreate_source_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_source_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ra__
#else
fcreate_ra
#endif
(str)
char **str;
/*<      subroutine fcreate_ra(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_ra(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_iau_name__
#else
fcreate_iau_name
#endif
(str)
char **str;
/*<      subroutine fcreate_IAU_name(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_IAU_name(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_dec__
#else
fcreate_dec
#endif
(str)
char **str;
/*<      subroutine fcreate_dec(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_dec(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ref_coord_frame__
#else
fcreate_ref_coord_frame
#endif
(str)
char **str;
/*<      subroutine fcreate_ref_coord_frame(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_ref_coord_frame(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source_position_ref__
#else
fcreate_source_position_ref
#endif
(str)
char **str;
/*<      subroutine fcreate_source_position_ref(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_source_position_ref(*str);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source_position_epoch__
#else
fcreate_source_position_epoch
#endif
(str)
char **str;
/*<      subroutine fcreate_source_position_epoch(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_source_position_epoch(*str);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_ra_rate__
#else
fcreate_ra_rate
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_ra_rate(ptr_ch(str)//(char *)0, >*/
/*<                                 ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_ra_rate(*str,
		 *str2);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_dec_rate__
#else
fcreate_dec_rate
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_dec_rate(ptr_ch(str)//(char *)0, >*/
/*<                                  ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_dec_rate(*str,
		  *str2);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_velocity_wrt_lsr__
#else
fcreate_velocity_wrt_lsr
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_velocity_wrt_LSR(ptr_ch(str)//(char *)0, >*/
/*<                                          ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_velocity_wrt_LSR(*str,
			  *str2);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_source_model__
#else
fcreate_source_model
#endif
(str, str2, str3, str4, str5, str6, str7, str8, str9,str10,str11,str12,str13)
char **str;
char **str2;
char **str3;
char **str4;
char **str5;
char **str6;
char **str7;
char **str8;
char **str9;
char **str10;
char **str11;
char **str12;
char **str13;
/*<      subroutine fcreate_source_model(ptr_ch(str)//(char *)0, >*/
/*<                                      ptr_ch(str2)//(char *)0, >*/
/*<                                      ptr_ch(str3)//(char *)0, >*/
/*<                                      ptr_ch(str4)//(char *)0, >*/
/*<                                      ptr_ch(str5)//(char *)0, >*/
/*<                                      ptr_ch(str6)//(char *)0, >*/
/*<                                      ptr_ch(str7)//(char *)0, >*/
/*<                                      ptr_ch(str8)//(char *)0, >*/
/*<                                      ptr_ch(str9)//(char *)0, >*/
/*<                                      ptr_ch(str10)//(char *)0, >*/
/*<                                      ptr_ch(str11)//(char *)0, >*/
/*<                                      ptr_ch(str12)//(char *)0, >*/
/*<                                      ptr_ch(str13)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4 >*/
/*<      character*(*) str5; >*/
/*<      character*(*) str6; >*/
/*<      character*(*) str7 >*/
/*<      character*(*) str8; >*/
/*<      character*(*) str9; >*/
/*<      character*(*) str10; >*/
/*<      character*(*) str11; >*/
/*<      character*(*) str12; >*/
/*<      character*(*) str13; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */
/*   character*(*) str3      - string */
/*   character*(*) str4      - string */
/*   character*(*) str5      - string */
/*   character*(*) str6      - string */
/*   character*(*) str7      - string */
/*   character*(*) str8      - string */
/*   character*(*) str9      - string */
/*   character*(*) str10     - string */
/*   character*(*) str11     - string */
/*   character*(*) str12     - string */
/*   character*(*) str13     - string */

/* output: */
/*   VEX format */
/* */
{

  create_source_model(*str,
		      *str2,
		      *str3,
		      *str4,
		      *str5,
		      *str6,
		      *str7,
		      *str8,
		      *str9,
		      *str10,
		      *str11,
		      *str12,
		      *str13);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_vsn__
#else
fcreate_vsn
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_vsn(ptr_ch(str)//(char *)0, >*/
/*<                          ptr_ch(str2)//(char *)0, >*/
/*<                          ptr_ch(str3)//(char *)0, >*/
/*<                          ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_vsn(*str,
	     *str2,
	     *str3,
	     *str4);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanin_def__
#else
fcreate_fanin_def
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_fanin_def(ptr_ch(str)//(char *)0, >*/
/*<                                   ptr_ch(str2)//(char *)0, >*/
/*<                                   ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_fanin_def(*str,
		   *str2,
		   *str3);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanin_def_list__
#else
fcreate_fanin_def_list
#endif
(str)
char **str;
/*<      subroutine fanin_def_list(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_fanin_def_list(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanout_def_subpass__
#else
fcreate_fanout_def_subpass
#endif
(str)
char **str;
/*<      subroutine fanout_def_subpass(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_fanout_def_subpass(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanout_def_headstack__
#else
fcreate_fanout_def_headstack
#endif
(str)
char **str;
/*<      subroutine fanout_def_headstack(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_fanout_def_headstack(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanout_trksid_list__
#else
fcreate_fanout_trksid_list
#endif
(str)
char **str;
/*<      subroutine fanout_trksid_list(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_fanout_trksID_list(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_fanout_bitstream_list__
#else
fcreate_fanout_bitstream_list
#endif
(str)
char **str;
/*<      subroutine fanout_bitstream_list(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_fanout_bitstream_list(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_track_frame_format__
#else
fcreate_track_frame_format
#endif
(str)
char **str;
/*<      subroutine fcreate_track_frame_format(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_track_frame_format(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_data_modulation__
#else
fcreate_data_modulation
#endif
(str)
char **str;
/*<      subroutine fcreate_data_modulation(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_data_modulation(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_vlba_frmtr_sys_trk__
#else
fcreate_vlba_frmtr_sys_trk
#endif
(str, str2, str3, str4) 
char **str;
char **str2;
char **str3;
char **str4;
/*<      subroutine fcreate_vlba_frmtr_sys_trk(ptr_ch(str)//(char *)0, >*/
/*<                                            ptr_ch(str2)//(char *)0, >*/
/*<                                            ptr_ch(str3)//(char *)0, >*/
/*<                                            ptr_ch(str4)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/
/*<      character*(*) str4; >*/

/* ... */

/* input: */
/*   character*(*) str       - string  */
/*   character*(*) str2      - string  */
/*   character*(*) str3      - string  */
/*   character*(*) str4      - string  */

/* output: */
/*   VEX format */
/* */
{
  create_vlba_frmtr_sys_trk(*str,
			    *str2,
			    *str3,
			    *str4);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_vlba_trnsprt_sys_trk__
#else
fcreate_vlba_trnsprt_sys_trk
#endif
(str, str2)
char **str;
char **str2;
/*<      subroutine fcreate_vlba_trnsprt_sys_trk(ptr_ch(str)//(char *)0, >*/
/*<                                              ptr_ch(str2)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */
/*   character*(*) str2      - string */

/* output: */
/*   VEX format */
/* */
{
  create_vlba_trnsprt_sys_trk(*str,
			      *str2);

  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_s2_recording_mode__
#else
fcreate_s2_recording_mode
#endif
(str)
char **str;
/*<      subroutine fcreate_s2_recording_mode(ptr_ch(str)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/

/* ... */

/* input: */
/*   character*(*) str       - string */

/* output: */
/*   VEX format */
/* */
{
  create_s2_recording_mode(*str);
  return;
}
/* ----------------------------------------------------------------------- */
void
#ifdef F2C
fcreate_s2_data_source__
#else
fcreate_s2_data_source
#endif
(str, str2, str3) 
char **str;
char **str2;
char **str3;
/*<      subroutine fcreate_s2_data_source(ptr_ch(str)//(char *)0, >*/
/*<                                        ptr_ch(str2)//(char *)0, >*/
/*<                                        ptr_ch(str3)//(char *)0) >*/
/*<      implicit none >*/
/*<      character*(*) str; >*/
/*<      character*(*) str2; >*/
/*<      character*(*) str3; >*/

/* ... */

/* input: */
/*   character*(*) str       - string 1 */
/*   character*(*) str2      - string 2 */
/*   character*(*) str3      - string 3 */

/* output: */
/*   VEX format */
/* */
{
  create_s2_data_source(*str,
			*str2,
			*str3);

  return;
}
