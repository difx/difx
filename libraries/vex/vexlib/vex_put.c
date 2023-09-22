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
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <stdlib.h>

#include "vex.h"
#include "y.tab.h"

#define TRUE 1
#define FALSE 0

static int blk=0;
static char *def_str;
static char *string[20];         /* string saves strings while a q_list */
static char *scan_str;           /* or other list is being built. */
static int block_flag = 0;
static int def_flag = 0;
static int in_def_flag = 0;
static int scan_flag = 0;
static int in_scan_flag = 0;
static int qref_flag = 0;
static struct llist *def_list=NULL;      /* To help build a def list */
static struct llist *qref_list=NULL;     /* To help build a ref list */
static struct llist *b_list=NULL;        /* To help build a block list */
static struct llist *q_list=NULL;        /* To help build a list */
static struct llist *q2_list=NULL;       /* To help build a list when
					    two (2) lists are needed. */
static struct llist *version_list=NULL;

/* error check statment */
char *err1 = "**** Statement"; 
char *err2 = "ERROR in"; 

extern FILE * yyin;
extern struct vex *vex_ptr;
char *filename;
FILE *fp;

/*********************************************************************
 * functions:                                                         *
 * create_vex(int screen_or_file) - Create a VEX format file to       *
 *                                  zero(0) screen or one(1) disk.    *
 *                                                                    *
 * create_block(char *str) - Create a block e.g. "GLOBAL", "ANTENNA", *
 *                           ..etc. (Legal names can be found in VEX  *
 *                                   documention)                     *
 *                                                                    *
 * create_def(char *str) - Create a def block.                        *
 * str: def name - Self expanatory.                                   *
 *                                                                    *
 * end_def() - End the current def.                                   *
 * This function only needs to be called if you want to add a         *
 * comment after the enddef within a block.                           *
 *                                                                    *
 * create_ref(char *str, char *str2) Create a ref.                    *
 * str:  Block name - Self explanatory.                               *
 * str2: Keyname    - This is a defined 'def'.                        *
 *                                                                    *
 * create_qref(char *str, char *str2) Create a ref.                   *
 * str:  Block name - Self explanatory.                               *
 * str2: Keyname    - This is a defined 'def'.                        *
 * str3: Qualifiers - If primitive block is not MODE this should be   *
 *                    set to null. If it is MODE then a string of     *
 *                    stations can be give: "EF SC VLBA  ..... etc.   *
 *                                                                    *
 * create_qref_qualifier(char *str)    Create a ref qualifier.        *
 * str:  Qualifiers - If primitive block in qref is not MODE then     *
 *                    qualifier should be set to null. If it is MODE  *
 *                    then a mode or station should be given.         *
 *                    You should end with a NULL.                     *
 *                                                                    *
 * create_external_ref(char *str, char *str2, char *str3)             *
 * str:  file name  - External file self explanatory.                 *
 * str2: block name - Self explanatory.                               *
 * str3: Key name   - Defined Keyword found in the block of an external*
 *                    file.                                           *
 *                                                                    *
 * create_version(char *str)                                          *
 * str: revision number - Self explanatory.                           *
 *                                                                    *
 * create_scan(char *str) - Create a scan block.                      *
 * str: scan name - Self expanatory.                                  *
 *                                                                    *
 * end_scan() - End the current scan.                                 *
 * This function only needs to be called if you want to add a         *
 * comment after an endscan within a $SCHED block.                    *
 *                                                                    *
 * create_comment(char *str, char *str2)                              *
 * str: trailing or header - 't' for trailing and anything else       *
 *                           using a separate line                    *
 * str2: comment - You must begin with a star(*).                     *
 * Examples:                 create_comment(" ","* This is Eflsberg");*
 *                           def EF        *  This is Eflsberg        *
 *                           create_comment("t","* This is Eflsberg");*
 *                           * This is Eflsberg                       *
 *                           def EF;                                  *
 *                                                                    *
 * create_start(char *str)                                            *
 * str: character string - date and time of first good data.          *
 * example: create_start("1999y263d0h15m00s")                         *
 *                                                                    *
 * create_mode(char *str)                                             *
 * str: character string - MODE keyword used in $MODE block.          *
 * example: create_mode("SX")                                         *
 *                                                                    *
 * create_source(char *str)                                           *
 * str: character string - SOURCE keyword used in $SOURCE block.      *
 * example: create_mode("HD123456")                                   *
 *                                                                    *
 * create_station(char *str, char *str2, char *str3, char *str4,      *
 *	          char *str5, char *str6, char *str7, char *str8,     *
 *	          char *str9)                                         *
 * create_station_drive_list(char *str) create a drive list.          *
 * str: drive                                                         *
 * create_data_transfer(char *str, char *str2, char *str3, char *str4,*
 *	          char *str5, char *str6, char *str7, *str 8)         *
 *                                                                    *
 * All the calls to these functions are basically the same the        *
 * definitions can be found in the "VEX File Definition/Example" doc. *
 * and the VEX Parameters Tables                                      *
 * ------------------------------------------------------------------ *
 * These are all purpose routines that help utilities build lists.    *
 * ------------------------------------------------------------------ *
 * NONE;                                                              *
 * ------------------------------------------------------------------ *
 *                                                                    *
 * Audit:                                                             *
 * June 1999: First instance for all create functions.(rdg)           *
 * Sept 1999: 1.) Removed all create list builder called by all create*
 *                functions.                                          *
 *            2.) Changed the reading of strings to reading of one    *
 *                value or mode at a time. (rdg)                      *
 *                example: call("EF SC AO") changed this              *
 *                         to this call("EF"), call("SC"), call(NULL) *
 *                                                                    *
 *********************************************************************/
/*-------------------------------------------------------------------*/
void *
create_vex(char *str) /* str = filename) */
{
  /* Get the last block_listing produced */
  if(in_def_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_DEF,
		          make_def(def_str,qref_list)));
      b_list=add_list(b_list,
		      make_block(blk,def_list));
    }
  else if(in_scan_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_SCAN,
			  make_def(scan_str,qref_list)));
      b_list=add_list(b_list,
		      make_block(blk,def_list));
    }
  else if(def_flag || scan_flag)
    {
      b_list=add_list(b_list,
		      make_block(blk,def_list));
    }
  else if(qref_flag) 
    {
      b_list=add_list(b_list,
		      make_block(blk,qref_list));
    }
  else if(blk != 0)
    {
      b_list=add_list(b_list,
		      make_block(blk,qref_list));
    }
  else if(blk == 0)
    { 
      b_list=NULL;
    }

  /* Print it to a file or just the screen */
  if(str!=NULL && strlen(str)!=0) 
    {
      filename = str;
      print_vex(make_vex(version_list,b_list));
      fclose(fp);
    }
  else
    {
      print_vex(make_vex(version_list,b_list));
    }
  qref_list=NULL;
  def_list=NULL;
  q_list=NULL;
  b_list=NULL;
  qref_flag=0;
  def_flag=0;
  scan_flag =0;
  in_def_flag=0;
  in_scan_flag=0;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_block(char *str)
{
  char *block_id;

  if (block_flag)
    {
      if(qref_flag && !def_flag && !scan_flag)
	{
	  b_list=add_list(b_list,
			  make_block(blk,qref_list));
	}
      else if(in_def_flag)
	{
	  def_list = add_list(def_list,
			      make_lowl(T_DEF,
			      make_def(def_str,qref_list)));
	  b_list=add_list(b_list,
			  make_block(blk,def_list));
	}
      else if(in_scan_flag)
	{
	  def_list = add_list(def_list,
			      make_lowl(T_SCAN,
			      make_def(scan_str,qref_list)));
	  b_list=add_list(b_list,
			  make_block(blk,def_list));
	}
      else
	{
	    b_list=add_list(b_list,
			    make_block(blk,def_list));
	}
    }
  qref_list=NULL;
  def_list=NULL;
  qref_flag=0;
  def_flag=0;
  scan_flag =0;
  in_def_flag=0;
  in_scan_flag=0;
  block_id=(char *)strdup(str);
  blk=block2int(block_id);
  block_flag = 1;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_def(char *str)
{
  if (in_def_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_DEF,
			  make_def(def_str,qref_list)));
    }
  else if(!def_flag && qref_list!=NULL) {
    def_list=qref_list;
  }
  qref_list=NULL;
  qref_flag=0;
  def_str=(char *)strdup(str);
  def_flag = 1;
  in_def_flag= 1;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
end_def()
{
  if (in_def_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_DEF,
			  make_def(def_str,qref_list)));
    }
  qref_list=NULL;
  qref_flag=0;
  def_str=NULL;
  in_def_flag = 0;
  return(NULL);
}

/*-------------------------------------------------------------------*/
void *
create_ref(char *str, char *str2)
{
  char *primitive,*keyword;

  primitive=str;
  keyword=(char *)strdup(str2);
  qref_list = add_list(qref_list, 
		       make_lowl(T_REF,
		       make_qref(block2int(primitive),
		       keyword,
		       NULL)));
  qref_flag = 1;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_qref(char *str, char *str2)
{
  string[0]=(char *)strdup(str);
  string[1]=(char *)strdup(str2);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_qref_qualifier(char *str)
{
  char *blockname,*keyword;
  char *stationkey;
 
  if(str!=NULL && strlen(str)!=0)
    {
      stationkey=(char *)strdup(str);
      q_list = add_list(q_list,stationkey);
    }
  else
    {
      blockname=string[0];
      keyword=(char *)strdup(string[1]);

      qref_list = add_list(qref_list, 
			   make_lowl(T_REF,
			   make_qref(block2int(blockname),
			   keyword,
                           q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_external_ref(char *str, char *str2, char *str3)
{
  char *primitive,*keyword,*ext_file;

  ext_file=(char *)strdup(str);
  primitive=str2;
  keyword=(char *)strdup(str3);
  qref_list = add_list(qref_list,make_lowl(T_REF,
				 make_external(ext_file,
                                 block2int(primitive),
                                 keyword)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_version(char *str)
{
  char *version_number;

  version_list = NULL;
  version_number=(char *)strdup(str);
  version_list = add_list(version_list,
			  make_lowl(T_VEX_REV, 
			  make_dvalue(version_number,NULL)));
  b_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_scan(char *str)
{

  if (in_scan_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_SCAN,
			  make_def(scan_str,qref_list)));
    }
  else if(!scan_flag && qref_list!=NULL) {
    def_list=qref_list;
  }

  qref_list=NULL;
  qref_flag = 0;
  q_list=NULL;
  scan_str=(char *)strdup(str);
  scan_flag = 1;
  in_scan_flag = 1;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
end_scan()
{

  if (in_scan_flag)
    {
      def_list = add_list(def_list,
			  make_lowl(T_SCAN,
			  make_def(scan_str,qref_list)));
    }
  qref_list=NULL;
  qref_flag = 0;
  q_list=NULL;
  scan_str=NULL;
  in_scan_flag = 0;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_comment(char *str, char *str2)
{
  char *comment;
  static struct llist **list;

  comment=(char *)strdup(str2);

  if(!block_flag)
    list=&version_list;
  else if(in_def_flag||in_scan_flag||(!def_flag&&!scan_flag)) {
    list=&qref_list;
    qref_flag = 1;
  } else
    list=&def_list;

  if(*str=='t')
    {
      *list = add_list(*list,make_lowl(T_COMMENT_TRAILING,
				     comment));
    }
  else
    {
      *list = add_list(*list,make_lowl(T_COMMENT,
				     comment));
    }

  return(NULL);
}
/*-------------------------------------------------------------------*/
/* SCHEDULE block builders                                           */
/*-------------------------------------------------------------------*/
void *
create_start(char *str)
{
  char *scan_start;

  scan_start=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_START,scan_start));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_mode(char *str)
{
  char *scan_mode;

  scan_mode=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_MODE,scan_mode));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_source(char *str)
{
  char *scan_source;

  scan_source=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SOURCE,scan_source));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_station(char *str, char *str2, char *str3, char *str4,
	       char *str5, char *str6, char *str7, char *str8,
	       char *str9)
{
  string[0] = (char *)strdup(str);
  string[1] = (char *)strdup(str2);
  string[2] = (char *)strdup(str3);
  string[3] = (char *)strdup(str4);
  string[4] = (char *)strdup(str5);
  string[5] = (char *)strdup(str6);
  string[6] = (char *)strdup(str7);
  string[7] = (char *)strdup(str8);
  string[8] = (char *)strdup(str9);
  q_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_station_drive_list(char *str)
{
  char *station_key;
  char *station_start_value;
  char *station_start_units;
  char *station_stop_value;
  char *station_stop_units;
  char *station_start_pos_value;
  char *station_start_pos_units;
  char *station_pass;
  char *station_point_sector;
  char *station_drive;

  if(str!=NULL && strlen(str)!=0)
    {
      station_drive=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(station_drive,NULL));
    }
  else
    {
      if(q_list==NULL) q_list = add_list(q_list,make_dvalue(NULL,NULL));
      station_key=string[0];
      station_start_value=string[1];
      station_start_units=string[2];
      station_stop_value=string[3];
      station_stop_units=string[4];
      station_start_pos_value=string[5];
      station_start_pos_units=string[6];
      station_pass=string[7];
      station_point_sector=string[8];
      qref_list = add_list(qref_list,make_lowl(T_STATION,
		   		     make_station(station_key,
				     make_dvalue(station_start_value,
                                                 station_start_units),
				     make_dvalue(station_stop_value,
						 station_stop_units),
				     make_dvalue(station_start_pos_value,
						 station_start_pos_units),
				     station_pass, station_point_sector,
				     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_data_transfer(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6, char *str7, char *str8)
{
  char *data_transfer_key;
  char *data_transfer_method;
  char *data_transfer_destination;
  char *data_transfer_start_value;
  char *data_transfer_start_units;
  char *data_transfer_stop_value;
  char *data_transfer_stop_units;
  char *data_transfer_options;

  data_transfer_key=(char *)strdup(str);
  data_transfer_method=(char *)strdup(str2);
  data_transfer_destination=(char *)strdup(str3);
  data_transfer_start_value=(char *)strdup(str4);
  data_transfer_start_units=(char *)strdup(str5);
  data_transfer_stop_value=(char *)strdup(str6);
  data_transfer_stop_units=(char *)strdup(str7);
  data_transfer_options=(char *)strdup(str8);

  qref_list = add_list(qref_list,
	      make_lowl(T_DATA_TRANSFER,
		        make_data_transfer(data_transfer_key,
		         data_transfer_method,
		         data_transfer_destination,
		         make_dvalue(data_transfer_start_value,
			             data_transfer_start_units),
			 make_dvalue(data_transfer_stop_value,
			             data_transfer_stop_units),
			 data_transfer_options)));
  q_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* ANTENNA block builders                                            */
/*-------------------------------------------------------------------*/
void *
create_antenna_diam(char *str, char *str2)
{
  char *diam_value,*diam_units;

  if(str==NULL || strlen(str) ==0 ||
     str2==NULL|| strlen(str2)==0)
    {
      printf("%s \'antenna_diam\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      diam_value=(char *)strdup(str);
      diam_units=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_ANTENNA_DIAM,
				     make_dvalue(diam_value,diam_units)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_axis_type(char *str, char *str2)
{
  char *axis_type_az,*axis_type_el;

  if(str==NULL || strlen(str) ==0 ||
     str2==NULL|| strlen(str2)==0)
    {
      printf("%s \'axis_type\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      axis_type_az=(char *)strdup(str);
      axis_type_el=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_AXIS_TYPE,
				     make_axis_type(axis_type_az,
				     axis_type_el)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_axis_offset(char *str, char *str2)
{
  char *axis_offset_value,*axis_offset_units;

  if(str==NULL || strlen(str) ==0 ||
     str2==NULL|| strlen(str2)==0)
    {
      printf("%s \'axis_offset\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      axis_offset_value=(char *)strdup(str);
      axis_offset_units=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_AXIS_OFFSET,
				     make_dvalue(axis_offset_value,
                                     axis_offset_units)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_antenna_motion(char *str, char *str2, char *str3, char *str4,
	      char *str5)
{
  char *motion_type;
  char *slew_value,*slew_units;
  char *settle_value,*settle_units;


  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0 ||
     str3==NULL || strlen(str3)==0 ||
     str4==NULL || strlen(str4)==0 ||
     str5==NULL || strlen(str5)==0)
    {
      printf("%s \'antenna_motion\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      motion_type=(char *)strdup(str);
      slew_value=(char *)strdup(str2);
      slew_units=(char *)strdup(str3);
      settle_value=(char *)strdup(str4);
      settle_units=(char *)strdup(str5);
      qref_list = add_list(qref_list,make_lowl(T_ANTENNA_MOTION,
				     make_antenna_motion(motion_type,
				     make_dvalue(slew_value,slew_units),
				     make_dvalue(settle_value,settle_units))));
  }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_pointing_sector(char *str, char *str2, char *str3, char *str4,
		       char *str5, char *str6, char *str7, char *str8,
		       char *str9, char *str10, char *str11)
{
  char *sector;
  char *axis1;
  char *lolimit1_value;
  char *lolimit1_units;
  char *hilimit1_value;
  char *hilimit1_units;
  char *axis2;
  char *lolimit2_value;
  char *lolimit2_units;
  char *hilimit2_value;
  char *hilimit2_units;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0 ||
     str3==NULL || strlen(str3)==0 ||
     str4==NULL || strlen(str4)==0 ||
     str5==NULL || strlen(str5)==0 ||
     str6==NULL || strlen(str6)==0 ||
     str7==NULL || strlen(str7)==0 ||
     str8==NULL || strlen(str8)==0 ||
     str9==NULL || strlen(str9)==0 ||
     str10==NULL || strlen(str10)==0 ||
     str11==NULL || strlen(str11)==0 )
       
    {
      printf("%s \'pointing_sector\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      sector=(char *)strdup(str);
      axis1=(char *)strdup(str2);
      lolimit1_value=(char *)strdup(str3);
      lolimit1_units=(char *)strdup(str4);
      hilimit1_value=(char *)strdup(str5);
      hilimit1_units=(char *)strdup(str6);
      axis2=(char *)strdup(str7);
      lolimit2_value=(char *)strdup(str8);
      lolimit2_units=(char *)strdup(str9);
      hilimit2_value=(char *)strdup(str10);
      hilimit2_units=(char *)strdup(str11);

      qref_list = add_list(qref_list,make_lowl(T_POINTING_SECTOR,
	 			     make_pointing_sector(sector, axis1,
				     make_dvalue(lolimit1_value,
						 lolimit1_units),
				     make_dvalue(hilimit1_value,
						 hilimit1_units),
				     axis2,
				     make_dvalue(lolimit2_value,
						 lolimit2_units),
				     make_dvalue(hilimit2_value,
						 hilimit2_units))));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* BBC block builders                                                */
/*-------------------------------------------------------------------*/
void *
create_bbc_assign(char *str, char *str2, char *str3)
{
  char *bbc_id, *bbc_physical_id, *bbc_if_id;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0 ||
     str3==NULL || strlen(str3)==0) 
    {
      printf("%s \'bbc_assign\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      bbc_id=(char *)strdup(str);
      bbc_physical_id=(char *)strdup(str2);
      bbc_if_id=(char *)strdup(str3);

      qref_list = add_list(qref_list,make_lowl(T_BBC_ASSIGN,
	  			   make_bbc_assign(bbc_id,
				   make_dvalue(bbc_physical_id,NULL),
				   bbc_if_id)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* CLOCK block builders                                              */
/*-------------------------------------------------------------------*/
void *
create_clock(char *str, char *str2, char *str3, char *str4, char *str5)
{
  char *valid_from, *clock_early_value, *clock_early_units,
       *clock_early_epoch, *rate;

  if(str2==NULL || strlen(str2)==0 ||
     str3==NULL || strlen(str3)==0) 
    {
      printf("%s \'clock\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      if(str==NULL || strlen(str)==0)
	{
	  if(str4==NULL || strlen(str4)==0 ||
	     str5==NULL || strlen(str5)==0)
	    {
	      clock_early_value=(char *)strdup(str2);
              clock_early_units=(char *)strdup(str3);
	      qref_list = add_list(qref_list,make_lowl(T_CLOCK_EARLY,
					     make_clock_early(NULL,
					     make_dvalue(clock_early_value,
					                 clock_early_units),
					     NULL,NULL)));
	    }
	  else
	    {
	      clock_early_value=(char *)strdup(str2);
              clock_early_units=(char *)strdup(str3);
	      clock_early_epoch=(char *)strdup(str4);
	      rate=(char *)strdup(str5);
	      qref_list = add_list(qref_list,make_lowl(T_CLOCK_EARLY,
					     make_clock_early(NULL,
					     make_dvalue(clock_early_value,
					                 clock_early_units),
 				             clock_early_epoch,
                 		             make_dvalue(rate,NULL))));
	    }
	}
      else if(str4==NULL || strlen(str4)==0 ||
	      str5==NULL || strlen(str5)==0)
	{ 
	  valid_from=(char *)strdup(str);
	  clock_early_value=(char *)strdup(str2);
	  clock_early_units=(char *)strdup(str3);
	  qref_list = add_list(qref_list,make_lowl(T_CLOCK_EARLY,
	   			         make_clock_early(valid_from,
				         make_dvalue(clock_early_value,
						     clock_early_units),
				         NULL,NULL)));
	}
      else
	{
	  valid_from=(char *)strdup(str);
	  clock_early_value=(char *)strdup(str2);
	  clock_early_units=(char *)strdup(str3);
	  clock_early_epoch=(char *)strdup(str4);
	  rate=(char *)strdup(str5);
	  qref_list = add_list(qref_list,make_lowl(T_CLOCK_EARLY,
				         make_clock_early(valid_from,
				         make_dvalue(clock_early_value,
					             clock_early_units),
				         clock_early_epoch,
				         make_dvalue(rate,NULL))));
	}
    }  
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* DAS block builders                                                */
/*-------------------------------------------------------------------*/
void *
create_record_transport_type(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'record_transport_type\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_RECORD_TRANSPORT_TYPE,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_electronics_rack_type(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'electronics_rack_type\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_ELECTRONICS_RACK_TYPE,s1));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_number_drives(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'number_drives\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_NUMBER_DRIVES,
		  		     make_dvalue(s1,NULL)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_headstack(char *str, char *str2, char *str3)
{
  char *headstack, *rw, *drive_offset;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'headstack\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      headstack=(char *)strdup(str);
      rw=(char *)strdup(str2);
      drive_offset=(char *)strdup(str3);
      qref_list = add_list(qref_list,make_lowl(T_HEADSTACK,
				     make_headstack(
				     make_dvalue(headstack,NULL),
				     rw,
				     make_dvalue(drive_offset,NULL))));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_record_density(char *str, char *str2)
{
  char *type, *speed;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'record_density\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      type=(char *)strdup(str);
      speed=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_RECORD_DENSITY,
					       make_dvalue(type,speed)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_tape_length(char *str, char *str2, char *str3, char *str4)
{
  char *dur_value, *dur_units, *speed, *tape;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'tape_length\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else if(str3==NULL || strlen(str3)==0 ||
	  str4==NULL || strlen(str4)==0)
    {
      dur_value=(char *)strdup(str);
      dur_units=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_TAPE_LENGTH,
			     	     make_tape_length(
                                     make_dvalue(dur_value,dur_units),
                                     NULL,NULL)));
    }
  else
    {
      dur_value=(char *)strdup(str);
      dur_units=(char *)strdup(str2);
      speed=(char *)strdup(str3);
      tape=(char *)strdup(str4);
      qref_list = add_list(qref_list,make_lowl(T_TAPE_LENGTH,
			     	     make_tape_length(
                                     make_dvalue(dur_value,dur_units),
                                     speed,
				     make_dvalue(tape,NULL))));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_recording_system_id(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'recording_system_id\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_RECORDING_SYSTEM_ID,
		 		     make_dvalue(s1,NULL)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_tape_motion(char *str, char *str2, char *str3, char *str4,
                   char *str5, char *str6, char *str7)
{
  char *s1, *s2, *s3, *s4, *s5, *s6, *s7; 

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'tape_motion\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      if(str2==NULL || strlen(str2)==0 ||
	 str3==NULL || strlen(str3)==0)
	{
	  s1=(char *)strdup(str);
	  qref_list = add_list(qref_list,make_lowl(T_TAPE_MOTION,
		 		         make_tape_motion(s1,
				         NULL,NULL,NULL)));
	}
      else if(str4==NULL || strlen(str4)==0 ||
	      str5==NULL || strlen(str5)==0 ||
	      str6==NULL || strlen(str6)==0 ||
	      str7==NULL || strlen(str7)==0)
	{
	  s1=(char *)strdup(str);
	  s2=(char *)strdup(str2);
	  s3=(char *)strdup(str3);
	  qref_list = add_list(qref_list,make_lowl(T_TAPE_MOTION,
				         make_tape_motion(s1,
				         make_dvalue(s2,s3),
				         NULL,NULL)));
	}
      else
	{
	  s1=(char *)strdup(str);
	  s2=(char *)strdup(str2);
	  s3=(char *)strdup(str3);
	  s4=(char *)strdup(str4);
	  s5=(char *)strdup(str5);
	  s6=(char *)strdup(str6);
	  s7=(char *)strdup(str7);
	  qref_list = add_list(qref_list,make_lowl(T_TAPE_MOTION,
				         make_tape_motion(s1,
				         make_dvalue(s2,s3),
				         make_dvalue(s4,s5),
				         make_dvalue(s6,s7))));
	}
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_tape_control(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'tape_control\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_TAPE_CONTROL,s1));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* EOP block builders                                                */
/*-------------------------------------------------------------------*/
void *
create_tai_utc(char *str, char *str2)
{
  char *s1,*s2;

  if(str==NULL || strlen(str)==0 || 
     str2==NULL || strlen(str2)==0)
    {
      printf("%s \'tai_utc\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_TAI_UTC,
				     make_dvalue(s1,s2)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_a1_tai(char *str, char *str2)
{
  char *s1,*s2;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL  || strlen(str2)==0)
    {
      printf("%s \'a1_tai\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_A1_TAI,
				     make_dvalue(s1,s2)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_eop_ref_epoch(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'eop_ref_epoch\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EOP_REF_EPOCH,s1));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_num_eop_points(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'num_eop_points\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_NUM_EOP_POINTS,
				     make_dvalue(s1,NULL)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_eop_interval(char *str, char *str2)
{
  char *s1,*s2;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0)
    {
      printf("%s \'eop_interval\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_EOP_INTERVAL,
				     make_dvalue(s1,s2)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_ut1_utc(char *str, char *str2)
{
  char *s1,*s2;

  if(str!=NULL && strlen(str)!=0 &&
     str2!=NULL && strlen(str2)!=0)
    {
      /* Create a list of things */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_UT1_UTC, q_list));
      q_list=NULL;
    }
  return(NULL);
} 
/*-------------------------------------------------------------------*/
void *
create_x_wobble(char *str, char *str2)
{
  char *s1,*s2;

  if(str!=NULL && strlen(str)!=0 &&
     str2!=NULL && strlen(str2)!=0)
    {
      /* Create a list of things */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_X_WOBBLE, q_list));
      q_list=NULL;
    }
  return(NULL);
} 
/*-------------------------------------------------------------------*/
void *
create_y_wobble(char *str, char *str2)
{
  char *s1,*s2;

  if(str!=NULL && strlen(str)!=0 &&
     str2!=NULL && strlen(str2)!=0)
    {
      /* Create a list of things */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_Y_WOBBLE, q_list));
      q_list=NULL;
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_nut_ref_epoch(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'nut_ref_epoch\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_NUT_REF_EPOCH,s1));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_num_nut_points(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'num_nut_points\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_NUM_NUT_POINTS,
				     make_dvalue(s1,NULL)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_nut_interval(char *str, char *str2)
{
  char *s1,*s2;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0)
    {
      printf("%s \'nut_interval\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_NUT_INTERVAL,
				     make_dvalue(s1,s2)));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_delta_psi(char *str, char *str2)
{
  char *s1,*s2;

  if(str!=NULL && strlen(str)!=0)
    {
      /* Create a list of things */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_DELTA_PSI, q_list));
      q_list=NULL;
    }
  return(NULL);
} 
/*-------------------------------------------------------------------*/
void *
create_delta_eps(char *str, char *str2)
{
  char *s1,*s2;

  if(str!=NULL && strlen(str)!=0)
    {
      /* Create a list of things */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_DELTA_EPS, q_list));
      q_list=NULL;
    }
  return(NULL);
} 
/*-------------------------------------------------------------------*/
void *
create_nut_model(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'nut_model\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_NUT_MODEL,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* EXPER block builders                                              */
/*-------------------------------------------------------------------*/
void *
create_exper_name(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'exper_name\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EXPER_NAME,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_exper_num(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'exper_num\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EXPER_NUM,
                                     make_dvalue(s1,NULL)));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_exper_description(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'exper_description\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EXPER_DESCRIPTION,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_exper_nominal_start(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'exper_nominal_start\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EXPER_NOMINAL_START,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_exper_nominal_stop(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'exper_nominal_stop\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_EXPER_NOMINAL_STOP,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_pi_name(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'pi_name\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_PI_NAME,s1));
    }
  return(NULL);
}
/*---------------------------------------------------------------------------*/
void *
create_pi_email(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'pi_email\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_PI_EMAIL,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_contact_name(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'contact_name\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_CONTACT_NAME,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_contact_email(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'contact_email\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_CONTACT_EMAIL,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_scheduler_name(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'scheduler_name\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_SCHEDULER_NAME,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_scheduler_email(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'scheduler_email\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_SCHEDULER_EMAIL,s1));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_target_correlator(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'target_correlator\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_TARGET_CORRELATOR,s1));
    }
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* FREQ block builders                                               */
/*-------------------------------------------------------------------*/
void *
create_chan_def(char *str, char *str2, char *str3, char *str4,
	        char *str5, char *str6, char *str7, char *str8,
		char *str9)
{
  if(str!=NULL && strlen(str)!=0)
    {
      string[0] = (char *)strdup(str);
    }
  else
    {
      string[0] = str;
    }
  string[1] = (char *)strdup(str2);
  string[2] = (char *)strdup(str3);
  string[3] = (char *)strdup(str4);
  string[4] = (char *)strdup(str5);
  string[5] = (char *)strdup(str6);
  string[6] = (char *)strdup(str7);
  string[7] = (char *)strdup(str8);
  string[8] = (char *)strdup(str9);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_chan_def_states(char *str)
{
  char *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9, *s10;

  if(str!=NULL && strlen(str)!=0)
    {
      s1=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(s1,NULL));
    }
  else
    {
      if(string[0]==NULL)
	{
	  s3=string[1];
	  s4=string[2];
	  s5=string[3];
	  s6=string[4];
	  s7=string[5];
	  s8=string[6];
	  s9=string[7];
	  s10=string[8];
	  qref_list = add_list(qref_list,make_lowl(T_CHAN_DEF,
				         make_chan_def(NULL,
				         make_dvalue(s3,s4),
				         s5,
				         make_dvalue(s6,s7),
				         s8, s9, s10,
				         q_list)));
	  q_list=NULL;
	}
      else
	{
	  s2=string[0];
	  s3=string[1];
	  s4=string[2];
	  s5=string[3];
	  s6=string[4];
	  s7=string[5];
	  s8=string[6];
	  s9=string[7];
	  s10=string[8];
	  qref_list = add_list(qref_list,make_lowl(T_CHAN_DEF,
				         make_chan_def(s2,
				         make_dvalue(s3,s4),
				         s5,
				         make_dvalue(s6,s7),
				         s8, s9, s10,
				         q_list)));
	  q_list=NULL;
	}
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_sample_rate(char *str, char *str2)
{
  char *s1,*s2;

  if(str==NULL || strlen(str)==0 ||
     str2==NULL || strlen(str2)==0)
    {
      printf("%s \'sample_rate\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      qref_list = add_list(qref_list,make_lowl(T_SAMPLE_RATE,
				     make_dvalue(s1,s2)));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_bits_per_sample(char *str)
{
  char *s1;

  if(str==NULL || strlen(str)==0)
    {
      printf("%s \'bits_per_sample\' %s %s block\n",
	     err1, err2, int2block(blk));
    }
  else
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_BITS_PER_SAMPLE,
				     make_dvalue(s1,NULL)));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_switching_cycle(char *str)
{
  string[0] = (char *)strdup(str);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_cycle(char *str, char *str2)
{
  char *s1,*s2, *s3;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used by the previous string */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      s3=string[0];
      qref_list = add_list(qref_list,make_lowl(T_SWITCHING_CYCLE,
				     make_switching_cycle(s3,
			  	     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* HEAD_POS block builders                                           */
/*-------------------------------------------------------------------*/
void *
create_headstack_reference(char *str)
{
  string[0] = (char *)strdup(str);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_headstack_pos(char *str, char *str2)
{
  char *s1,*s2, *s3;

  if(str!=NULL && strlen(str)!=0 &&
     str2!=NULL && strlen(str2)!=0)
    {
      /* create a list of things to be used by the previous string */
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,make_dvalue(s1,s2));
    }
  else
    {
      s3=string[0];
      qref_list = add_list(qref_list,make_lowl(T_HEADSTACK_POS,
				     make_headstack_pos(
				     make_dvalue(s3,NULL),
				     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* IF block builders                                                 */
/*-------------------------------------------------------------------*/
void *
create_if_def(char *str, char *str2, char *str3, char *str4,
	      char *str5, char *str6, char *str7, char *str8,
	      char *str9, char *str10, char *str11)
{
  char *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9, *s10, *s11;

  if(str7==NULL || strlen(str7)==0 ||
     str8==NULL || strlen(str8)==0)
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      s4=(char *)strdup(str4);
      s5=(char *)strdup(str5);
      s6=(char *)strdup(str6);
      s11=(char *)strdup(str11);
      qref_list = add_list(qref_list,make_lowl(T_IF_DEF,
				     make_if_def(s1,s2,s3,
				     make_dvalue(s4,s5),
				     s6,
				     NULL,
				     NULL,
					s11)));
    }
  else if(str9==NULL || strlen(str9)==0 ||
	  str10==NULL || strlen(str10)==0)
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      s4=(char *)strdup(str4);
      s5=(char *)strdup(str5);
      s6=(char *)strdup(str6);
      s7=(char *)strdup(str7);
      s8=(char *)strdup(str8);
      s11=(char *)strdup(str11);
      qref_list = add_list(qref_list,make_lowl(T_IF_DEF,
				     make_if_def(s1,s2,s3,
				     make_dvalue(s4,s5),
				     s6,
				     make_dvalue(s7,s8),
				     NULL,
					s11)));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      s4=(char *)strdup(str4);
      s5=(char *)strdup(str5);
      s6=(char *)strdup(str6);
      s7=(char *)strdup(str7);
      s8=(char *)strdup(str8);
      s9=(char *)strdup(str9);
      s10=(char *)strdup(str10);
      s11=(char *)strdup(str11);
      qref_list = add_list(qref_list,make_lowl(T_IF_DEF,
				     make_if_def(s1,s2,s3,
				     make_dvalue(s4,s5),
				     s6,
				     make_dvalue(s7,s8),
				     make_dvalue(s9,s10),
					s11)));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* PASS_ORDER block builders                                         */
/*-------------------------------------------------------------------*/
void *
create_pass_order(char *str)
{
  char *s1;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used before placing it in memory */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,s1);
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_PASS_ORDER, q_list));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_s2_group_order(char *str)
{
  char *s1;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used before placing it in memory */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(s1,NULL));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_S2_GROUP_ORDER, q_list));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* PHASE_CAL_DETECT block builders                                   */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
void *
create_phase_cal_detect(char *str)
{
  string[0] = (char *)strdup(str);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_phase_cal_detect_list(char *str)
{
  char *s1, *s2;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used by the previous string */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(s1,NULL));
    }
  else
    {
      s2 = string[0];
      qref_list = add_list(qref_list,make_lowl(T_PHASE_CAL_DETECT,
				     make_phase_cal_detect(s2,
				     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* PROCEDURE block builders                                          */
/*-------------------------------------------------------------------*/
void *
create_tape_change(char *str, char *str2)
{
  char *s1,*s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_TAPE_CHANGE,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_headstack_motion(char *str, char *str2)
{
  char *s1,*s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_HEADSTACK_MOTION,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_new_source_command(char *str, char *str2)
{
  char *s1,*s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_NEW_SOURCE_COMMAND,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_new_tape_setup(char *str, char *str2)
{
  char *s1,*s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_NEW_TAPE_SETUP,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_setup_always(char *str, char *str2, char *str3)
{
  char *s1,*s2,*s3;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  qref_list = add_list(qref_list,make_lowl(T_SETUP_ALWAYS,
				 make_setup_always(s1,
				 make_dvalue(s2,s3))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_parity_check(char *str, char *str2, char *str3)
{
  char *s1,*s2,*s3;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  qref_list = add_list(qref_list,make_lowl(T_PARITY_CHECK,
				 make_parity_check(s1,
				 make_dvalue(s2,s3))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_tape_prepass(char *str, char *str2, char *str3)
{
  char *s1,*s2,*s3;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  qref_list = add_list(qref_list,make_lowl(T_TAPE_PREPASS,
				 make_tape_prepass(s1,
				 make_dvalue(s2,s3))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_preob_cal(char *str, char *str2, char *str3, char *str4)
{
  char *s1,*s2,*s3,*s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_PREOB_CAL,
				 make_preob_cal(s1,
				 make_dvalue(s2,s3),s4)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_midob_cal(char *str, char *str2, char *str3, char *str4)
{
  char *s1,*s2,*s3,*s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_MIDOB_CAL,
				 make_midob_cal(s1,
				 make_dvalue(s2,s3),s4)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_postob_cal(char *str, char *str2, char *str3, char *str4)
{
  char *s1,*s2,*s3,*s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_POSTOB_CAL,
				 make_postob_cal(s1,
				 make_dvalue(s2,s3),s4)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_procedure_name_prefix(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_PROCEDURE_NAME_PREFIX,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* ROLL block builders                                               */
/*-------------------------------------------------------------------*/
void *
create_roll(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_ROLL,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_roll_reinit_period(char *str, char *str2)
{
  char *s1,*s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_ROLL_REINIT_PERIOD,
				 make_dvalue(s1,s2)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_roll_inc_period(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_ROLL_INC_PERIOD,
				 make_dvalue(s1,NULL)));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_roll_def(char *str)
{
  char *s1;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used before placing it in memory */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(s1,NULL));
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_ROLL_DEF, q_list));
      q_list=NULL;
    }
  return(NULL);
}
/* -------------------------------------------------------------------*/
/* SCHEDULING_PARAMS block builders     using literals               */
/*-------------------------------------------------------------------*/
void *
create_literal(char *str)
{
  char *s1;

  if(str!=NULL && strlen(str)!=0)
    {
      /* Create a list of literals */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,s1);
    }
  else
    {
      qref_list = add_list(qref_list,make_lowl(T_LITERAL, q_list));
      q_list=NULL;
    }
  return(NULL);
} 
/*-------------------------------------------------------------------*/
/* SEFD_MODEL block builders                                         */
/*-------------------------------------------------------------------*/
void *
create_sefd_model(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SEFD_MODEL,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_sefd(char *str, char *str2, char *str3)
{
  string[0] = (char *)strdup(str);
  string[1] = (char *)strdup(str2);
  string[2] = (char *)strdup(str3);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_sefd_model_parameter(char *str)
{
  char *s1, *s2, *s3, *s4;

  if(str!=NULL && strlen(str)!=0)
    {
      /* create a list of things to be used before placing it in memory */
      s1=(char *)strdup(str);
      q_list = add_list(q_list,make_dvalue(s1,NULL));
    }
  else
    {
      s2=string[0];
      s3=string[1];
      s4=string[2];
      qref_list = add_list(qref_list,make_lowl(T_SEFD,
			             make_sefd(s2,
				     make_dvalue(s3,s4),
				     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
/* SITE block builders                                               */
/*-------------------------------------------------------------------*/
void *
create_site_name(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SITE_NAME,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_type(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SITE_TYPE,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_ID(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SITE_ID,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_position(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6)
{
  char *s1, *s2, *s3, *s4, *s5, *s6;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  s5=(char *)strdup(str5);
  s6=(char *)strdup(str6);
  qref_list = add_list(qref_list,make_lowl(T_SITE_POSITION,
				 make_site_position(make_dvalue(s1,s2),
				 make_dvalue(s3,s4),
				 make_dvalue(s5,s6))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_position_epoch(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SITE_POSITION_EPOCH,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_position_ref(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SITE_POSITION_REF,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_site_velocity(char *str, char *str2, char *str3, char *str4,
		     char *str5, char *str6)
{
  char *s1, *s2, *s3, *s4, *s5, *s6;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  s5=(char *)strdup(str5);
  s6=(char *)strdup(str6);
  qref_list = add_list(qref_list,make_lowl(T_SITE_VELOCITY,
				 make_site_velocity(
				 make_dvalue(s1,s2),
				 make_dvalue(s3,s4),
				 make_dvalue(s5,s6))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_horizon_map(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  q_list = add_list(q_list,make_dvalue(s1,s2));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_horizon_map_az()
{
  qref_list = add_list(qref_list,make_lowl(T_HORIZON_MAP_AZ, q_list));
  q_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_horizon_map_el()
{
  qref_list = add_list(qref_list,make_lowl(T_HORIZON_MAP_EL, q_list));
  q_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_zen_atmos(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_ZEN_ATMOS,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_ocean_load_vert(char *str, char *str2, char *str3, char *str4)
{
  char *s1, *s2, *s3, *s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_OCEAN_LOAD_VERT,
				 make_ocean_load_vert(
				 make_dvalue(s1,s2),
				 make_dvalue(s3,s4))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_ocean_load_horiz(char *str, char *str2, char *str3, char *str4)
{
  char *s1, *s2, *s3, *s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_OCEAN_LOAD_HORIZ,
				 make_ocean_load_horiz(
                                 make_dvalue(s1,s2),
				 make_dvalue(s3,s4))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_occupation_code(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_OCCUPATION_CODE,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_inclination(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_INCLINATION,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_eccentricity(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_ECCENTRICITY,
				 make_dvalue(s1,NULL)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_arg_perigee(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_ARG_PERIGEE,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_ascending_node(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_ASCENDING_NODE,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_mean_anomaly(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_MEAN_ANOMALY,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_semi_major_axis(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_SEMI_MAJOR_AXIS,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_mean_motion(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_MEAN_MOTION,
				 make_dvalue(s1,NULL)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_orbit_epoch(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_ORBIT_EPOCH,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* SOURCE block builders                                             */
/*-------------------------------------------------------------------*/
void *
create_source_type(char *str, char *str2)
{
  char *s1, *s2;

  if(str!=NULL && strlen(str)!=0)
    {
      s1=(char *)strdup(str);
      q_list = add_list(NULL,s1);
    }
  if(str2!=NULL && strlen(str2)!=0)
    {
      s2=(char *)strdup(str2);
      q_list = add_list(q_list,s2);
    }
  qref_list = add_list(qref_list,make_lowl(T_SOURCE_TYPE,q_list));
  q_list=NULL;
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_source_name(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SOURCE_NAME,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_ra(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_RA,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_IAU_name(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_IAU_NAME,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_dec(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_DEC,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_ref_coord_frame(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_REF_COORD_FRAME,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_source_position_ref(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SOURCE_POSITION_REF,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_source_position_epoch(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_SOURCE_POSITION_EPOCH,s1));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_ra_rate(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_RA_RATE,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_dec_rate(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_DEC_RATE,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_velocity_wrt_LSR(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  qref_list = add_list(qref_list,make_lowl(T_VELOCITY_WRT_LSR,
				 make_dvalue(s1,s2)));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_source_model(char *str, char *str2, char *str3, char *str4,
		    char *str5, char *str6, char *str7, char *str8,
		    char *str9, char *str10, char *str11, char *str12,
		    char *str13)

{
  char *s1, *s2, *s3, *s4, *s5, *s6;
  char *s7, *s8, *s9, *s10, *s11, *s12, *s13;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  s5=(char *)strdup(str5);
  s6=(char *)strdup(str6);
  s7=(char *)strdup(str7);
  s8=(char *)strdup(str8);
  s9=(char *)strdup(str9);
  s10=(char *)strdup(str10);
  s11=(char *)strdup(str11);
  s12=(char *)strdup(str12);
  s13=(char *)strdup(str13);
  qref_list = add_list(qref_list,make_lowl(T_SOURCE_MODEL,
				 make_source_model(make_dvalue(s1,NULL),
				 s2,
				 make_dvalue(s3,s4),
				 make_dvalue(s5,s6),
				 make_dvalue(s7,NULL),
				 make_dvalue(s8,s9),
				 make_dvalue(s10,s11),
				 make_dvalue(s12,s13))));
  return(NULL);
}  
/*-------------------------------------------------------------------*/
/* TAPELOG_OBS block builders                                        */
/*-------------------------------------------------------------------*/
void *
create_vsn(char *str, char *str2, char *str3, char *str4)
{
  char *s1, *s2, *s3, *s4;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  s3=(char *)strdup(str3);
  s4=(char *)strdup(str4);
  qref_list = add_list(qref_list,make_lowl(T_VSN,
				 make_vsn(make_dvalue(s1,NULL),
				 s2,s3,s4)));
  return(NULL);
}  

/*-------------------------------------------------------------------*/
/* TRACKS block builders                                             */
/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/
void *
create_fanin_def(char *str, char *str2, char *str3)
{

  string[0] = (char *)strdup(str);
  string[1] = (char *)strdup(str2);
  string[2] = (char *)strdup(str3);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_fanin_def_list(char *str)
{
  char *s1, *s2, *s3, *s4, *s5;

  if(str!=NULL && strlen(str)!=0)
    {
      s1=(char *)strdup(str);
      q_list = add_list(q_list,s1);
    }
  else
    {
      s3=string[0];
      s4=string[1];
      s5=string[2];
      qref_list = add_list(qref_list,make_lowl(T_FANIN_DEF,
				     make_fanin_def(s3,
			             make_dvalue(s4,NULL),
				     make_dvalue(s5,NULL),
				     q_list)));
      q_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_fanout_def_subpass(char *str)
{
  string[0] = (char *)strdup(str);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_fanout_def_headstack(char *str)
{
  string[1] = (char *)strdup(str);
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_fanout_bitstream_list(char *str)
{
  char *s1;

  if(str!=NULL && strlen(str)!=0)
    {
      s1=(char *)strdup(str);
      q_list = add_list(q_list,s1);
    }  
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_fanout_trksID_list(char *str)
{
  char *s1, *s2, *s3;

  if(str!=NULL && strlen(str)!=0)
    {

      s1=(char *)strdup(str);
      q2_list = add_list(q2_list,make_dvalue(s1,NULL));
    }
  else
    {
      s2=string[0];
      s3=string[1];
      qref_list = add_list(qref_list,make_lowl(T_FANOUT_DEF,
				     make_fanout_def(s2,
                                     q_list,
				     make_dvalue(s3,NULL),
				     q2_list)));
      q_list=NULL;
      q2_list=NULL;
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_track_frame_format(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_TRACK_FRAME_FORMAT,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_data_modulation(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_DATA_MODULATION,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_vlba_frmtr_sys_trk(char *str, char *str2, char *str3, char *str4)
{
  char *s1, *s2, *s3, *s4;

  if(str4==NULL || strlen(str4)==0)
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      qref_list = add_list(qref_list,make_lowl(T_VLBA_FRMTR_SYS_TRK,
				     make_vlba_frmtr_sys_trk(
                                     make_dvalue(s1,NULL),s2,
                                     make_dvalue(s3,NULL),
                                     NULL)));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      s4=(char *)strdup(str4);
      qref_list = add_list(qref_list,make_lowl(T_VLBA_FRMTR_SYS_TRK,
				     make_vlba_frmtr_sys_trk(
                                     make_dvalue(s1,NULL),s2,
                                     make_dvalue(s3,NULL),
                                     make_dvalue(s4,NULL))));
    }
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_vlba_trnsprt_sys_trk(char *str, char *str2)
{
  char *s1, *s2;

  s1=(char *)strdup(str);
  s2=(char *)strdup(str2);
  q_list = add_list(q_list,make_dvalue(s1,NULL));
  q_list = add_list(q_list,make_dvalue(s2,NULL));
  qref_list = add_list(qref_list,make_lowl(T_VLBA_TRNSPRT_SYS_TRK,q_list));
  q_list=NULL;
  return(NULL);
}  
/*-------------------------------------------------------------------*/
void *
create_s2_recording_mode(char *str)
{
  char *s1;

  s1=(char *)strdup(str);
  qref_list = add_list(qref_list,make_lowl(T_S2_RECORDING_MODE,s1));
  return(NULL);
}
/*-------------------------------------------------------------------*/
void *
create_s2_data_source(char *str, char *str2, char *str3)
{
  char *s1, *s2, *s3;

  if(str2==NULL || strlen(str2)==0 ||
     str3==NULL || strlen(str3)==0)
    {
      s1=(char *)strdup(str);
      qref_list = add_list(qref_list,make_lowl(T_S2_DATA_SOURCE,
			   	     make_s2_data_source(s1,NULL,NULL)));
    }
  else
    {
      s1=(char *)strdup(str);
      s2=(char *)strdup(str2);
      s3=(char *)strdup(str3);
      qref_list = add_list(qref_list,make_lowl(T_S2_DATA_SOURCE,
			   	     make_s2_data_source(s1,s2,s3)));
    }
  return(NULL);
}


