/*
 * Copyright (c) 2020 NVI, Inc.
 *
 * This file is part of VLBI Field System
 * (see http://github.com/nvi-inc/fs).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vex.h"
#include "vex_parse.tab.h"
extern struct vex *vex_ptr;

void print_vex(struct vex *vex)
{
  if(filename) /* Check to see if create_vex() set this to 1 */
    {               /* Found in vex_put.c source                  */
      fp=fopen(filename,"w");
    }
  else
    fp=stdout;
  print_lowl(vex->version);

  print_vex_blocks(vex->blocks);
  fprintf(fp,"\n");
}

void print_vex_blocks(struct llist *blocks)
{
  while (blocks!=NULL) {
    struct block *this_=(struct block *)blocks->ptr;
    switch (this_->block) {
    case B_GLOBAL:
      fprintf(fp, "\n$GLOBAL;");
      print_qref_block(this_->items);
      break;
    case B_STATION:
      fprintf(fp, "\n$STATION;");
      print_def_block(this_->items,print_qref_block);
      break;
    case B_MODE:
      fprintf(fp, "\n$MODE;");
      print_def_block(this_->items,print_qref_block);
      break;
    case T_COMMENT:
      print_comment((char *)this_->items);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this_->items);
      break;
    default:
      fprintf(fp,"\n");
      print_block_name(this_->block);
      fprintf(fp,";");
      print_def_block(this_->items,print_lowl);
      break;
    }
    blocks=blocks->next;
  }
}
void print_def_block(struct llist *items,void func())
{
  while (items!=NULL) {
    struct lowl *this_=(struct lowl *)items->ptr;
    switch(this_->statement) {
    case T_DEF:
      {struct def *def=(struct def *)this_->item;

      /* new */
      if(!strstr(def->name,"comment")) {
	fprintf(fp, "\n  def ");
	print_svalue(def->name);
	fprintf(fp, ";");
	
	func(def->refs);
	
	fprintf(fp, "\n  enddef;");
      } else {
	func(def->refs);
      }
      }
      break;
      /*end new */

      /* old */
      /*fprintf(fp, "\n  def ");
      print_svalue(def->name);
      fprintf(fp, ";");
      func(def->refs);
      fprintf(fp, "\n  enddef;");
      */
      /*end old*/
    case T_SCAN:
      {struct def *def=(struct def *)this_->item;
      fprintf(fp, "\n  scan ");
      print_svalue(def->name);
      fprintf(fp, ";");

      func(def->refs);

      fprintf(fp, "\n  endscan;");
      }
      break;
    case T_COMMENT:
      print_comment((char *)this_->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this_->item);
      break;
    default:
      fprintf(stderr,"Unknown def_lowl %d\n",this_->statement);
      exit(1);
    }
    items=items->next;
  }
}
void print_qref_block(struct llist *items)
{
  while (items!=NULL) {
    struct lowl *this_=(struct lowl *)items->ptr;
    switch(this_->statement) {
    case T_REF:
      { struct qref *qref=(struct qref *)this_->item;
      fprintf(fp, "\n    ref ");
      print_block_name(qref->primitive);
      fprintf(fp, " = ");
      print_svalue(qref->name);
      print_qualifiers(qref->qualifiers);
      fprintf(fp, ";");
      }
      break;
    case T_COMMENT:
      print_comment((char *)this_->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this_->item);
      break;
    default:
      fprintf(stderr,"Unknown def_lowl %d\n",this_->statement);
      exit(1);
    }
    items=items->next;
  }
}
void print_block_name(int block)
{
  char *ptr;

  ptr=int2block(block);
  if(ptr==NULL) {
    fprintf(stderr,"unknown block in print_block_name %d\n",block);
    exit(1);
  }
  fprintf(fp, "$%s",ptr);
}
void print_qualifiers(struct llist *items)
{
  while (items!=NULL) {
    char *this_=(char *)items->ptr;
	fprintf(fp, ":");
    print_svalue(this_);
    items=items->next;
  }
}
void print_lowl(struct llist *items)
{
  while (items!=NULL) {
    struct lowl *this_=(struct lowl *)items->ptr;
    switch (this_->statement) {
    case T_LITERAL:
      print_literal_list((struct llist *) this_->item);
      break;
    case T_REF:
      print_external((struct external *) this_->item);
      break;
    case T_COMMENT:
      print_comment((char *) this_->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *) this_->item);
      break;
    default:
      print_lowl_st(this_->statement,this_->item);
    }
    items=items->next;
  }
}
void print_lowl_st(int statement, void *ptr)
{
  char *value, *units;
  int link, name, i, ierr, j, jcount;

  ierr=0;
  jcount=1;
  for (i=0;ierr==0;i++) {
    ierr=vex_field(statement,ptr,i,&link,&name,&value,&units);
    if(ierr!=0)
      continue;
    if(i==0) {
      if(statement!=T_VEX_REV)
/*	fprintf(fp, "   ");*/
	fprintf(fp, "\n   "); 
    } else if(i==1)
	  fprintf(fp, " =");
    if(value!=NULL && *value!='\0') {
      if (i >=2) {
        for (j=0;j<jcount;j++)
	      fprintf(fp, " :");
        jcount=1;
      }
      if(statement!=T_VEX_REV || i !=0)
	    fprintf(fp, " ");
       if(link)
         fprintf(fp, "&");
       if(name)
	     print_svalue(value);
       else
	     fprintf(fp, "%s",value);
       if(units!=NULL && *units!='\0') {
	     fprintf(fp, " ");
	     fprintf(fp, "%s",units);
       }
    } else if(i>1) /* make up for null fields if there is one not null */
      jcount++;
  }
  if(ierr==-1) {
    fprintf(stderr,"Unknown lowl %d",statement);
    exit(1);
  } else if(ierr!=0 && ierr != -2) {
      fprintf(stderr,"Unknown error in print_lowl_st %d\n",ierr);
      exit(1);
  }
    fprintf(fp, ";");
}
void print_external(struct external *this_)
{
    fprintf(fp, "\n    ref ");
  print_svalue(this_->file);

    fprintf(fp, ":");
  print_block_name(this_->primitive);

    fprintf(fp, " = ");
  print_svalue(this_->name);
    fprintf(fp, ";");

}
void print_svalue(char *svalue)
{
  char *ptr;
  static char quotec[]={" \t\n;:=&*$\""};
  int quote=0;
  int outch;

  if(svalue==NULL || *svalue == '\0')
    return;

  for(ptr=svalue;*ptr!=0;ptr++) {
    if((!isgraph(*ptr)) || NULL!=strchr(quotec,*ptr)) {
      quote=1;
      break;
    }
  }
  
  if(!quote) {       
    fprintf(fp, "%s",svalue);
  return;
  }

    fprintf(fp, "\"");
  for(ptr=svalue;*ptr!=0;ptr++) {
    if(isprint(*ptr) && '"'!=*ptr) {
	fprintf(fp, "%c",*ptr);
    } else {
	fprintf(fp, "\\");
      switch (*ptr) {
      case '\b':
	outch='b';
	break;
      case '\t':
	outch='t';
	break;
      case '\n':
	outch='n';
	break;
      case '\v':
	outch='v';
	break;
      case '\f':
	outch='f';
	break;
      case '\r':
	outch='r';
	break;
      case '"':
	outch='"';
	break;
      default:
	fprintf(fp, "x%02x",*ptr);
      outch='\0';
      }
      if(outch!='\0') {
	fprintf(fp, "%c",outch);
      }
    }
  }
	fprintf(fp, "\"");
}

void print_literal_list(struct llist *literals)
{
  char *text=(char *) literals->ptr;

	fprintf(fp, "\nstart_literal(");
	fprintf(fp, "%s",text);
	fprintf(fp, ");");

  literals=literals->next;
  while (literals!=NULL) {
      fprintf(fp, "\n%s",(char *) literals->ptr);
    literals=literals->next;
  }
      fprintf(fp, "\nend_literal(");
      fprintf(fp, "%s",text);
      fprintf(fp, ");");
}
void print_comment(char *comment)
{
    fprintf(fp, "\n%s",comment);
}
void print_comment_trailing(char *comment_trailing)
{
    fprintf(fp, " %s",comment_trailing);
}
