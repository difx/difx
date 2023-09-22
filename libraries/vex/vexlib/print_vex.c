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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vex.h"
#include "y.tab.h"
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
  char *ptr;

  while (blocks!=NULL) {
    struct block *this=(struct block *)blocks->ptr;
    switch (this->block) {
    case B_GLOBAL:
      fprintf(fp, "\n$GLOBAL;");
      print_qref_block(this->items);
      break;
    case B_STATION:
      fprintf(fp, "\n$STATION;");
      print_def_block(this->items,print_qref_block);
      break;
    case B_MODE:
      fprintf(fp, "\n$MODE;");
      print_def_block(this->items,print_qref_block);
      break;
    case T_COMMENT:
      print_comment((char *)this->items);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this->items);
      break;
    default:
      fprintf(fp,"\n");
      print_block_name(this->block);
      fprintf(fp,";");
      print_def_block(this->items,print_lowl);
      break;
    }
    blocks=blocks->next;
  }
}
void print_def_block(struct llist *items,void func())
{
  while (items!=NULL) {
    struct lowl *this=(struct lowl *)items->ptr;
    switch(this->statement) {
    case T_DEF:
      {struct def *def=(struct def *)this->item;

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
      {struct def *def=(struct def *)this->item;
      fprintf(fp, "\n  scan ");
      print_svalue(def->name);
      fprintf(fp, ";");

      func(def->refs);

      fprintf(fp, "\n  endscan;");
      }
      break;
    case T_COMMENT:
      print_comment((char *)this->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this->item);
      break;
    default:
      fprintf(stderr,"Unknown def_lowl %d",this->statement);
      exit(1);
    }
    items=items->next;
  }
}
void print_qref_block(struct llist *items)
{
  while (items!=NULL) {
    struct lowl *this=(struct lowl *)items->ptr;
    switch(this->statement) {
    case T_REF:
      { struct qref *qref=(struct qref *)this->item;
      fprintf(fp, "\n    ref ");
      print_block_name(qref->primitive);
      fprintf(fp, " = ");
      print_svalue(qref->name);
      print_qualifiers(qref->qualifiers);
      fprintf(fp, ";");
      }
      break;
    case T_COMMENT:
      print_comment((char *)this->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *)this->item);
      break;
    default:
      fprintf(stderr,"Unknown def_lowl %d",this->statement);
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
    char *this=(char *)items->ptr;
	fprintf(fp, ":");
    print_svalue(this);
    items=items->next;
  }
}
void print_lowl(struct llist *items)
{
  while (items!=NULL) {
    struct lowl *this=(struct lowl *)items->ptr;
    switch (this->statement) {
    case T_LITERAL:
      print_literal_list((struct llist *) this->item);
      break;
    case T_REF:
      print_external((struct external *) this->item);
      break;
    case T_COMMENT:
      print_comment((char *) this->item);
      break;
    case T_COMMENT_TRAILING:
      print_comment_trailing((char *) this->item);
      break;
    default:
      print_lowl_st(this->statement,this->item);
    }
    items=items->next;
  }
}
void print_lowl_st(int statement, void *ptr)
{
  char *value, *units;
  int link, name, i, ierr;

  ierr=0;
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
    else
	fprintf(fp, " :");
    if(value!=NULL && *value!='\0') {
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
    }
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
void print_external(struct external *this)
{
    fprintf(fp, "\n    ref ");
  print_svalue(this->file);

    fprintf(fp, ":");
  print_block_name(this->primitive);

    fprintf(fp, " = ");
  print_svalue(this->name);
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
      outch:
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
