#ifndef MK4_AFIO_H
#define MK4_AFIO_H

#include "adata.h"

extern void set_afile_com_char(int star);
extern char get_afile_com_char(void);
extern int  afile_comment(char *line);
extern int  afile_header(int version, int type, FILE *fp);
extern void aline_id(char *line, int *version, int *type);
extern void clear_csumm(corelsum *dsumm);
extern void clear_fsumm(fringesum *dsumm);
extern void clear_rsumm(rootsum *dsumm);
extern void clear_tsumm(trianglesum *dsumm);
extern char *corelname(corelsum *csumm);
extern char *fringename(fringesum *fsumm);   
extern int  get_unique_name(char *input, char *output);
extern int  parse_csumm(char *line, corelsum *file);
extern int  parse_fsumm(char *line, fringesum *file);
extern int  parse_rsumm(char *line, rootsum *file);
extern int  parse_tsumm(char *line, trianglesum *file);
extern char *rootname(rootsum *rsumm);
extern int  write_csumm(corelsum *data, FILE *fp);
extern int  write_fsumm(fringesum *data, FILE *fp);
extern int  write_rsumm(rootsum *data, FILE *fp);
extern int  write_tsumm(trianglesum *data, FILE *fp);   

#endif
