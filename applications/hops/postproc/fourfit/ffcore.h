#ifndef FFCORE_H__
#define FFCORE_H__

extern int set_defaults();
extern int read_sdata (struct fileset *fset, struct mk4_sdata *sdata);
extern int make_passes (struct scan_struct *ovex, struct freq_corel *corel, struct type_param *param, struct type_pass **pass, int *npass);
extern int get_corel_data (fstruct* fs, struct scan_struct* ovex, char* filename, struct mk4_corel* cdata);

#endif