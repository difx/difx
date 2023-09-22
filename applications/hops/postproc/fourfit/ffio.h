#ifndef FFIO_H__
#define FFIO_H__


extern int
fill_200 (
struct scan_struct *root,
struct type_param *param,
struct type_200 *t200);

extern int 
fill_201 (struct scan_struct *root,
struct type_param *param,
struct type_201 *t201);

extern int
fill_202 (
struct vex *root,
struct type_param *param,
struct type_202 *t202);

extern int
fill_203 (
struct scan_struct *root,
struct type_param *param,
struct type_203 *t203);

extern int
fill_204 (
struct type_204 *t204);

extern int
fill_205 (
struct scan_struct *root,
struct type_pass *pass,
struct type_param *param,
struct type_203 *t203,
struct type_205 *t205);

extern int
fill_206 (
struct scan_struct *root,
struct type_pass *pass,
struct type_param *param,
struct type_status *status,
struct type_206 *t206);

extern int
fill_207 (
struct type_pass *pass,
struct type_status *status,
struct type_param *param,
struct type_207 *t207);

extern int
fill_208 (
struct type_pass *pass,
struct type_param *param,
struct type_status *status,
struct type_202 *t202,
struct type_208 *t208);

extern int
fill_210 (
struct type_pass *pass,
struct type_status *status,
struct type_210 *t210);

extern int
fill_212 (
struct type_pass *pass,
struct type_status *status,
struct type_param *param,
int fr,
struct type_212 *t212);

extern int
fill_222(
struct type_param *param,
struct type_222 **t222);

extern int
fill_230 (
struct type_pass *pass,
struct type_param *param,
int fr,
int ap,
struct type_230 *t230);

extern int
fill_fringe_info (
struct vex *root,
struct type_pass *pass,
char *filename);

#endif /* end of include guard: FFIO_H__ */
