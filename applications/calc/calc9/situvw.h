/*
 * situvw.h: A c++ header file correspong to situvw.i, for use in c/c++ programs
 *           Defines a global struct equivalent to the Fortran common block in situvw.i
 */
 #ifdef _cplusplus
extern"C" {
#endif

struct {
    double uvwp[3], uvwv[3];
} situvw_;

#ifdef _cplusplus
}
#endif  
