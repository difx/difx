/*
 * cphys.h: A c++ header file correspong to cphys.i, for use in c/c++ programs
 *          Defines a global struct equivalent to the Fortran common block in cphys.i
 */
 #ifdef _cplusplus
extern"C" {
#endif

 struct {
    double VLIGHT, GRAVCT, GAUSS, GMSUN, GMMOON, SECPAU, REARTH, EFLAT, EMRATO,
           GAMMA, GMEARTH, VLIGHT2, VLIGHT3, GMPLANET[7], AU_meters;
} cphys_;

#ifdef _cplusplus
}
#endif  
