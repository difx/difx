#ifndef HOPS_COMPLEX_WRAPPER__
#define HOPS_COMPLEX_WRAPPER__

#ifndef __cplusplus
#include <complex.h>
#endif

////////////////////////////////////////////////////////////////////////////////
#ifdef __cplusplus
    extern "C"
    {
#endif /* __cplusplus */
////////////////////////////////////////////////////////////////////////////////

        //if compiling against c++ we dont'want the complex types polluting
        //the namespace, so we have to alias them and then undef them
        #if defined(_Complex_I) && defined(complex) && defined(I)
            typedef double _Complex hops_complex_impl;
        #else
            typedef double hops_complex_impl[2];
        #endif

////////////////////////////////////////////////////////////////////////////////
#ifdef __cplusplus
    } //end of extern C
#endif /* __cplusplus */
////////////////////////////////////////////////////////////////////////////////

//alias to the implementation
#define hops_complex hops_complex_impl

typedef struct hops_complex_tag	/* needed in type_230 */
{
   double real;
   double imag;
}
hops_scomplex;

#endif /* end of include guard: HOPS_COMPLEX_WRAPPER */
