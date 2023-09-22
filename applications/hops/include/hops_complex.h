#ifndef HOPS_COMPLEX_WRAPPER__
#define HOPS_COMPLEX_WRAPPER__

#ifndef HOPS3_USE_CXX
    #define USE_C_COMPLEX
    #ifdef __cplusplus
        #define USE_CXX_DUMMY
    #endif
#endif

#ifdef USE_CXX_DUMMY
extern "C"
{
#endif

typedef struct hops_complex_tag	/* needed in type_230 */
{
   double real;
   double imag;
}
hops_scomplex;

#ifdef USE_C_COMPLEX
    //using c definition of complex 
    #ifndef USE_CXX_DUMMY //pure c case
        #include <complex.h>
        #if defined(_Complex_I) && defined(complex) && defined(I)
            typedef double _Complex hops_complex_impl;
            extern const hops_complex_impl cmplx_unit_I;
        #endif
    #else //c++ lib is linking against this c-library 
        typedef union hops_complex_impl
        {
            struct hops_complex_tag named; 
            double array[2];
        }
        hops_complex_impl;
    extern const hops_complex_impl cmplx_unit_I; 
    #endif
#else 
    //using c++ definition of complex
    #include <complex>
    typedef std::complex<double> hops_complex_impl;
    extern const std::complex<double> cmplx_unit_I; 
#endif

//alias to the implementation
#define hops_complex hops_complex_impl

extern void zero_complex(hops_complex* val);
extern void set_complex(hops_complex* val, double real, double imag);
extern double abs_complex(hops_complex val);
extern double arg_complex(hops_complex val);
extern double real_comp(hops_complex val);
extern double imag_comp(hops_complex val);
extern hops_complex exp_complex(hops_complex val);
extern hops_complex conjugate(hops_complex val);

#ifdef USE_CXX_DUMMY
}
#endif


#endif /* end of include guard: HOPS_COMPLEX_WRAPPER */
