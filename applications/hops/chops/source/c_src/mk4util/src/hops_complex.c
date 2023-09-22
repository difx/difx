#include "hops_complex.h"
#include <math.h>

#ifdef USE_C_COMPLEX
    const hops_complex_impl cmplx_unit_I = I;
#else
    const std::complex<double> cmplx_unit_I = std::complex<double>(0.0,1.0); 
#endif

void zero_complex(hops_complex* val)
{
    #ifdef USE_C_COMPLEX
        *val = 0.0;
    #else
        *val = 0.0;
    #endif
}


void set_complex(hops_complex* val, double real, double imag)
{
    #ifdef USE_C_COMPLEX
        *val = real + I*imag;
    #else
        *val = std::complex<double>(real,imag);
    #endif
}

double abs_complex(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return cabs(val);
    #else
        return std::abs(val);
    #endif
}


double arg_complex(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return carg(val);
    #else
        return std::arg(val);
    #endif
}

hops_complex 
exp_complex(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return cexp(val);
    #else
        return std::exp(val);
    #endif
}


extern double real_comp(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return creal(val);
    #else
        return std::real(val);
    #endif
}

extern double imag_comp(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return cimag(val);
    #else
        return std::imag(val);
    #endif
}


extern hops_complex conjugate(hops_complex val)
{
    #ifdef USE_C_COMPLEX
        return conj(val);
    #else
        return std::conj(val);
    #endif
}