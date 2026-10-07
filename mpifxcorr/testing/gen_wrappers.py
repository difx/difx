#!/usr/bin/env python3
"""Generate arch_wrappers.inc: one wrapper per architecture.h vector routine.

Each wrapper has a neutral C signature (arch_api.h types) and calls the
vector* macro of whichever architecture.h it is compiled with.  Each entry
is guarded by ARCH_SKIP_<name> so an implementation can leave out routines
that do not compile there (the skip is then recorded for the report).

Run: ./gen_wrappers.py > arch_wrappers.inc
"""

# (name, parameters, body).  A body that is a plain expression is returned
# as the status; a body starting with '{' is used as the function body.
F, D, C, Z, S, I, B, CS = 'float', 'double', 'acf32', 'acf64', 'int16_t', 'int32_t', 'uint8_t', 'acs16'
f32, f64, cf32, cf64, s16, s32, u8, cs16 = 'f32', 'f64', 'cf32', 'cf64', 's16', 's32', 'u8', 'cs16'

def unary(name, mac, ti, to, tvi, tvo, const=True):
    c = 'const ' if const else ''
    return (name, f'({c}{ti} *s, {to} *d, int n)', f'{mac}(({c}{tvi} *)s, ({tvo} *)d, n)')

def inplace(name, mac, t, tv):
    return (name, f'({t} *sd, int n)', f'{mac}(({tv} *)sd, n)')

def binary_i(name, mac, t, tv, const=True):
    c = 'const ' if const else ''
    return (name, f'(const {t} *s, {t} *sd, int n)', f'{mac}(({c}{tv} *)s, ({tv} *)sd, n)')

def binary(name, mac, t1, t2, to, tv1, tv2, tvo):
    return (name, f'(const {t1} *s1, const {t2} *s2, {to} *d, int n)',
            f'{mac}((const {tv1} *)s1, (const {tv2} *)s2, ({tvo} *)d, n)')

def scal(name, mac, t, tv, tvval=None, valconv=None):
    v = valconv or 'v'
    return (name, f'(const {t} *s, {t} v, {t} *d, int n)', f'{mac}((const {tv} *)s, {v}, ({tv} *)d, n)')

def scal_i(name, mac, t, tv, valconv=None):
    v = valconv or 'v'
    return (name, f'({t} v, {t} *sd, int n)', f'{mac}({v}, ({tv} *)sd, n)')

E = []
# Add
E += [binary_i('Add_f32_I', 'vectorAdd_f32_I', F, f32), binary_i('Add_f64_I', 'vectorAdd_f64_I', D, f64),
      binary_i('Add_s16_I', 'vectorAdd_s16_I', S, s16), binary_i('Add_s32_I', 'vectorAdd_s32_I', I, s32),
      binary_i('Add_cf32_I', 'vectorAdd_cf32_I', C, cf32), binary_i('Add_cf64_I', 'vectorAdd_cf64_I', Z, cf64, const=False)]
E += [binary('Add_f32', 'vectorAdd_f32', F, F, F, f32, f32, f32), binary('Add_f64', 'vectorAdd_f64', D, D, D, f64, f64, f64),
      binary('Add_s16', 'vectorAdd_s16', S, S, S, s16, s16, s16), binary('Add_s32', 'vectorAdd_s32', I, I, I, s32, s32, s32),
      binary('Add_cf32', 'vectorAdd_cf32', C, C, C, cf32, cf32, cf32), binary('Add_cf64', 'vectorAdd_cf64', Z, Z, Z, cf64, cf64, cf64)]
E += [scal('AddC_f64', 'vectorAddC_f64', D, f64), scal('AddC_f32', 'vectorAddC_f32', F, f32),
      scal_i('AddC_f32_I', 'vectorAddC_f32_I', F, f32), scal_i('AddC_s16_I', 'vectorAddC_s16_I', S, s16),
      scal_i('AddC_f64_I', 'vectorAddC_f64_I', D, f64)]
E += [('AddProduct_cf32', '(const acf32 *s1, const acf32 *s2, acf32 *acc, int n)',
       'vectorAddProduct_cf32((const cf32 *)s1, (const cf32 *)s2, (cf32 *)acc, n)')]
# Conj, Copy, Cos
E += [unary('Conj_cf32', 'vectorConj_cf32', C, C, cf32, cf32), inplace('Conj_cf32_I', 'vectorConj_cf32_I', C, cf32),
      unary('ConjFlip_cf32', 'vectorConjFlip_cf32', C, C, cf32, cf32)]
E += [unary('Copy_u8', 'vectorCopy_u8', B, B, u8, u8), unary('Copy_s16', 'vectorCopy_s16', S, S, s16, s16),
      unary('Copy_s32', 'vectorCopy_s32', I, I, s32, s32), unary('Copy_f32', 'vectorCopy_f32', F, F, f32, f32),
      unary('Copy_cf32', 'vectorCopy_cf32', C, C, cf32, cf32), unary('Copy_f64', 'vectorCopy_f64', D, D, f64, f64)]
E += [unary('Cos_f32', 'vectorCos_f32', F, F, f32, f32), unary('Sin_f32', 'vectorSin_f32', F, F, f32, f32),
      ('SinCos_f32', '(const float *s, float *sn, float *cs, int n)', 'vectorSinCos_f32((const f32 *)s, (f32 *)sn, (f32 *)cs, n)')]
# Conversions
E += [('ConvertScaled_s16f32', '(const int16_t *s, float *d, int n, int sf)', 'vectorConvertScaled_s16f32((const s16 *)s, (f32 *)d, n, sf)'),
      ('ConvertScaled_f32s16', '(const float *s, int16_t *d, int n, int rnd, int sf)', 'vectorConvertScaled_f32s16((const f32 *)s, (s16 *)d, n, ARCH_RNDMODE(rnd), sf)'),
      ('ConvertScaled_f32u8', '(const float *s, uint8_t *d, int n, int rnd, int sf)', 'vectorConvertScaled_f32u8((const f32 *)s, (u8 *)d, n, ARCH_RNDMODE(rnd), sf)'),
      ('Convert_f32s32', '(const float *s, int32_t *d, int n, int rnd)', 'vectorConvert_f32s32((const f32 *)s, (s32 *)d, n, ARCH_RNDMODE(rnd))'),
      unary('Convert_s16f32', 'vectorConvert_s16f32', S, F, s16, f32), unary('Convert_s32f32', 'vectorConvert_s32f32', I, F, s32, f32),
      unary('Convert_f64f32', 'vectorConvert_f64f32', D, F, f64, f32)]
# Divide
E += [binary('Divide_f32', 'vectorDivide_f32', F, F, F, f32, f32, f32),
      ('DivC_cf32_I', '(acf32 v, acf32 *sd, int n)', '{ cf32 val; memcpy(&val, &v, sizeof(val)); return (int)vectorDivC_cf32_I(val, (cf32 *)sd, n); }'),
      scal_i('DivC_f32_I', 'vectorDivC_f32_I', F, f32)]
E += [('DotProduct_f64', '(const double *s1, const double *s2, int n, double *out)', 'vectorDotProduct_f64((const f64 *)s1, (const f64 *)s2, n, (f64 *)out)')]
E += [inplace('Flip_f64_I', 'vectorFlip_f64_I', D, f64), unary('Flip_cf32', 'vectorFlip_cf32', C, C, cf32, cf32)]
E += [unary('Magnitude_cf32', 'vectorMagnitude_cf32', C, F, cf32, f32), unary('Phase_cf32', 'vectorPhase_cf32', C, F, cf32, f32),
      unary('Real_cf32', 'vectorReal_cf32', C, F, cf32, f32)]
E += [('Mean_cf32', '(const acf32 *s, int n, acf32 *m)', 'vectorMean_cf32((cf32 *)s, n, (cf32 *)m, vecAlgHintFast)'),
      ('Sum_cf32', '(const acf32 *s, int n, acf32 *m)', 'vectorSum_cf32((const cf32 *)s, n, (cf32 *)m, vecAlgHintFast)'),
      ('StdDev_f32', '(const float *s, int n, float *sd)', 'vectorStdDev_f32((f32 *)s, n, (f32 *)sd, vecAlgHintFast)'),
      ('Max_f32', '(const float *s, int n, float *m)', 'vectorMax_f32((const f32 *)s, n, (f32 *)m)'),
      ('Min_f32', '(const float *s, int n, float *m)', 'vectorMin_f32((const f32 *)s, n, (f32 *)m)'),
      ('MaxIndx_f32', '(const float *s, int n, float *m, int *idx)', 'vectorMaxIndx_f32((f32 *)s, n, (f32 *)m, idx)')]
# Mul
E += [binary('Mul_f32', 'vectorMul_f32', F, F, F, f32, f32, f32), binary_i('Mul_f32_I', 'vectorMul_f32_I', F, f32),
      binary_i('Mul_cf32_I', 'vectorMul_cf32_I', C, cf32), binary('Mul_cf32', 'vectorMul_cf32', C, C, C, cf32, cf32, cf32),
      binary('Mul_f32cf32', 'vectorMul_f32cf32', F, C, C, f32, cf32, cf32),
      scal('MulC_f32', 'vectorMulC_f32', F, f32),
      ('MulC_cs16_I', '(acs16 v, acs16 *sd, int n)', '{ cs16 val; memcpy(&val, &v, sizeof(val)); return (int)vectorMulC_cs16_I(val, (cs16 *)sd, n); }'),
      scal_i('MulC_f32_I', 'vectorMulC_f32_I', F, f32),
      ('MulC_cf32_I', '(acf32 v, acf32 *sd, int n)', '{ cf32 val; memcpy(&val, &v, sizeof(val)); return (int)vectorMulC_cf32_I(val, (cf32 *)sd, n); }'),
      ('MulC_cf32', '(const acf32 *s, acf32 v, acf32 *d, int n)', '{ cf32 val; memcpy(&val, &v, sizeof(val)); return (int)vectorMulC_cf32((const cf32 *)s, val, (cf32 *)d, n); }'),
      scal_i('MulC_f64_I', 'vectorMulC_f64_I', D, f64), scal('MulC_f64', 'vectorMulC_f64', D, f64)]
E += [('RealToComplex_f32', '(const float *re, const float *im, acf32 *c, int n)', 'vectorRealToComplex_f32((const f32 *)re, (const f32 *)im, (cf32 *)c, n)'),
      ('Set_f32', '(float v, float *d, int n)', 'vectorSet_f32(v, (f32 *)d, n)'),
      ('SplitScaled_s16f32', '(const int16_t *s, float **d, int nchan, int chanlen)', 'vectorSplitScaled_s16f32((const s16 *)s, (f32 **)d, nchan, chanlen)')]
E += [inplace('Square_f32_I', 'vectorSquare_f32_I', F, f32), inplace('Square_f64_I', 'vectorSquare_f64_I', D, f64),
      inplace('Sqrt_f32_I', 'vectorSqrt_f32_I', F, f32), inplace('Abs_f32_I', 'vectorAbs_f32_I', F, f32)]
E += [binary_i('Sub_f32_I', 'vectorSub_f32_I', F, f32), binary('Sub_s32', 'vectorSub_s32', I, I, I, s32, s32, s32),
      binary_i('Sub_cf32_I', 'vectorSub_cf32_I', C, cf32), binary('Sub_cf32', 'vectorSub_cf32', C, C, C, cf32, cf32, cf32)]
E += [('Zero_u8', '(uint8_t *d, int n)', 'vectorZero_u8((u8 *)d, n)'), ('Zero_s16', '(int16_t *d, int n)', 'vectorZero_s16((s16 *)d, n)'),
      ('Zero_s32', '(int32_t *d, int n)', 'vectorZero_s32((s32 *)d, n)'), ('Zero_f32', '(float *d, int n)', 'vectorZero_f32((f32 *)d, n)'),
      ('Zero_cf32', '(acf32 *d, int n)', 'vectorZero_cf32((cf32 *)d, n)'), ('Zero_cf64', '(acf64 *d, int n)', 'vectorZero_cf64((cf64 *)d, n)')]
E += [unary('Move_f32', 'vectorMove_f32', F, F, f32, f32, const=False), unary('Move_cf32', 'vectorMove_cf32', C, C, cf32, cf32, const=False)]

# Allocation: allocate, then free with vectorFree
for t, tv in (('u8', u8), ('s16', s16), ('cs16', cs16), ('s32', s32), ('f32', f32), ('cf32', cf32), ('f64', f64), ('cf64', cf64)):
    E += [(f'Alloc_{t}', '(int n, void **p)', f'{{ *p = (void *)vectorAlloc_{t}(n); return *p ? 0 : -1; }}'),
          (f'Free_{t}', '(void *p)', f'{{ {tv} *q = ({tv} *)p; vectorFree(q); return 0; }}')]

# FFTs.  Init stores the spec and work buffer; buf/bufsize start as sentinels
# so an init that does not set them can be detected.
def fft(name, specty, initmac, sizearg, runmac, intype, outtype, invmac=None, freemac=None):
    out = [(f'{name}_init', f'(int {sizearg}, int flag, void **spec, unsigned char **buf, int *bufsize)',
            f'{{ {specty} *s = 0; u8 *b = (u8 *)ARCH_UNSET_PTR; int wb = ARCH_UNSET_INT; '
            f'int st = (int){initmac}(&s, {sizearg}, ARCH_FFTFLAG(flag), vecAlgHintFast, &wb, &b); '
            f'*spec = (void *)s; *buf = (unsigned char *)b; *bufsize = wb; return st; }}'),
           (f'{name}_fwd', f'(void *spec, unsigned char *buf, const {intype} *src, {outtype} *dst)',
            f'{runmac}((const {"f32" if intype == "float" else "cf32"} *)src, ({"f32" if outtype == "float" else "cf32"} *)dst, ({specty} *)spec, (u8 *)buf)')]
    if invmac:
        out += [(f'{name}_inv', '(void *spec, unsigned char *buf, const float *src, float *dst)',
                 f'{invmac}((const f32 *)src, (f32 *)dst, ({specty} *)spec, (u8 *)buf)')]
    out += [(f'{name}_free', '(void *spec, unsigned char *buf)',
             f'{{ {freemac}(({specty} *)spec); if(buf && buf != ARCH_UNSET_PTR) {{ u8 *q = (u8 *)buf; vectorFree(q); }} return 0; }}')]
    return out

E += fft('FFTR_f32', 'vecFFTSpecR_f32', 'vectorInitFFTR_f32', 'order', 'vectorFFT_RtoC_f32', 'float', 'float',
         invmac='vectorFFTInv_CCSToR_32f', freemac='vectorFreeFFTR_f32')
E += fft('FFTC_cf32', 'vecFFTSpecC_cf32', 'vectorInitFFTC_cf32', 'order', 'vectorFFT_CtoC_cf32', 'acf32', 'acf32', freemac='vectorFreeFFTC_cf32')
E += fft('DFTC_cf32', 'vecDFTSpecC_cf32', 'vectorInitDFTC_cf32', 'length', 'vectorDFT_CtoC_cf32', 'acf32', 'acf32', freemac='vectorFreeDFTC_cf32')
E += fft('DFTR_f32', 'vecDFTSpecR_f32', 'vectorInitDFTR_f32', 'length', 'vectorDFT_RtoC_f32', 'float', 'float', freemac='vectorFreeDFTR_f32')
# Real inverse transforms with their own spec (generic section only)
E += [('DFTCR_f32_init', '(int length, int flag, void **spec, unsigned char **buf, int *bufsize)',
       '{ GenFFTPtrCRf32 *s = 0; u8 *b = (u8 *)ARCH_UNSET_PTR; int wb = ARCH_UNSET_INT; '
       'int st = (int)vectorInitDFTCR_f32(&s, length, ARCH_FFTFLAG(flag), vecAlgHintFast, &wb, &b); '
       '*spec = (void *)s; *buf = (unsigned char *)b; *bufsize = wb; return st; }'),
      ('DFTCR_f32_inv', '(void *spec, unsigned char *buf, const float *src, float *dst)',
       'vectorDFT_CtoR_f32((const f32 *)src, (f32 *)dst, (GenFFTPtrCRf32 *)spec, (u8 *)buf)'),
      ('DFTCR_f32_free', '(void *spec, unsigned char *buf)', '{ vectorFreeDFTCR_f32((GenFFTPtrCRf32 *)spec); (void)buf; return 0; }'),
      ('FFTCR_f32_init', '(int order, int flag, void **spec, unsigned char **buf, int *bufsize)',
       '{ GenFFTPtrCRf32 *s = 0; u8 *b = (u8 *)ARCH_UNSET_PTR; int wb = ARCH_UNSET_INT; '
       'int st = (int)vectorInitFFTCR_f32(&s, order, ARCH_FFTFLAG(flag), vecAlgHintFast, &wb, &b); '
       '*spec = (void *)s; *buf = (unsigned char *)b; *bufsize = wb; return st; }')]
# 2D FFT (difx_monitor)
E += [('FFT2D_cf32_init', '(int orderx, int ordery, int flag, void **spec)',
       '{ vec2DFFTSpecC_cf32 *s = 0; int st = (int)vector2DInitFFTC_cf32(&s, orderx, ordery, ARCH_FFTFLAG(flag), vecAlgHintFast); *spec = (void *)s; return st; }'),
      ('FFT2D_cf32_fwd', '(void *spec, unsigned char *buf, const acf32 *src, int sstep, acf32 *dst, int dstep)',
       'vector2DFFT_CtoC_cf32((const cf32 *)src, sstep, (cf32 *)dst, dstep, (vec2DFFTSpecC_cf32 *)spec, (u8 *)buf)'),
      ('FFT2D_cf32_free', '(void *spec)', '{ vector2DFreeFFTC_cf32((vec2DFFTSpecC_cf32 *)spec); return 0; }')]
# IPP-only routines
E += [('GenerateFIRLowpass_f64', '(double freq, double *taps, int n)',
       'vectorGenerateFIRLowpass_f64(freq, (f64 *)taps, n, vecHamming, vecTrue)')]

print('/* Generated by gen_wrappers.py; do not edit.  Wrappers for every vector* routine */')
print('/* in architecture.h, with neutral types (arch_api.h).  Included by arch_generic.cpp */')
print('/* and arch_ipp.cpp, which define WRAP/SKIP and any ARCH_SKIP_<name> */\n')
for name, params, body in E:
    print(f'#ifndef ARCH_SKIP_{name}')
    if body.startswith('{'):
        print(f'WRAP_BODY({name}, {params}, {body})')
    else:
        print(f'WRAP({name}, {params}, {body})')
    print('#else')
    print(f'SKIP({name})')
    print('#endif')
