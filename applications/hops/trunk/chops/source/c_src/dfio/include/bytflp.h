#include "hops_config.h"
#ifndef BYTFLP_H
#define BYTFLP_H

#ifdef BYTFLP

#define cp_short(a,b)  {a = b; short_reverse (&(a));}
#define cp_unsig(a,b)  {a = b; unsig_reverse (&(a));}
#define cp_int(a,b)    {a = b; int_reverse (&(a));}
#define cp_long(a,b)   {a = b; long_reverse (&(a));}
#define cp_float(a,b)  {a = b; float_reverse (&(a));}
#define cp_double(a,b) {a = b; double_reverse (&(a));}

#else

#define cp_short(a,b)  a = b
#define cp_unsig(a,b)  a = b
#define cp_int(a,b)    a = b
#define cp_long(a,b)   a = b
#define cp_float(a,b)  a = b
#define cp_double(a,b) a = b

#endif

#endif
