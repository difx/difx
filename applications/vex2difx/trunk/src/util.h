#ifndef __UTIL_H__
#define __UTIL_H__

// To capitalize a string
#define Upper(s) transform(s.begin(), s.end(), s.begin(), (int(*)(int))toupper)

extern "C" {
int fvex_double(char **field, char **units, double *d);
int fvex_ra(char **field, double *ra);
int fvex_dec(char **field, double *dec);
}

#endif
