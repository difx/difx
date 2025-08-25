#ifndef __DIFXIO_MACROS_H__
#define __DIFXIO_MACROS_H__

/* The following macro safely replaces "strncpy" with "snprintf" and tests for string truncation, printing a warning if it occurs
 * Parameters:
 *   d = destination string
 *   s = source string
 *   l = full length of destination string (e.g., for "char d[12]", l is 12
 */
#define strncpy_warn(d,s,l) {int vv;vv=snprintf((d),(l),"%s",(s));if(vv >= (l)){fprintf(stderr,"Warning: String truncation (%d >= %d) at %s line %d\n", vv, (l), __FILE__, __LINE__);}}

/* The following macro safely tests "sprintf" for string truncation, printing a warning if it occurs
 * Parameters:
 *   d = destination string
 *   l = full length of destination string (e.g., for "char d[12]", l is 12
 *   f = printf-like format string
 *   ... = source variables for snprintf
 */
#define snprintf_warn(d,l,f,...) {int vv;vv=snprintf((d),(l),(f),__VA_ARGS__);if(vv >= (l)){fprintf(stderr,"Warning: String truncation (%d >= %d) at %s line %d\n", vv, (l), __FILE__, __LINE__);}}

#endif
