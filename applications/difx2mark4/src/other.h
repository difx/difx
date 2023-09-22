#ifndef __OTHER_H__
#define __OTHER_H__

#ifndef NELEMENTS
#define NELEMENTS(array)    /* number of elements in an array */ \
              (sizeof (array) / sizeof ((array) [0]))
#endif

char *mjd2str(long, char *);
int mjd2dayno(long, int *);
int mjd2date(long, int*, int*, int*);
int rad2hms(double, char *);
int rad2dms(double, char *);
#endif
