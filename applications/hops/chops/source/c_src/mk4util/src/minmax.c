/************************************************************************/
/*									*/
/* Some trivial comparison and windowing routines ... extensively used	*/
/* in fourfit.								*/
/*									*/
/* Created January 12 1994 by CJL					*/
/*									*/
/************************************************************************/

int
imin(int i, int j)
    {
    if (i <= j) return (i);
    else return (j);
    }

int
imax(int i, int j)
    {
    if (i >= j) return (i);
    else return (j);
    }

int
iwin(int value, int lower, int upper)
    {
    if (value < lower) return (lower);
    else if (value > upper) return (upper);
    else return (value);
    }

double
dmin(double a, double b)
    {
    if (a <= b) return (a);
    else return (b);
    }

double
dmax(double a, double b)
    {
    if (a >= b) return (a);
    else return (b);
    }

double
dwin(double value, double lower, double upper)
    {
    if (value < lower) return (lower);
    else if (value > upper) return (upper);
    else return (value);
    }
