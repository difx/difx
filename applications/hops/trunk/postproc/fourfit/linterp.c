/************************************************************************/
/*                                                                      */
/* Trivial routine to do a linear interpolation                         */
/*                                                                      */
/*      Inputs:     coord1          location of first point             */
/*                  value1          value of first point                */
/*                  coord2          location of second point            */
/*                  value2          value of second point               */
/*                  coord           target location                     */
/*                                                                      */
/*      Output:     value           interpolated value                  */
/*                  return value    0=OK, else bad                      */
/*                                                                      */
/* Created  March 14 2000 by CJL                                        */
/*                                                                      */
/************************************************************************/

int
linterp (/* coord1, value1, coord2, value2, coord, value) */
double coord1,
double value1,
double coord2,
double value2,
double coord,
double *value)
    {
    double cdiff, lweight, uweight, lower, upper, lval, uval;
                                        /* Condition inputs */
    if (coord1 < coord2)
        {
        lower = coord1;
        lval = value1;
        upper = coord2;
        uval = value2;
        }
    else if (coord2 < coord1)
        {
        lower = coord2;
        lval = value2;
        upper = coord1;
        uval = value1;
        }
                                        /* Trap case of upper=lower */
    else if (coord != coord1)
        {
        msg ("Degenerate inputs to linterp()", 2);
        msg ("coord1, coord2, coord = %.10g %.10g %.10g", 2, coord1, coord2, coord);
        return (-1);
        }
    else
        {
        *value = value1;
        return (0);
        }
                                        /* Range check */
    if ((coord < lower) || (coord > upper))
        {
        msg ("Out of range inputs to linterp()", 2);
        msg ("coord, lower, upper = %.10g %.10g %.10g", 2, coord, lower, upper);
        return (-1);
        }
                                        /* Simple linear interpolation */
    cdiff = upper - lower;
    lweight = (upper - coord) / cdiff;
    uweight = (coord - lower) / cdiff;
    *value = lweight * lval + uweight * uval;
    return (0);
    }
