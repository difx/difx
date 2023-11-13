#include <cmath>
#include <algorithm>
#include "mathutil.h"

/**
 * Greatest common divisor.
 */
long long gcd(long a, long b)
{
    if (a == 0 || b == 0)
        return std::max(a,b);
    while (true) {
        a = a%b;
        if (a == 0) {
           return b;
        }
        b = b%a;
        if (b == 0) {
           return a;
        }
    }
}

/**
 * Greatest common divisor, with rounding of floating point input args.
 */
long long gcd(double a, double b)
{
    return gcd((long)floor(a + 0.5), (long)floor(b + 0.5));
}
