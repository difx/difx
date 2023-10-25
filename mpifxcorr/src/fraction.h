#ifndef _FRACTION_H
#define _FRACTION_H
/********************************************************************************************************
 * @file fraction.h
 * @brief Defines class 'fraction' that holds a fraction, consisting of integer numerator and denominator.
 ********************************************************************************************************/

#include "mathutil.h"

class fraction {

   public:

      fraction() : num(0), denom(1) { }

      fraction(int numerator, int denominator) : num(numerator), denom(denominator) { }
      fraction(long numerator, long denominator) : num(numerator), denom(denominator) { }

      ~fraction() { }

      // Copy constructors

      fraction& operator= (const fraction& o) {
          this->num = o.num;
          this->denom = o.denom;
          return *this;
      }
      fraction& operator= (const int& o) {
          this->num = o;
          this->denom = 1;
          return *this;
      }
      fraction& operator= (const long& o) {
          this->num = o;
          this->denom = 1;
          return *this;
      }

      fraction(const fraction& o) {
          this->num = o.num;
          this->denom = o.denom;
      }
      fraction(const long& o) {
          this->num = o;
          this->denom = 1;
      }
      fraction(const int& o) {
          this->num = o;
          this->denom = 1;
      }

      // Math operators

      fraction& operator+ (const long& m) {
          this->num += m * this->denom;
          return *this;
      }
      fraction operator+ (const long& m) const {
          return fraction(this->num + m * this->denom, this->denom);
      }

      fraction& operator- (const long& m) {
          this->num -= m * this->denom;
          return *this;
      }
      fraction operator- (const long& m) const {
          return fraction(this->num - m * this->denom, this->denom);
      }

      fraction& operator* (const long& m) {
          this->num *= m;
          return *this;
      }
      fraction operator* (const long& m) const {
          return fraction(m * this->num, this->denom);
      }

      fraction& operator/ (const long& d) {
          if ((this->num % d) == 0) {
              this->num /= d;
          } else {
              this->denom *= d;
          }
          return *this;
      }
      fraction operator/ (const long& d) const {
          if ((this->num % d) == 0) {
              return fraction(this->num / d, this->denom);
          } else {
              return fraction(this->num, d * this->denom);
          }
      }

      // Math operators with other fractions

      fraction& operator+ (const fraction& rhs) {
          if (this->denom == rhs.denom) {
              this->num += rhs.num;
          } else {
              this->num = this->num * rhs.denom + rhs.num * this->denom;
              this->denom = this->denom * rhs.denom;
          }
          return *this;
      }
      fraction operator+ (const fraction& rhs) const {
          if (this->denom == rhs.denom) {
              return fraction(this->num + rhs.num, this->denom);
          } else {
              return fraction(this->num * rhs.denom + rhs.num * this->denom, this->denom * rhs.denom);
          }
      }

      fraction& operator- (const fraction& rhs) {
          if (this->denom == rhs.denom) {
              this->num -= rhs.num;
          } else {
              this->num = this->num * rhs.denom - rhs.num * this->denom;
              this->denom = this->denom * rhs.denom;
          }
          return *this;
      }
      fraction operator- (const fraction& rhs) const {
          if (this->denom == rhs.denom) {
              return fraction(this->num - rhs.num, this->denom);
          } else {
              return fraction(this->num * rhs.denom - rhs.num * this->denom, this->denom * rhs.denom);
          }
      }


   public:

      const double approx() const { return double(num)/denom; }

      void simplify() {
          long g;
          while((g = gcd(this->num, this->denom)) > 1) {
              this->num /= g;
              this->denom /= g;
          }
      }

   public:

      long num, denom;
};

inline bool operator==(const fraction& lhs, const fraction& rhs) {
    return (lhs.num == rhs.num && lhs.denom == rhs.denom)
           || ((lhs.num * rhs.denom - rhs.num * lhs.denom) == 0);
}

inline bool operator==(const fraction& lhs, const long& rhs) {
    return ((lhs.num - rhs * lhs.denom) == 0);
}

#endif
