#ifndef _FRACTION_H
#define _FRACTION_H
/********************************************************************************************************
 * @file fraction.h
 * @brief Defines class 'fraction' that holds a fraction, consisting of integer numerator and denominator.
 ********************************************************************************************************/

class fraction {

   public:

      fraction() : num(0), denom(1) { }

      fraction(int numerator, int denominator) : num(numerator), denom(denominator) { }
      fraction(long numerator, long denominator) : num(numerator), denom(denominator) { }

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

      ~fraction() { }

   public:

      const double approx() const { return double(num)/denom; }

   public:

      long num, denom;
};

#endif
