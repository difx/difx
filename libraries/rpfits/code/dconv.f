*=======================================================================
*   These are a set of byte-oriented routines to facilitate conversion
*   of data between different architectures.  They are designed to work
*   on any architecture which uses two's complement byte arithmetic.
*
*     IB2B: Big-endian integer -> big-endian integer (copy).
*     IB2L: big-endian integer -> little-endian integer (byte swap).
*     IB2V: big-endian integer -> VAX integer (byte swap).
*
*     IL2B: little-endian integer -> big-endian integer (byte swap).
*     IL2L: little-endian integer -> little-endian integer (copy).
*     IL2V: little-endian integer -> VAX integer (copy).
*
*     IV2B: VAX integer -> big-endian integer (byte swap).
*     IV2L: VAX integer -> little-endian integer (copy).
*     IV2V: VAX integer -> VAX integer (copy).
*
*     RB2B: IEEE big-endian real -> IEEE big-endian real (copy).
*     RB2L: IEEE big-endian real -> IEEE little-endian real (byte swap).
*     RB2V: IEEE big-endian real -> VAX real (format translation).
*
*     RL2B: IEEE little-endian real -> IEEE big-endian real (byte swap).
*     RL2L: IEEE little-endian real -> IEEE little-endian real (copy).
*     RL2V: IEEE little-endian real -> VAX real (format translation).
*
*     RV2B: VAX real -> IEEE big-endian real (format translation).
*     RV2L: VAX real -> IEEE little-endian real (format translation).
*     RV2V: VAX real -> VAX real (copy).
*
*   The routines take two BYTE(4) dummy arguments, the first being the
*   given value-to-convert and the second being the returned, converted
*   value.  They may be called with INTEGER or REAL actual arguments in
*   place of the BYTE(4) dummy arguments, and the arguments may have the
*   same address, e.g. CALL IV2B (B, B) is allowed.
*
*   Original: 1997/08/20 MRC
*   $Id: dconv.f,v 1.6 2007/05/04 02:27:54 cal103 Exp $
*=======================================================================
*   Two's complement integer representation
*-----------------------------------------------------------------------
*
*   The one's complement of a bit pattern is formed by interchanging
*   zeros and ones.  For example:
*
*           Binary pattern: 00110010
*         One's complement: 11001101
*
*   In two's complement arithmetic, negative integers are represented
*   as the one's complement plus 1.  For a one-byte signed integer
*
*                Binary  1: 00000001
*      Two's complement -1: 11111111
*
*   When a binary integer is added to its two's complement the result
*   is all zeros (binary 0) with the carry bit set (ignored).
*
*   Note the special case:
*
*           Binary pattern: 10000000
*         Two's complement: 10000000
*
*   This is taken to be the least negative integer, so the range for
*   one-byte signed integers is -128 to +127.  Similarly for multi-byte
*   integers.
*
*   All modern computers (including VAXs) use two's complement integer
*   representation.  For four-byte integers
*
*      byte:    0        1        2        3
*            siiiiiii iiiiiiii iiiiiiii iiiiiiii
*            ^                                 ^
*      bit: 31                                 0
*
*   These byte and bit numbers DO NOT reflect the order in memory, see
*   below.
*
*   Bit 31 is the sign bit and significance increases with increasing
*   bit number.
*
*   The "storage format" is commonly "big-endian" or "little-endian":
*
*                       Byte addresses in memory
*   Storage format     b      b+1     b+2     b+3
*   --------------    -----------------------------
*      Big-endian      0       1       2       3
*   Little-endian      3       2       1       0
*
*   Where b is the byte address in memory (b precedes b+1).  DEC (VAX,
*   Alpha) and INTEL machines (PCs) are little-endian while most others
*   are big-endian.
*
*=======================================================================
*   IEEE 754-1985 single-precision floating representation
*-----------------------------------------------------------------------
*
*   The bit-pattern of an IEEE 754-1985 single-precision floating point
*   number is
*
*      byte:    0        1        2        3
*            seeeeeee efffffff ffffffff ffffffff
*            ^                                 ^
*      bit: 31                                 0
*
*   These byte and bit numbers DO NOT reflect the order in memory, see
*   below.
*
*      bit field  (significance increases with increasing bit number)
*      ---------
*           31     sign bit ( 1 bit)
*        23-30     exponent ( 8 bits)
*         0-22     fraction (23 bits)
*
*   The IEEE single-precision representation is summarized as follows:
*
*      seeeeeee efffffff ffffffff ffffffff
*      -----------------------------------
*      00000000 00000000 00000000 00000000    +0
*      10000000 00000000 00000000 00000000    -0
*          0 < e < 255                        Normalized number
*          e = 0, f != 0                      Unnormalized number
*      01111111 10000000 00000000 00000000    +Infinity
*      11111111 10000000 00000000 00000000    -Infinity
*      s1111111 11ffffff ffffffff ffffffff    Quiet NaN
*      s1111111 10ffffff ffffffff ffffffff    Signalling NaN (f != 0)
*
*   The value of a normalized number is 
*
*      (-1)**s * 2**(e-127) * 1.f
*
*   where the implicit fraction bit occurs to the left of the binary
*   radix point.
*
*   The value of an unnormalized number is 
*
*      (-1)**s * 2**(-126) * 0.f
*
*   Unnormalized numbers allow for "progressive underflow".
*
*   The IEEE 754-1985 standard explicitly does not specify a "storage
*   format", byte ordering is commonly "big-endian" or "little-endian":
*
*                       Byte addresses in memory
*   Storage format     b      b+1     b+2     b+3
*   --------------    -----------------------------
*      Big-endian      0       1       2       3
*   Little-endian      3       2       1       0
*
*   Where b is the byte address in memory (b precedes b+1).  DEC Alphas,
*   and INTEL machines (PCs) are commonly little-endian, while most
*   other IEEE 754 machines are big-endian.
*
*=======================================================================
*   Integer representations of IEEE floating point infinities and NaNs
*-----------------------------------------------------------------------
*
*   Assuming consistent endianness of integer and single-precision
*   representations the following relations may be useful:
*
*   -Infinity:
*     11111111 10000000 00000000 00000000 =   -8388608
*
*   Negative range of signalling NaNs:
*     11111111 10000000 00000000 00000001 =   -8388607
*         :
*     11111111 10111111 11111111 11111111 =   -4194305
*
*   Negative range of quiet NaNs:
*     11111111 11000000 00000000 00000000 =   -4194304
*         :
*     11111111 11111111 11111111 11111111 =         -1
*
*   +Infinity:
*     01111111 10000000 00000000 00000000 = 2139095040
*
*   Positive range of signalling NaNs:
*     01111111 10000000 00000000 00000001 = 2139095041
*         :
*     01111111 10111111 11111111 11111111 = 2143289343
*
*   Positive range of quiet NaNs:
*     01111111 11000000 00000000 00000000 = 2143289344
*         :
*     01111111 11111111 11111111 11111111 = 2147483647
*
*   Thus the integer representation of IEEE floating point infinities
*   and NaNs ranges from -8388608 to -1, and 2139095040 to 2147483647
*   inclusive.
*
*=======================================================================
*   IEEE 754-1985 double-precision floating representation
*-----------------------------------------------------------------------
*
*   The bit-pattern of an IEEE 754-1985 double-precision floating point
*   number is
*
*      byte:    0        1        2        3        4     ...    7
*            seeeeeee eeeeffff ffffffff ffffffff ffffffff ... ffffffff
*            ^                                                       ^
*      bit: 63                                                       0
*
*      bit field  (significance increases with increasing bit number)
*      ---------
*           63     sign bit ( 1 bit)
*        52-62     exponent (11 bits)
*         0-51     fraction (52 bits)
*
*   The representation of infinities and NaNs follows the pattern
*   established for single-precision numbers.
*
*   The value of a normalized number is 
*
*      (-1)**s * 2**(e-1023) * 1.f
*
*   where the implicit fraction bit occurs to the left of the binary
*   radix point.
*
*   The value of an unnormalized number is 
*
*      (-1)**s * 2**(-1022) * 0.f
*
*=======================================================================
*   VAX F_floating single-precision floating representation
*-----------------------------------------------------------------------
*
*   VAX F_floating format does not follow the IEEE 754 standard.  The
*   bit pattern is
*
*      byte:    0        1        2        3
*            seeeeeee efffffff ffffffff ffffffff
*            ^                                 ^
*      bit: 31                                 0
*
*   These byte and bit numbers DO NOT reflect the order in memory, see
*   below.
*
*      bit field  (significance increases with increasing bit number)
*      ---------
*           31     sign bit
*        23-30     exponent
*         0-22     fraction
*
*   The VAX F_floating representation is summarized as follows:
*
*      seeeeeee efffffff ffffffff ffffffff
*      -----------------------------------
*      00000000 0fffffff ffffffff ffffffff    0 (unsigned)
*      10000000 0fffffff ffffffff ffffffff    reserved operand
*          0 < e <= 255                       Normal number
*
*   The value of a normal number is
*
*      (-1)**s * 2**(e-128) * 0.1f
*
*   where the implicit fraction bit occurs to the right of the binary
*   radix point.
*
*                       Byte addresses in memory
*   Storage format     b      b+1     b+2     b+3
*   --------------    -----------------------------
*   VAX F_floating     1       0       3       2
*
*   The address of a VAX F_floating datum is the address, b, of byte 1.
*
*-----------------------------------------------------------------------
*
*   Apart from byte ordering, normal IEEE 754 numbers differ from VAX
*   F_floating numbers by a factor of 4 because:
*
*      * IEEE exponents are excess 127 whereas VAX exponents are excess
*        128.
*
*      * The IEEE implicit fraction bit occurs to the left of the binary
*        radix point wheras the VAX implicit bit occurs to the right.
*
*   The multiplication or division by 4 can be accomplished by adding
*   or subtracting 1 from byte 0.
*
*=======================================================================



      SUBROUTINE BCOPY (B1, B2)
*-----------------------------------------------------------------------
*  Does byte copying 0123 <--> 0123.
*-----------------------------------------------------------------------
      BYTE      B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Copy big-endian integer --> big-endian integer.
      ENTRY IB2B (B1, B2)

*     Copy little-endian integer --> little-endian integer.
      ENTRY IL2L (B1, B2)

*     Copy VAX integer --> VAX integer.
      ENTRY IV2V (B1, B2)
*-----------------------------------------------------------------------
*     Copy VAX integer --> little-endian integer.
      ENTRY IV2L (B1, B2)

*     Copy little-endian integer --> VAX integer.
      ENTRY IL2V (B1, B2)
*-----------------------------------------------------------------------
*     Copy IEEE big-endian real --> IEEE big-endian real.
      ENTRY RB2B (B1, B2)

*     Copy IEEE little-endian real --> IEEE little-endian real.
      ENTRY RL2L (B1, B2)

*     Copy VAX real --> VAX real.
      ENTRY RV2V (B1, B2)
*-----------------------------------------------------------------------
      B2(0) = B1(0)
      B2(1) = B1(1)
      B2(2) = B1(2)
      B2(3) = B1(3)
 
      RETURN
      END



      SUBROUTINE BSWAP (B1, B2)
*-----------------------------------------------------------------------
*   Does byte swapping 0123 <--> 3210.
*-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Convert VAX integer --> big-endian integer.
      ENTRY IV2B (B1, B2)

*     Convert big-endian integer --> VAX integer.
      ENTRY IB2V (B1, B2)
*-----------------------------------------------------------------------
*     Convert little-endian integer --> big-endian integer.
      ENTRY IL2B (B1, B2)

*     Convert big-endian integer --> little-endian integer.
      ENTRY IB2L (B1, B2)
*-----------------------------------------------------------------------
*     Convert IEEE little-endian real --> IEEE big-endian real.
      ENTRY RL2B (B1, B2)

*     Convert IEEE big-endian real --> IEEE little-endian real.
      ENTRY RB2L (B1, B2)
*-----------------------------------------------------------------------
      B(0) = B1(3)
      B(1) = B1(2)
      B(2) = B1(1)
      B(3) = B1(0)

      B2(0) = B(0)
      B2(1) = B(1)
      B2(2) = B(2)
      B2(3) = B(3)

      RETURN
      END



      SUBROUTINE RV2B (B1, B2)
*-----------------------------------------------------------------------
*     Converts VAX real --> IEEE big-endian real.
*-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Natural order.
      B(0) = B1(1)
      B(1) = B1(0)
      B(2) = B1(3)
      B(3) = B1(2)

      IF (B(0).EQ.-128 .AND. B(1).GE.0) THEN
*        VAX reserved operand, set to NaN.
         B(0) = -1
         B(1) = -1
         B(2) =  0
         B(3) =  0

      ELSE IF (B(1).LT.0 .AND. (B(0).EQ.127 .OR. B(0).EQ.-1)) THEN
*        Overflow, set to signed infinity.
*        B(0) = B(0)
         B(1) = -128
         B(2) =  0
         B(3) =  0

      ELSE IF (B(0).EQ.0 .AND. B(1).GE.0) THEN
*        VAX zero.
         B(0) = 0
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE
*        Scale VAX to IEEE.
         B(0) = B(0) - 1
      END IF

*     Big-endian ordering.
      B2(0) = B(0)
      B2(1) = B(1)
      B2(2) = B(2)
      B2(3) = B(3)

      RETURN
      END



      SUBROUTINE RB2V (B1, B2)
*-----------------------------------------------------------------------
*     Converts IEEE big-endian real --> VAX real.
*-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Natural ordering.
      B(0) = B1(0)
      B(1) = B1(1)
      B(2) = B1(2)
      B(3) = B1(3)

      IF (B(1).LT.0 .AND. (B(0).EQ.127 .OR. B(0).EQ.-1)) THEN
*        Convert IEEE NaN or Infinity to VAX reserved operand.
         B(0) = -128
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE IF (B(0).EQ.-128 .AND. B(1).GE.0) THEN
*        Convert IEEE negative zero or unnormalized number to VAX zero.
         B(0) = 0
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE
*        Scale IEEE to VAX.
         B(0) = B(0) + 1
      END IF

*     VAX ordering.
      B2(0) = B(1)
      B2(1) = B(0)
      B2(2) = B(3)
      B2(3) = B(2)

      RETURN
      END



      SUBROUTINE RV2L (B1, B2)
*-----------------------------------------------------------------------
*     Converts VAX real --> IEEE little-endian real.
*-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Natural order.
      B(0) = B1(1)
      B(1) = B1(0)
      B(2) = B1(3)
      B(3) = B1(2)

      IF (B(0).EQ.-128 .AND. B(1).GE.0) THEN
*        VAX reserved operand, set to NaN.
         B(0) = -1
         B(1) = -1
         B(2) =  0
         B(3) =  0

      ELSE IF (B(1).LT.0 .AND. (B(0).EQ.127 .OR. B(0).EQ.-1)) THEN
*        Overflow, set to signed infinity.
*        B(0) = B(0)
         B(1) = -128
         B(2) =  0
         B(3) =  0

      ELSE IF (B(0).EQ.0 .AND. B(1).GE.0) THEN
*        VAX zero.
         B(0) = 0
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE
*        Scale VAX to IEEE.
         B(0) = B(0) - 1
      END IF
 
*     Little-endian ordering.
      B2(0) = B(3)
      B2(1) = B(2)
      B2(2) = B(1)
      B2(3) = B(0)

      RETURN
      END



      SUBROUTINE RL2V (B1, B2)
*-----------------------------------------------------------------------
*     Converts IEEE little-endian real --> VAX real.
*-----------------------------------------------------------------------
      BYTE      B(0:3), B1(0:3), B2(0:3)
*-----------------------------------------------------------------------
*     Natural order.
      B(0) = B1(3)
      B(1) = B1(2)
      B(2) = B1(1)
      B(3) = B1(0)

      IF (B(1).LT.0 .AND. (B(0).EQ.127 .OR. B(0).EQ.-1)) THEN
*        Convert IEEE NaN or Infinity to VAX reserved operand.
         B(0) = -128
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE IF (B(0).EQ.-128 .AND. B(1).GE.0) THEN
*        Convert IEEE negative zero or unnormalized number to VAX zero.
         B(0) = 0
         B(1) = 0
         B(2) = 0
         B(3) = 0

      ELSE
*        Scale IEEE to VAX.
         B(0) = B(0) + 1
      END IF

*     VAX ordering.
      B2(0) = B(1)
      B2(1) = B(0)
      B2(2) = B(3)
      B2(3) = B(2)

      RETURN
      END
