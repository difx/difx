/*
 * xcTypes.h
 *
 *  Created on: Apr 22, 2010
 *      Author: jroberts
 *
 *      Standard defines shared across XCube development projects.
 *
 */
#ifndef XCTYPES_H_
#define XCTYPES_H_

//BOOLEAN
#define TRUE  1
#define FALSE 0

//SIGNED
typedef long                LONG;
typedef long long   		INT64;
typedef int   				INT32;
typedef short 				INT16;
typedef char  				INT8;

typedef short 				SHORT;
typedef char  				CHAR;

//UNSIGNED
typedef unsigned long       ULONG;
typedef unsigned long long  UINT64;  //Same as 'long', 64bits
typedef unsigned int        UINT32;
typedef unsigned short      UINT16;
typedef unsigned char       UINT8;

typedef unsigned short      USHORT;
typedef unsigned char       UCHAR;

//Usful Macros
#define ISODD(val) val % 2



#endif /* XCTYPES_H_ */
