/*  @(#)STDDEFS.H  version 1.14  created 06/11/06 10:52:45
                 fetched from SCCS 06/11/06 10:52:51
    LANGUAGE: C
    ENVIRONMENT: Any
*/
	
#ifndef INCstddefsh
#define INCstddefsh      /* this include file has been included */
 
/***************************************************/
/*            various symbolic constants           */
/***************************************************/

#ifndef NULL
#define NULL  0
#endif

#ifndef EOF
#define EOF  (-1)
#endif

#ifndef TRUE
#define TRUE  1
#endif

#ifndef FALSE
#define FALSE  0
#endif

#ifndef STD_IN
#define STD_IN  0
#endif

#ifndef STD_OUT
#define STD_OUT  1
#endif

#ifndef STD_ERR
#define STD_ERR  2
#endif

#ifndef FOREVER
#define FOREVER  for(;;)
#endif

#ifndef NONE
#define NONE  (-1)	/* for times when NULL won't do */
#endif

#ifndef EOS
#define EOS   '\0'	/* C string terminator */
#endif

#ifndef NUL
#define NUL   '\0'      /*  nul character, do not confuse with NULL pointer  */
#endif

#ifndef ERROR
#define ERROR  (-1)
#endif

#ifndef OK
#define OK     0
#endif

#ifndef NELEMENTS
#define NELEMENTS(array)    /* number of elements in an array */ \
              (sizeof (array) / sizeof ((array) [0]))
#endif

/***************************************************/
/*              max and min macros                 */
/***************************************************/

#if     !defined(max)
#define max(x, y)	(((x) < (y)) ? (y) : (x))
#define min(x, y)	(((x) < (y)) ? (x) : (y))
#endif


/***************************************************/
/*                  type definitions               */
/***************************************************/

#ifndef LOCAL
#define FAST	register
#define IMPORT	extern
#define LOCAL	static
typedef int (*FUNCPTR) ();		/* pointer to a function */
typedef char *(*CFUNCPTR) ();    /* ptr to a function returning ptr to char */
typedef char TINY;
typedef char TBOOL;
typedef unsigned char UTINY;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned long ULONG;
typedef int ARGINT, BOOL, VOID, STATUS;
typedef unsigned short INSTR;		/* word-alligned instructions */
#endif


/***************************************************/ 
/*  macros for character testing and manipulation  */
/***************************************************/ 

#ifndef isalpha
#define isalpha(c) (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
#define isdigit(c) ( '0' <= c  &&  c <= '9' )
#define islower(c) ( 'a' <= c  &&  c <= 'z' )
#define isupper(c) ( 'A' <= c  &&  c <= 'Z' )
#define iswhite(c) (c == ' ' || c == '\t' || c == '\n')
#define isalnum(c) (isalpha(c) || isdigit(c))
#endif
#ifndef tolower
#define tolower(c) (isupper(c) ? (c - 'A' + 'a') : c)
#define toupper(c) (islower(c) ? (c - 'a' + 'A') : c)
#endif


/***************************************************/
/*         macros for string manipulation          */
/***************************************************/

#define SKIPWHITE(sptr)  { while (iswhite(*sptr)) sptr++; }
 

/***************************************************/
/* allow gcc to use __attribute__; acc & cc ignore*/
/***************************************************/
#ifndef __GNUC__
#define __attribute__(x)
#endif

#endif /* INCstddefsh */
